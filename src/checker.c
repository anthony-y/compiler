#include "headers/passes.h"
#include "headers/ast.h"
#include "headers/common.h"
#include "headers/context.h"
#include "headers/arena.h"
#include "headers/type.h"

#include <stdio.h>
#include <assert.h>

#include "headers/stb/stretchy_buffer.h"
#include "headers/stb/stb_ds.h"

void check_statement(Context *ctx, AstNode *node);
void check_block(Context *ctx, AstBlock *block, AstNodeType restriction);
Type *type_from_expr(Context *ctx, AstNode *expr);
void check_struct(Context *ctx, AstNode *structdef);
bool check_assignment(Context *ctx, AstBinary *binary);


// TODO consider
    // const values -> type
    // lookup the const value -> type

// Returns true if `first` and `second` point to the same Type, false otherwise.
// Does not print errors.
static bool do_pointer_types_match(Type *first, Type *second) {
    if (first->data.base == second->data.base) {
        return true; // both types point to the same thing
    } else if (!first->data.base || !second->data.base) { // TODO add Context* to this func, to check directly for Context.decoy_ptr, atm this will suffice since decoy_ptr is the only pointer type without a base type.
        return true; // one of them is the decoy pointer type, which can be assigned to any pointer type.
    } else if (first->data.base->kind != second->data.base->kind) {
        return false; // they don't point to the same type
    } else if (first->data.base->kind == Type_POINTER) { // since the previous case failed, this is effectively checking that both types are pointers to pointers.
        return do_pointer_types_match(first->data.base, second->data.base);
    }
    return false;
}

static bool do_types_match(Type *a, Type *b) {
    if (a->kind == Type_ALIAS && b->kind == Type_ALIAS) return (a == b);

    // This allows us to ensure that literals can be compared with aliases,
    // but still be restrictive about alias vs alias matching.
    if (a->kind == Type_ALIAS) {
        a = a->data.alias_of;
        return do_types_match(a, b);
    } else if (b->kind == Type_ALIAS) {
        b = b->data.alias_of;
        return do_types_match(b, a);
    }

    if (a->kind == Type_PRIMITIVE && b->kind == Type_PRIMITIVE && a->data.signage != Signage_NaN && b->data.signage != Signage_NaN) { // both types are integer types
        return (a->data.signage == b->data.signage && a->size == b->size);
    }

    if (a->kind == Type_POINTER && b->kind == Type_POINTER) return do_pointer_types_match(a, b);
    if (a->kind != b->kind) return false;
    if (a != b) return false;

    return true;
}

// Utility function to unwrap a pointer to it's ultimate base type.
// Returns the unwrapped pointer, and return the depth to `out_depth`.
// Does not print errors.
static inline Type *unwrap_pointer_type(Type *ptr, int *out_depth) {
    assert(ptr->kind == Type_POINTER);
    int depth = 0;
    while (ptr->kind == Type_POINTER) {
        ptr = ptr->data.base;
        depth++;
    }
    if (out_depth) *out_depth = depth;
    return ptr;
}

static Type *maybe_unwrap_type_alias(Type *alias) {
    if (alias->kind != Type_ALIAS) return alias;
    return alias->data.alias_of;
}

// Performs semantic analysis on a function call.
// Returns true if the call was semantically correct, otherwise false.
// Writes the return type to `out_return_type` ONLY if it is SUCCEEDS.
// Prints its own errors.
// You may pass `out_return_type` as NULL.
static bool check_call(Context *ctx, AstNode *callnode, Type **out_return_type) {
    AstCall *call = &callnode->as.function_call;

    char *name = call->name->as.ident->text;

    u64 index = shgeti(ctx->symbol_table, name); // TODO when local procedures are implemented, this will need to be a check using lookup_local or something else (maybe a prospective find_proc which is optimized for global scope first, and then local scope).
    if (index == -1) {
        compile_error(ctx, callnode->token, "Attempted to call undeclared procedure \"%s\"", name);
        return false;
    }

    AstNode *symbol = ctx->symbol_table[index].value;
    if (symbol->tag != Node_PROCEDURE) {
        compile_error(ctx, callnode->token, "\"%s\" is not a procedure", name);
        return false;
    }

    AstProcedure *proc = &symbol->as.procedure;
    int proc_arg_count = (proc->params ? proc->params->len : 0);
    if (!call->params) {
        if (proc_arg_count != 0) {
            compile_error(ctx, callnode->token, "Call to \"%s\" specifies no arguments, but it's defined to expect %d of them", name, proc_arg_count);
            return false;
        }
        if (out_return_type) *out_return_type = symbol->as.procedure.return_type->as.type;
        return true;
    }

    int call_arg_count = call->params->len;

    if (call_arg_count > proc_arg_count) {
        int diff = call_arg_count - proc_arg_count;
        compile_error(ctx, callnode->token, "%d too many arguments in call to \"%s\"", diff, name);
        return false;
    }

    if (call_arg_count < proc->params->len) {
        int diff = proc_arg_count - call_arg_count;
        compile_error(ctx, callnode->token, "%d too few arguments in call to \"%s\"", diff, name);
        return false;
    }

    assert(call_arg_count == proc_arg_count);

    for (int i = 0; i < call_arg_count; i++) {
        Type *caller_type = type_from_expr(ctx, call->params->nodes[i]);
        if (caller_type == ctx->error_type) return false;
        Type *defn_type = proc->params->nodes[i]->as.var.typename->as.type;
        if (!do_types_match(caller_type, defn_type)) {
            compile_error_start(ctx, callnode->token, "type mismatch: argument %d of procedure \"%s\" is defined as type ", i+1, name);
            print_type(defn_type, stderr);
            compile_error_add_line(ctx, " but caller provided argument of type ");
            print_type(caller_type, stderr);
            compile_error_end();
        }
    }

    if (out_return_type) *out_return_type = symbol->as.procedure.return_type->as.type;
    return true;
}

// Performs semantic analysis on an identifier.
// Returns true if it points to a declaration, otherwise false.
// However, it may not point to a Node_VAR, callers need to check for this.
// Writes said declaration to `out_decl_site` ONLY if it succeeds.
static bool check_ident(Context *ctx, AstNode *node, AstNode **out_decl_site) {
    Name *name = node->as.ident;
    AstNode *hopefully_var = lookup_local(ctx->curr_checker_proc->block, name); // TODO when lookup_local scoures outer scopes, this will return nodes that could be procedures, etc. which will fix the poor error message I get right now if `name` is actually the name of a procedure.
    if (!hopefully_var) {
        compile_error(ctx, node->token, "Undeclared identifier \"%s\"", name->text);
        return false;
    }
    *out_decl_site = hopefully_var;
    return true;
}

//
// Returns true if `expr` is compatible with `type`.
// Type can be passed as NULL, however do not pass `out_actual_type` as NULL.
// `out_actual_type` is a return variable, in which the actual type of `expr` will be written,
// regardless of whether true or false is returned.
//
bool does_type_describe_expr(Context *ctx, Type *type, AstNode *expr, Type **out_actual_type) {
    if (expr->tag == Node_CALL) {
        Type *return_type = NULL;
        if (!check_call(ctx, expr, &return_type)) {
            *out_actual_type = ctx->error_type;
            return false;
        }
        assert(return_type);
        *out_actual_type = return_type;

        // For return types, aliases must match exactly.
        if (return_type->kind == Type_ALIAS) return (return_type == type);

        // In other words, if the type is an int, allow any other integer type to be compatible with it.
        if (type == ctx->type_int) return (return_type->kind == Type_PRIMITIVE && return_type->data.signage != Signage_NaN);

        return do_types_match(return_type, type);
    }

    if (expr->tag == Node_IDENT) {
        AstNode *var_decl = NULL;
        if (!check_ident(ctx, expr, &var_decl)) {
            *out_actual_type = ctx->error_type;
            return false;
        }
        assert(var_decl);
        if (var_decl->tag != Node_VAR) {
            compile_error(ctx, expr->token, "\"%s\" is not a variable");
            *out_actual_type = ctx->error_type;
            return false;
        }
        return does_type_describe_expr(ctx, type, var_decl->as.var.value, out_actual_type);
    }

    // If the type we're comparing against is an alias, unwrap it to it's 'base' type.
    //
    // We do this before checking literal values because we want to be able to bind them to
    // variables which have alias types, but only where the alias' base type is a builtin.
    //
    if (type->kind == Type_ALIAS) {
        type = type->data.alias_of;
    }

    switch (expr->tag) {
    case Node_BINARY: {
        AstBinary *binary = &expr->as.binary;
        if (is_binary_comparison(*binary)) {
            *out_actual_type = ctx->type_bool;
            return (type == ctx->type_bool);
        }
        if (is_assignment(*binary)) {
            return check_assignment(ctx, binary);
        }
    } break;

    case Node_UNARY: {
        const AstUnary *unary = &expr->as.unary;
        if (unary->op == Token_BANG) {
            *out_actual_type = ctx->type_bool;
            return (type == ctx->type_bool);
        }
        // TODO dereference
        if (unary->op == Token_CARAT) { // address-of operator
            Type *expr_type = NULL;

            int addressof_depth = 1; // we'll use this a bit later on to create the real type of the expression.
            while (unary->expr->tag == Node_UNARY && unary->expr->as.unary.op == Token_CARAT) {
                unary = &unary->expr->as.unary;
                addressof_depth++;
            }

            // It's a literal, you can't get a pointer a literal
            if (unary->expr->tag > Node_LITERALS_START && unary->expr->tag < Node_LITERALS_END) {
                *out_actual_type = ctx->error_type;
                compile_error(ctx, expr->token, "Cannot get the address of a literal value");
                return false;
            }

            else if (unary->expr->tag == Node_CALL) {
                if (!check_call(ctx, unary->expr, &expr_type)) {
                    *out_actual_type = ctx->error_type;
                    return false;
                }
            }

            else if (unary->expr->tag == Node_IDENT) {
                AstNode *decl = NULL;
                char *name = unary->expr->as.ident->text;
                if (!check_ident(ctx, unary->expr, &decl)) {
                    *out_actual_type = ctx->error_type;
                    return false;
                }
                if (decl->tag != Node_VAR) {
                    *out_actual_type = ctx->error_type;
                    compile_error(ctx, expr->token, "Cannot get the address of \"%s\", as it is not a variable", name);
                    return false;
                }
                expr_type = decl->as.var.typename->as.type;
            }

            // TODO The following code is independent of what the expression was,
            // it just relies on the expressions resulting type. So, to add checking
            // for function calls, etc. just evaluate them to a type and then do this.

            assert(expr_type);

            // Both of these start out as the same value, but by the end
            // resulting_type will still point to the outer pointer type,
            // and inner_type will point to the inner-most pointer.
            Type *resulting_type = make_pointer_type(NULL);
            Type *inner_type = resulting_type;

            for (int i = 1; i < addressof_depth; i++) { // i starts at 1 because we're already 1 deep
                Type *inner_ptr = make_pointer_type(NULL);
                inner_type->data.base = inner_ptr;
                inner_type = inner_ptr;
            }
            inner_type->data.base = expr_type;
            *out_actual_type = resulting_type;

            if (type->kind != Type_POINTER) return false;

            return do_pointer_types_match(type, resulting_type);
        }
    } break;

    // Literals
    case Node_NIL:
        *out_actual_type = ctx->decoy_ptr;
        return (type->kind == Type_POINTER);
    case Node_INT_LIT:
        *out_actual_type = ctx->type_int;
        return (type->kind == Type_PRIMITIVE && type->data.signage != Signage_NaN);
    case Node_STRING_LIT:
        *out_actual_type = ctx->type_string;
        return (type == ctx->type_string);
    case Node_BOOL_LIT:
        *out_actual_type = ctx->type_bool;
        return (type == ctx->type_bool);
    case Node_ENCLOSED:
        return does_type_describe_expr(ctx, type, expr->as.enclosed.sub_expr, out_actual_type);
    default:
        assert(false);
    }
    return false;
}

// Wrapper around `does_type_describe_expr` which discards the boolean value
// of said function and just returns the actual type of `expr`.
// Callers of this function must check that the type is not ctx->error_type.
Type *type_from_expr(Context *ctx, AstNode *expr) {
    Type *ret = NULL;
    does_type_describe_expr(ctx, ctx->type_void, expr, &ret);
    if (ret == ctx->error_type) return ctx->error_type;
    assert(ret);
    return ret;
}

// Ensure a return statement is compatible with the procedure declaration
// that it resides inside. Uses `ctx->curr_checker_proc` to compare against.
bool check_proc_return_value(Context *ctx, AstNode *retnode) {
    Type *return_type = ctx->curr_checker_proc->return_type->as.type;

    AstReturn *r = &retnode->as.return_;

    if (!r->expr) {
        return_type = maybe_unwrap_type_alias(return_type); // this allows type aliases of 'void' to work here.
        if (return_type != ctx->type_void) { // procedure return type is not void
            compile_error(ctx, retnode->token, "Only procedures declared as void can have value-less return statements");
            return false;
        }
        return true;
    }

    Type *ret_expr_type = NULL;

    // Return value was provided, type was just wrong.
    if (!does_type_describe_expr(ctx, return_type, r->expr, /*out=*/&ret_expr_type)) {
        if (ret_expr_type == ctx->error_type) return false;
        compile_error_start(ctx, retnode->token, "type mismatch: procedure has return type ");
        print_type(return_type, stderr);
        compile_error_add_line(ctx, ", but return value is of type ");
        print_type(ret_expr_type, stderr);
        compile_error_end();

        return false;
    }

    return true;
}

bool check_var(Context *ctx, AstNode *node) {
    AstVar *var = &node->as.var;

    if (var->flags & VAR_IS_INFERRED) return true;

    Type *type = var->typename->as.type;

    if (var->flags & VAR_TYPE_IS_ANON_STRUCT) {
        check_struct(ctx, type->data.user);
    }

    else if (type == ctx->type_void) {
        compile_error(ctx, var->typename->token, "Only procedures may use the \"void\" type");
        return false;
    }

    else if (type->kind == Type_ALIAS && maybe_unwrap_type_alias(type) == ctx->type_void) {
        compile_error_start(ctx, var->typename->token, "Variable is declared to have type \"%s\" ", type->name);
        compile_error_add_line(ctx, "which is an alias of void; only procedures may use the \"void\" type");
        compile_error_end();
        return false;
    }

    if (!(var->flags & VAR_IS_INITED)) return true;

    Type *value_type = NULL;

    if (!does_type_describe_expr(ctx, type, var->value, &value_type)) {
        if (value_type == ctx->error_type) return false;
        compile_error_start(ctx, node->token, "type mismatch: variable declared as type ");
        print_type(type, stderr);
        compile_error_add_line(ctx, ", but value was of type ");
        print_type(value_type, stderr);
        compile_error_end();

        return false;
    }

    return true;
}

void check_if(Context *ctx, AstNode *node) {
    AstIf *iff = &node->as.if_;
    Type *expr_type = type_from_expr(ctx, iff->condition);
    if (expr_type == ctx->error_type) return;
    if (expr_type != ctx->type_bool) {
        compile_error_start(ctx, node->token, "'If' statement requires a condition which evaluates to a boolean, this one evaluates to ");
        print_type(expr_type, stderr);
        compile_error_end();
        return;
    }
    if (iff->block_or_stmt->tag == Node_BLOCK) {
        check_block(ctx, &iff->block_or_stmt->as.block, Node_ZERO);
    } else check_statement(ctx, iff->block_or_stmt);
}

Type *resolve_accessor(Context *ctx, AstBinary *accessor, bool nested) {
    assert(accessor->op == Token_DOT);
    assert(accessor->right->tag == Node_IDENT);
    Name *rhs = accessor->right->as.ident;

    // lookup left in scope
    // ensure its a struct instance (its type is a struct)
    // use lookup_local to find the field corresponding to name on the RHS
    // return the type of the field

    Type *lhs_type = NULL;

    if (accessor->left->tag == Node_IDENT) {
        Name *lhs = accessor->left->as.ident;
        AstNode *lhs_decl = NULL;
        if (!check_ident(ctx, accessor->left, &lhs_decl)) {
            return NULL;
        }
        if (lhs_decl->tag != Node_VAR) {
            compile_error(ctx, accessor->left->token, "Cannot access member in \"%s\" because it's not a variable", lhs->text);
            return NULL;
        }

        lhs_type = lhs_decl->as.var.typename->as.type;
        if (lhs_type->kind != Type_STRUCT) {
            compile_error(ctx, accessor->left->token, "Cannot access member in \"%s\" because it's not a struct instance", lhs->text);
            return NULL;
        }
    }

    else if (accessor->left->tag == Node_BINARY) {
        lhs_type = resolve_accessor(ctx, &accessor->left->as.binary, false);
        if (!lhs_type) return NULL;
    }

    assert(lhs_type);

    AstStruct *struct_def = &lhs_type->data.user->as.struct_;
    AstNode *field = lookup_local(struct_def->members, rhs);
    if (!field) {
        compile_error(ctx, accessor->right->token, "No such field as \"%s\" in struct field access", rhs->text);
        return NULL;
    }
    if (field->tag != Node_VAR) {
        assert(false); // should have been checked by now, ill see if i can make this go off
    }
    return field->as.var.typename->as.type;
}

bool check_assignment(Context *ctx, AstBinary *binary) {
    assert(is_assignment(*binary));

    bool is_math_assign = binary->op != Token_EQUAL; // is it a normal assignment? or *=, etc.
    AstNode *left = binary->left; // the name or member access
    AstNode *right = binary->right; // the value

    assert(right->tag != Node_BINARY);

    Type *left_type  = NULL; // needs to be discovered depending on what left->tag is
    Type *right_type = NULL; // filled in later by does_type_describe_expr

    if (left->tag == Node_IDENT) {
        Name *assigning_to = left->as.ident;
        AstNode *left_decl;
        if (!check_ident(ctx, left, &left_decl)) {
            return false;
        }
        if (left_decl->tag != Node_VAR) {
            compile_error(ctx, left->token, "Cannot assign to \"%s\" as it is not a variable", assigning_to->text);
            return false;
        }
        left_type = left_decl->as.var.typename->as.type;
    }

    else if (left->tag == Node_BINARY) {
        AstBinary *accessor = &left->as.binary;
        assert(accessor->op == Token_DOT); // TODO probs replace with real error
        Type *hopefully_left_type = resolve_accessor(ctx, accessor, /*nested=*/false);
        if (!hopefully_left_type) return false;
        left_type = hopefully_left_type;
    }

    assert(left_type);

    if (!does_type_describe_expr(ctx, left_type, right, &right_type)) {
        compile_error_start(ctx, left->token, "type mismatch: cannot assign value of type ");
        print_type(right_type, stderr);
        compile_error_add_line(ctx, " to variable of type ");
        print_type(left_type, stderr);
        compile_error_end();
        return false;
    }
    return true;
}

void check_block(Context *ctx, AstBlock *block, AstNodeType restriction) {
    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        if (restriction != Node_ZERO && restriction != stmt->tag) {
            compile_error(ctx, stmt->token, "This statement is not allowed at this scope");
            return;
        }
        check_statement(ctx, stmt);
    }
}

// Check a block-level statement.
void check_statement(Context *ctx, AstNode *node) {
    switch (node->tag) {
    case Node_PROCEDURE:
        ctx->curr_checker_proc = (AstProcedure*)node;
        check_block(ctx, (AstBlock *)((AstProcedure *)node)->block, Node_ZERO);
        break;
    case Node_VAR:
        check_var(ctx, node);
        break;
    case Node_IF:
        check_if(ctx, node);
        break;
    case Node_CALL:
        check_call(ctx, node, NULL);
        break;
    case Node_BINARY: {
        AstBinary *bin = &node->as.binary;
        check_assignment(ctx, bin);
    } break;
    case Node_RETURN: {
        check_proc_return_value(ctx, node);
    } break;
    }
}

void check_struct(Context *ctx, AstNode *structdef) {
    assert(structdef->tag == Node_STRUCT);
    AstBlock *fields = &structdef->as.struct_.members->as.block;
    check_block(ctx, fields, Node_VAR);
}

void check_typedef(Context *ctx, AstNode *node) {
    AstTypedef *td = &node->as.typedef_;
    if (td->of->tag == Node_STRUCT) {
        check_struct(ctx, td->of);
        return;
    }
    if (td->of->tag == Node_TYPENAME) {
        if (td->of->as.type->kind == Type_ALIAS) {
            compile_error(ctx, td->of->token, "You cannot create a type alias from another type alias");
        }
        if (td->of->as.type == ctx->type_void) {
            //compile_error(ctx, td->of->token, "You cannot create a type alias from \"void\"");
        }
        return;
    }
    assert(false);
}

// Recursively perform semantic analysis and type-checking on an Ast.
void check_ast(Context *ctx, Ast *ast) {
    if (!ast || !ast->nodes || ast->len == 0) return;

    for (int i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (!node) return;

        // Top levels
        switch (node->tag) {
        default: continue;
        case Node_PROCEDURE:
            ctx->curr_checker_proc = (AstProcedure *)node;
            check_block(ctx, (AstBlock *)((AstProcedure *)node)->block, Node_ZERO);
            break;

        case Node_VAR:
            check_var(ctx, node);
            break;

        case Node_TYPEDEF:
            check_typedef(ctx, node);
            break;
        }
    }
}
