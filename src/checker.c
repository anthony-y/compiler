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

void check_statement  (Context *, AstStmt *);
void check_block      (Context *, AstBlock *, AstNodeType);
Type *type_from_expr  (Context *, AstNode *);
void check_struct     (Context *, AstStruct *);
bool check_assignment (Context *, AstBinary *);

// TODO consider
//   const values -> type
//   lookup the const value -> type

// Returns true if `a` and `b` point to the same Type, false otherwise.
// Does not print errors.
static bool do_pointer_types_match(Context *ctx, Type *a, Type *b) {
    if (a->data.base == ctx->type_void || b->data.base == ctx->type_void) {
        return true; // ^void is compatible with all pointer types
    } else if (a->data.base == b->data.base) {
        return true; // both types point to the same thing
    } else if (a == ctx->decoy_ptr || b == ctx->decoy_ptr) { // TODO add Context* to this func, to check directly for Context.decoy_ptr, atm this will suffice since decoy_ptr is the only pointer type without a base type.
        return true; // one of them is the decoy pointer type, which can be assigned to any pointer type.
    } else if (a->data.base->kind != b->data.base->kind) {
        return false; // they don't point to the same type
    } else if (a->data.base->kind == Type_POINTER) { // since the previous case failed, this is effectively checking that both types are pointers to pointers.
        return do_pointer_types_match(ctx, a->data.base, b->data.base);
    }
    return false;
}

static bool do_types_match(Context *ctx, Type *a, Type *b) {
    if (a->kind == Type_ALIAS && b->kind == Type_ALIAS) return (a == b);

    // This allows us to ensure that literals can be compared with aliases,
    // but still be restrictive about alias vs alias matching.
    if (a->kind == Type_ALIAS) {
        a = a->data.alias_of;
        return do_types_match(ctx, a, b);
    } else if (b->kind == Type_ALIAS) {
        b = b->data.alias_of;
        return do_types_match(ctx, b, a);
    }

    if (a->kind == Type_PRIMITIVE && b->kind == Type_PRIMITIVE && a->data.signage != Signage_NaN && b->data.signage != Signage_NaN) { // both types are integer types
        return (a->data.signage == b->data.signage && a->size == b->size);
    }

    if (a->kind == Type_POINTER && b->kind == Type_POINTER) return do_pointer_types_match(ctx, a, b);
    if (a->kind != b->kind) return false;
    if (a != b) return false;

    return true;
}

static Type *maybe_unwrap_type_alias(Type *alias) {
    if (alias->kind != Type_ALIAS) return alias;
    return alias->data.alias_of;
}

static Type *get_decl_type(AstDecl *node) {
    switch (node->tag) {
    case Decl_VAR: return (Type *)((AstVar *)node)->typename;
    case Decl_PROC: return (Type *)((AstProcedure *)node)->return_type;
    }
    assert(false);
}

// Performs semantic analysis on a function call.
// Returns true if the call was semantically correct, otherwise false.
// Writes the return type to `out_return_type` ONLY if it is SUCCEEDS.
// Prints its own errors.
// You may pass `out_return_type` as NULL.
static bool check_call(Context *ctx, AstNode *callnode) {
    AstCall *call = (AstCall *)callnode;

    char *name = call->name->as.name->text;

    AstProcedure *proc = call->calling;
    int real_len = shlenu(proc->params);
    int proc_arg_count = (proc->params ? real_len : 0);
    if (!call->params) {
        if (proc_arg_count != 0) {
            compile_error(ctx, callnode->token, "Call to \"%s\" specifies no arguments, but it's defined to expect %d of them", name, proc_arg_count);
            return false;
        }
        return true;
    }

    int call_arg_count = call->params->len;

    if (call_arg_count > proc_arg_count) {
        int diff = call_arg_count - proc_arg_count;
        compile_error(ctx, callnode->token, "%d too many arguments in call to \"%s\"", diff, name);
        return false;
    }

    if (call_arg_count < real_len) {
        int diff = proc_arg_count - call_arg_count;
        compile_error(ctx, callnode->token, "%d too few arguments in call to \"%s\"", diff, name);
        return false;
    }

    assert(call_arg_count == proc_arg_count);

    for (int i = 0; i < call_arg_count; i++) {
        Type *caller_type = call->params->nodes[i]->as.expr.resolved_type;
        Type *defn_type = proc->params[i].value->as.var.typename->as.type;
        if (!do_types_match(ctx, caller_type, defn_type)) {
            compile_error_start(ctx, callnode->token, "type mismatch: argument %d of procedure \"%s\" is defined as type ", i+1, name);
            print_type(defn_type, stderr);
            compile_error_add_line(ctx, " but caller provided argument of type ");
            print_type(caller_type, stderr);
            compile_error_end();
        }
    }
    return true;
}

//
// Returns true if `expr` is compatible with `type`.
// Type can be passed as NULL, however do not pass `out_actual_type` as NULL.
// `out_actual_type` is a return variable, in which the actual type of `expr` will be written,
// regardless of whether true or false is returned.
//
bool does_type_describe_expr(Context *ctx, Type *type, AstExpr *expr) {
    switch (expr->tag) {
    case Expr_CALL: {
        check_call(ctx, (AstNode *)expr);
        return do_types_match(ctx, type, expr->resolved_type);
    } break;
    
    default: assert(false);
    }
    return false;
    #if 0
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

        return do_types_match(ctx, return_type, type);
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
        AstVar *var = &var_decl->as.var;
        if (!(var->flags & VAR_IS_INITED)) {
            return do_types_match(ctx, var->typename->as.type, type);
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
    case Node_CAST: {
        AstCast *cast = &expr->as.cast;
        if (type == ctx->type_void) { // we're in type inference, so just allow it
            *out_actual_type = cast->typename->as.type;
            return true;
        }
        // TODO check if the casts expression can even be casted to the requested type
        bool matches = do_types_match(ctx, cast->typename->as.type, type);
        if (matches) {
            *out_actual_type = cast->typename->as.type;
            return true;
        }
        *out_actual_type = ctx->error_type;
        return false;
    } break;

    case Node_BINARY: {
        AstBinary *binary = &expr->as.binary;
        if (is_binary_comparison(*binary)) {
            *out_actual_type = ctx->type_bool;
            return (type == ctx->type_bool);
        }
        if (is_assignment(*binary)) {
            return check_assignment(ctx, binary);
        }
        if (binary->op == Token_DOT) {
            Type *result = resolve_accessor(ctx, binary);
            if (!result) {
                *out_actual_type = ctx->error_type;
                return false;
            }
            *out_actual_type = result;
            return do_types_match(ctx, result, type);
        }
        // All that's left is +, -, / and *
        // TODO
        Type *left_type = type_from_expr(ctx, binary->left);
        Type *right_type = type_from_expr(ctx, binary->right);
        if (left_type->kind != Type_POINTER || (left_type->kind == Type_PRIMITIVE && left_type->data.signage == Signage_NaN)) {
            compile_error(ctx, binary->left->token, "Left hand side of arithmetic expression must be of one of these types: integer, floating point or pointer");
            *out_actual_type = ctx->error_type;
            return false;
        }
        if (right_type->kind != Type_POINTER || (right_type->kind == Type_PRIMITIVE && right_type->data.signage == Signage_NaN)) {
            compile_error(ctx, binary->left->token, "Right hand side of arithmetic expression must be of one of these types: integer, floating point or pointer");
            *out_actual_type = ctx->error_type;
            return false;
        }
        *out_actual_type = ctx->type_int;
        return (type->kind == Type_PRIMITIVE && type->data.signage != Signage_NaN);
    } break;

    case Node_UNARY: {
        const AstUnary *unary = &expr->as.unary;
        if (unary->op == Token_BANG) {
            Type *real_expr_type = type_from_expr(ctx, unary->expr);
            if (real_expr_type == ctx->error_type) {
                return false;
            }
            if (real_expr_type->kind != Type_POINTER && real_expr_type != ctx->type_bool) {
                *out_actual_type = ctx->error_type;
                compile_error(ctx, expr->token, "Unary \"not\" expression must have a boolean or pointer operand");
                return false;
            }
            *out_actual_type = ctx->type_bool;
            return (type == ctx->type_bool);
        }
        if (unary->op == Token_MINUS) { // TODO

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

            return do_pointer_types_match(ctx, type, resulting_type);
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
    #endif
}

// Ensure a return statement is compatible with the procedure declaration
// that it resides inside. Uses `ctx->curr_checker_proc` to compare against.
bool check_proc_return_value(Context *ctx, AstStmt *retnode) {
    Type *return_type = ctx->curr_checker_proc->return_type->as.type;

    Token tok = stmt_tok(retnode);
    AstReturn *r = &retnode->as._return;

    if (!r->expr) {
        return_type = maybe_unwrap_type_alias(return_type); // this allows type aliases of 'void' to work here.
        if (return_type != ctx->type_void) { // procedure return type is not void
            compile_error(ctx, tok, "Only procedures declared as void can have value-less return statements");
            return false;
        }
        return true;
    }

    // Return value was provided, type was just wrong.
    if (!does_type_describe_expr(ctx, return_type, r->expr)) {
        compile_error_start(ctx, tok, "type mismatch: procedure has return type ");
        print_type(return_type, stderr);
        compile_error_add_line(ctx, ", but return value is of type ");
        print_type(r->expr->resolved_type, stderr);
        compile_error_end();

        return false;
    }

    return true;
}

bool check_var(Context *ctx, AstDecl *node) {
    Token   tok = decl_tok(node);
    AstVar *var = &node->as.var;

    if (var->flags & VAR_IS_INFERRED) return true;

    Type *type = var->typename->as.type;

    if (var->flags & VAR_TYPE_IS_ANON_STRUCT) {
        check_struct(ctx, (AstStruct *)type->data.user);
    }

    else if (type == ctx->type_void) {
        compile_error(ctx, tok, "Only procedures may use the \"void\" type");
        return false;
    }

    else if (type->kind == Type_ALIAS && maybe_unwrap_type_alias(type) == ctx->type_void) {
        compile_error_start(ctx, tok, "Variable is declared to have type \"%s\" ", type->name);
        compile_error_add_line(ctx, "which is an alias of void; only procedures may use the \"void\" type");
        compile_error_end();
        return false;
    }

    if (!(var->flags & VAR_IS_INITED)) return true;

    Type *value_type = var->value->resolved_type;
    assert(value_type);

    if (!does_type_describe_expr(ctx, type, var->value)) {
        compile_error_start(ctx, tok, "type mismatch: variable declared as type ");
        print_type(type, stderr);
        compile_error_add_line(ctx, ", but value was of type ");
        print_type(value_type, stderr);
        compile_error_end();

        return false;
    }

    return true;
}

void check_if(Context *ctx, AstStmt *node) {
    AstIf *iff = &node->as._if;
    if (iff->condition->resolved_type != ctx->type_bool) {
        compile_error_start(ctx, stmt_tok(node), "'If' statement requires a condition which evaluates to a boolean, this one evaluates to ");
        print_type(iff->condition->resolved_type, stderr);
        compile_error_end();
        return;
    }
    if (iff->block_or_stmt->tag == Stmt_BLOCK) {
        check_block(ctx, &iff->block_or_stmt->as.block, Node_ZERO);
    } else check_statement(ctx, iff->block_or_stmt);
}

bool check_assignment(Context *ctx, AstBinary *binary) {
    assert(is_assignment(*binary));

    Token tok = ((AstNode *)binary)->token;

    bool is_math_assign = binary->op != Token_EQUAL; // is it a normal assignment? or *=, etc.

    Type *left_type  = binary->left->resolved_type; // needs to be discovered depending on what left->tag is
    Type *right_type = binary->right->resolved_type; // filled in later by does_type_describe_expr

    assert(left_type);
    assert(right_type);

    if (!does_type_describe_expr(ctx, left_type, binary->right)) {
        compile_error_start(ctx, tok, "type mismatch: cannot assign value of type ");
        print_type(right_type, stderr);
        compile_error_add_line(ctx, " to variable of type ");
        print_type(left_type, stderr);
        compile_error_end();
        return false;
    }
    return true;
}

void check_block(Context *ctx, AstBlock *block, AstNodeType restriction) {
    u64 num_decls = shlenu(block->symbols);
    for (int i = 0; i < num_decls; i++) {
        check_var(ctx, block->symbols[i].value); // TODO local procs
    }
    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        if (restriction != Node_ZERO && restriction != stmt->tag) {
            compile_error(ctx, stmt->token, "This statement is not allowed at this scope");
            return;
        }
        check_statement(ctx, (AstStmt *)stmt);
    }
}

// Check a block-level statement.
void check_statement(Context *ctx, AstStmt *node) {
    switch (node->tag) {
    case Stmt_IF:
        check_if(ctx, node);
        break;
    case Stmt_CALL:
        check_call(ctx, (AstNode *)node);
        break;
    case Stmt_ASSIGN: {
        // Guaranteed to be an assignment by parser
        AstBinary *bin = &node->as.binary;
        check_assignment(ctx, bin);
    } break;
    case Stmt_RETURN: {
        //check_proc_return_value(ctx, node);
    } break;
    }
}

void check_struct(Context *ctx, AstStruct *structdef) {
    AstBlock *fields = &structdef->members->as.block;
    check_block(ctx, fields, Node_VAR);
}

void check_typedef(Context *ctx, AstDecl *node) {
    AstTypedef *td = &node->as.typedefi;
    if (td->of->tag == Node_STRUCT) {
        check_struct(ctx, (AstStruct *)td->of);
        return;
    }
    if (td->of->tag == Node_TYPENAME) {
        if (td->of->as.type->kind == Type_ALIAS) {
            compile_error(ctx, td->of->token, "You cannot create a type alias from another type alias");
        }
        // if (td->of->as.type == ctx->type_void) {
        //     compile_error(ctx, td->of->token, "You cannot create a type alias from \"void\"");
        // }
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
            check_var(ctx, (AstDecl *)node);
            break;

        case Node_TYPEDEF:
            check_typedef(ctx, (AstDecl *)node);
            break;
        }
    }
}
