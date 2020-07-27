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

// TODO consider
    // const values -> type
    // lookup the const value -> type

//
// Returns true if `first` and `second` point to the same Type, false otherwise.
//
static bool do_pointer_types_match(Type *first, Type *second) {
    if (first->data.base == second->data.base) {
        return true; // both types point to the same thing
    } else if (first->data.base->kind != second->data.base->kind) {
        return false; // they don't point to the same type
    } else if (first->data.base->kind == Type_POINTER) { // since the initial case failed, this is effectively checking that both types are pointers to pointers.
        return do_pointer_types_match(first->data.base, second->data.base);
    }
    return false;
}

//
// Utility function to unwrap a pointer to it's ultimate base type.
// Returns the unwrapped pointer, and return the depth to `out_depth`.
//
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

//
// Returns true if `expr` is compatible with `type`.
// Type can be passed as NULL, however do not pass `out_actual_type` as NULL.
// `out_actual_type` is a return variable, in which the actual type of `expr` will be written,
// regardless of whether true or false is returned.
//
bool does_type_describe_expr(Context *ctx, Type *type, AstNode *expr, Type **out_actual_type) {
    if (expr->tag == Node_CALL) {
        const AstCall *call = &expr->as.function_call;
        char *name = call->name->as.ident.name;
        u64 index = shgeti(ctx->symbol_table, name);
        if (index == -1) {
            assert(false); // todo
            return false;
        }

        Symbol *symbol = &ctx->symbol_table[index].value;
        if (symbol->decl->tag != Node_PROCEDURE) {
            *out_actual_type = ctx->error_type;
            compile_error(ctx, expr->token, "\"%s\" is not a procedure", name);
            return false;
        }

        Type *return_type = symbol->decl->as.procedure.return_type->as.type;
        *out_actual_type = return_type;

        if (return_type->kind == Type_POINTER && type->kind == Type_POINTER) {
            return do_pointer_types_match(type, return_type);
        }

        if (return_type->kind == Type_ALIAS) {
            return (return_type == type);
        }

        return (type == return_type);
    }

    if (expr->tag == Node_IDENT) {
        char *name = ((AstIdent*)expr)->name;
        AstNode *hopefully_var = lookup_local(ctx->curr_checker_proc->block, name);
        if (hopefully_var->tag != Node_VAR) {
            *out_actual_type = ctx->error_type;
            compile_error(ctx, expr->token, "\"%s\" is not a variable", name);
            return false;
        }
        AstVar *asvar = &hopefully_var->as.var;
        return does_type_describe_expr(ctx, type, asvar->value, out_actual_type);
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
    case Node_UNARY: {
        const AstUnary *unary = &expr->as.unary;
        if (unary->op == Token_BANG) {
            *out_actual_type = ctx->type_bool;
            return (type == ctx->type_bool);
        }
        if (unary->op == Token_CARAT) { // address-of operator
            AstIdent *the_ident = (unary->expr->tag == Node_IDENT ? &unary->expr->as.ident : NULL);
            Type *expr_type = NULL;

            int addressof_depth = 1;
            while (unary->expr->tag == Node_UNARY && unary->expr->as.unary.op == Token_CARAT) {
                unary = &unary->expr->as.unary;
                if (unary->expr->tag == Node_IDENT) {
                    the_ident = (AstIdent *)unary->expr;
                }
                addressof_depth++;
            }

            assert(the_ident);

            // It's a literal, you can't get a pointer a literal
            if (unary->expr->tag > Node_LITERALS_START && unary->expr->tag < Node_LITERALS_END) {
                *out_actual_type = ctx->error_type;
                compile_error(ctx, expr->token, "Cannot get the address of a literal value");
                return false;
            }

            if (unary->expr->tag == Node_CALL) {
                compile_error(ctx, expr->token, "EXPRESSION PARSING IS NOW MORE FIXED");
                return true;
            }

            assert(unary->expr->tag == Node_IDENT); // TODO more

            if (unary->expr->tag == Node_IDENT) {
                char *name = the_ident->name;
                AstNode *decl = lookup_local(ctx->curr_checker_proc->block, name);

                if (!decl) {
                    *out_actual_type = ctx->error_type;
                    compile_error(ctx, expr->token, "Undeclared variable \"%s\"", name);
                    return false;
                }

                if (decl->tag != Node_VAR) {
                    *out_actual_type = ctx->error_type;
                    compile_error(ctx, expr->token, "Cannot get the address of \"%s\", as it is not a variable or procedure call", name); // TODO bad error message
                    return false;
                }

                expr_type = decl->as.var.typename->as.type;
            }

            // TODO The following code is independent of what the expression was,
            // it just relies on the expressions resulting type. So, to add checking
            // for function calls, etc. just evaluate them to a type and then do this.
            assert(expr_type);
            Type *resulting_type = make_type(Type_POINTER, "", sizeof(void *));
            Type *inner_type = resulting_type;
            for (int i = 1; i < addressof_depth; i++) { // i starts at 1 because we're already 1 deep
                Type *inner_ptr = make_type(Type_POINTER, "", sizeof(void *));
                inner_type->data.base = inner_ptr;
                inner_type = inner_ptr;
            }
            inner_type->data.base = expr_type;
            *out_actual_type = resulting_type;

            if (type->kind != Type_POINTER) return false;

            return do_pointer_types_match(type, resulting_type);
        }
    } break;
    case Node_BINARY: {
        const AstBinary *binary = &expr->as.binary;
        if (is_assignment(*binary)) {
            // TODO scope lol
        }
        if (is_binary_comparison(*binary)) {
            *out_actual_type = ctx->type_bool;
            return (type == ctx->type_bool);
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
Type *type_from_expr(Context *ctx, AstNode *expr) {
    Type *ret = NULL;
    does_type_describe_expr(ctx, ctx->type_void, expr, &ret);
    assert(ret);
    return ret;
}

// Ensure a return statement is compatible with the procedure declaration
// that it resides inside. Uses `ctx->curr_checker_proc` to compare against.
bool check_proc_return_value(Context *ctx, AstNode *retnode) {
    Type *return_type = ctx->curr_checker_proc->return_type->as.type;

    AstReturn *r = &retnode->as.return_;

    if (!r->expr) {
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

    if (var->flags & VAR_IS_INFERRED || (!(var->flags & VAR_IS_INITED)))
        return true;

    Type *type = var->typename->as.type;
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
        check_block(ctx, (AstBlock *)((AstProcedure*)node)->block, Node_ZERO);
        break;
    case Node_VAR:
        check_var(ctx, node);
        break;
    case Node_IF:
        check_if(ctx, node);
        break;
    case Node_RETURN: {
        check_proc_return_value(ctx, node);
    } break;
    }
}

void check_typedef(Context *ctx, AstNode *node) {
    AstTypedef *td = &node->as.typedef_;
    if (td->of->tag == Node_STRUCT) {
        AstBlock *fields = &td->of->as.struct_.members->as.block;
        check_block(ctx, fields, Node_VAR);
        return;
    }
    if (td->of->tag == Node_TYPENAME) {
        if (td->of->as.type->kind == Type_ALIAS) {
            compile_error(ctx, node->token, "You cannot create a type alias from another type alias");
            return;
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
            ctx->curr_checker_proc = (AstProcedure*)node;
            check_block(ctx, (AstBlock *)((AstProcedure*)node)->block, Node_ZERO);
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
