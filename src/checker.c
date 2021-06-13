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

#define FAILED_BUT_DONT_ERROR_AT_CALL_SITE true

static AstProcedure *curr_checker_proc;

void check_statement  (Context *, AstStmt *);
void check_block      (Context *, AstBlock *, AstNodeType);
void check_struct     (Context *, AstStruct *);
bool check_assignment(Context *ctx, AstExpr *expr, bool lhs_must_be_pointer);
bool does_type_describe_expr(Context *ctx, Type *type, AstExpr *expr);

// Returns true if `a` and `b` point to the same Type, false otherwise.
// Does not print errors.
static bool do_pointer_types_match(Context *ctx, Type *a, Type *b) {
    if (a->data.base == ctx->type_void || b->data.base == ctx->type_void) {
        return true; // ^void is compatible with all pointer types
    } else if (a->data.base == b->data.base) {
        return true; // both types point to the same thing
    } else if (a->data.base->kind != b->data.base->kind) {
        return false; // they don't point to the same type
    } else if (a->data.base->kind == Type_POINTER) { // since the previous case failed, this is effectively checking that both types are pointers to pointers.
        return do_pointer_types_match(ctx, a->data.base, b->data.base);
    }
    return false;
}

static bool do_types_match(Context *ctx, Type *a, Type *b) {
    if (a == ctx->type_any || b == ctx->type_any) return true;

    if (a->kind == Type_ANON_STRUCT || b->kind == Type_ANON_STRUCT) return false;
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

    if (is_type_numeric(a) && is_type_numeric(b)) { // both types are integer types
        bool same_specific_type = (a->data.signage == b->data.signage && a->size == b->size);
        return (a == ctx->type_int || b == ctx->type_int) || same_specific_type;
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

// Performs type checking on a call and ensures it's arguments are correct.
// Prints it's own errors.
static bool check_call(Context *ctx, AstNode *callnode) {
    AstCall *call = (AstCall *)callnode;
    char *name = call->name->as.name->text;
    AstProcedure *proc = call->calling;

    int proc_arg_count = (proc->params ? proc->params->len : 0);
    if (!call->params) {
        if (proc_arg_count != 0) {
            if (proc->var_args) {
                compile_error(ctx, callnode->token, "call to \"%s\" specifies no arguments, but it's defined to expect at least %d of them", name, proc_arg_count);
                return false;
            } else {
                compile_error(ctx, callnode->token, "call to \"%s\" specifies no arguments, but it's defined to expect %d of them", name, proc_arg_count);
                return false;
            }
        }
        return true;
    }

    int call_arg_count = call->params->len;

    if (call_arg_count > proc_arg_count && !proc->var_args) {
        int diff = call_arg_count - proc_arg_count;
        compile_error(ctx, callnode->token, "%d too many arguments in call to \"%s\"", diff, name);
        return false;
    }

    if (call_arg_count < proc_arg_count) {
        int diff = proc_arg_count - call_arg_count;
        compile_error(ctx, callnode->token, "%d too few arguments in call to \"%s\"", diff, name);
        return false;
    }

    int i; // save this for later so we know when the optional arguments begin
    for (i = 0; i < proc_arg_count; i++) {
        Type *caller_type = call->params->nodes[i]->as.expr.resolved_type;
        Type *defn_type = proc->params->nodes[i]->as.decl.as.var.typename->as.type;
        if (!do_types_match(ctx, caller_type, defn_type)) {
            compile_error_start(ctx, callnode->token, "type mismatch: argument %d of procedure \"%s\" is defined as type ", i+1, name);
            print_type(defn_type);
            compile_error_add_line(ctx, " but caller provided argument of type ");
            print_type(caller_type);
            compile_error_end();
            return true;
        }
    }

    if (!proc->var_args) return true;

    if (call_arg_count == proc_arg_count) return true;

    // Loop from the end of the non-var-args arguments
    Type *var_args_type = proc->var_args->typename->as.type->data.base;
    for (int j = 0; j < call->params->len+i; j++) {
        Type *caller_type = call->params->nodes[i]->as.expr.resolved_type;
        if (!do_types_match(ctx, caller_type, var_args_type)) {
            compile_error_start(ctx, callnode->token, "type mismatch: argument %d of procedure \"%s\" is defined as type ", i+1, name);
            print_type(var_args_type);
            compile_error_add_line(ctx, " but caller provided argument of type ");
            print_type(caller_type);
            compile_error_end();
            return true;
        }
    }

    return true;
}

static bool check_unary_against_type(Context *ctx, AstUnary *unary, Type *against, Token expr_token) {
    Type *sub_expr_type = unary->expr->resolved_type;

    if (unary->op == Token_BANG) {
        if (sub_expr_type->kind != Type_POINTER && sub_expr_type != ctx->type_bool) {
            compile_error(ctx, expr_token, "unary \"not\" expression must have a boolean or pointer operand");
            return false;
        }
        return (against == ctx->type_bool || against->kind == Type_POINTER);
    }

    if (unary->op == Token_MINUS) {
        if (!is_type_numeric(unary->expr->resolved_type)) {
            compile_error_start(ctx, expr_token, "unary negating expression expects integer or float operand, given ");
            print_type(unary->expr->resolved_type);
            compile_error_end();
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
        }
        return true;
    }

    if (unary->op == Token_STAR) { // dereference
        if (unary->expr->tag == Expr_BINARY) {
            check_assignment(ctx, unary->expr, true);
        }
        if (sub_expr_type->kind != Type_POINTER) {
            compile_error_start(ctx, expr_token, "pointer dereference expects a pointer operand, but was given type ");
            print_type(sub_expr_type);
            compile_error_end();
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
        }
        return do_types_match(ctx, against, sub_expr_type->data.base);
    }

    if (unary->op == Token_CARAT) { // address-of operator
        if (against->kind != Type_POINTER) return false;

        int addressof_depth = 1; // we'll use this a bit later on to create the real type of the expression.
        while (unary->expr->tag == Expr_UNARY && unary->expr->as.unary.op == Token_CARAT) {
            unary = &unary->expr->as.unary;
            addressof_depth++;
        }

        sub_expr_type = unary->expr->resolved_type;

        // It's a literal, you can't get a pointer to a literal
        if (unary->expr->tag > Expr_LITERALS_START && unary->expr->tag < Expr_LITERALS_END) {
            compile_error(ctx, expr_token, "cannot get the address of a literal value");
            return false;
        }

        else if (unary->expr->tag == Expr_CALL) {
            if (!check_call(ctx, (AstNode *)unary->expr)) {
                return false;
            }
        }

        assert(sub_expr_type);

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
        inner_type->data.base = sub_expr_type;

        return do_pointer_types_match(ctx, against, resulting_type);
    }
    return false;
}

static bool can_type_cast_to_type(Context *ctx, AstExpr *site, Type *from, Type *to) {
    if (do_types_match(ctx, to, from)) {
        compile_warning(ctx, expr_tok(site), "unnecessary cast");
        return true;
    }
    if (from->data.signage != Signage_NaN) {
        return (to != ctx->type_string);
    }
    return true;
}

// Returns true if `expr` is compatible with `type`.
bool does_type_describe_expr(Context *ctx, Type *type, AstExpr *expr) {
    Token expr_token = expr_tok(expr);

    // We want to check against the element type.
    if (type->kind == Type_ARRAY) {
        type = type->data.base;
    }

    if (expr->tag == Expr_CALL) {
        if (!check_call(ctx, (AstNode *)expr))
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;

        Type *return_type = expr->resolved_type;

        // For return types, aliases must match exactly.
        if (return_type->kind == Type_ALIAS) {
            return (return_type == type);
        }

        // In other words, if the type is an int, allow any other integer type to be compatible with it.
        if (type == ctx->type_int) return is_type_numeric(return_type);

        return do_types_match(ctx, type, return_type);
    }

    type = maybe_unwrap_type_alias(type); // do this after checking calls since calls are more strict about aliases

    switch (expr->tag) {
    case Expr_BINARY: {
        AstBinary *binary = &expr->as.binary;
        if (is_binary_comparison(*binary)) {
            return (type == ctx->type_bool);
        }
        if (is_assignment(*binary)) {
            return check_assignment(ctx, expr, false);
        }
        if (binary->op == Token_DOT) {
            return do_types_match(ctx, expr->resolved_type, type);
        }

        Type *left_type = binary->left->resolved_type;
        Type *right_type = binary->right->resolved_type;
        if (!is_type_numeric(left_type) && !is_type_numeric(right_type) && left_type->kind != Type_POINTER && right_type->kind != Type_POINTER) {
            compile_error(ctx, expr_token, "operands of arithmetic expressions need to be numeric");
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
        }
        return is_type_numeric(type) || type->kind == Type_POINTER;
    } break;
    case Expr_UNARY: {
        return check_unary_against_type(ctx, (AstUnary *)expr, type, expr_token);
    } break;
    case Expr_CAST: {
        AstCast *cast = &expr->as.cast;
        Type *as_type = cast->typename->as.type;
        if (!can_type_cast_to_type(ctx, expr, cast->expr->resolved_type, as_type)) {
            compile_error_start(ctx, expr_token, "cannot cast value of type ");
            print_type(cast->expr->resolved_type);
            compile_error_add_line(ctx, " to type ");
            print_type(as_type);
            compile_error_end();
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
        }
        return do_types_match(ctx, as_type, type);
    } break;
    case Expr_INDEX: {
        Type *array_type = ((AstArrayIndex *)expr)->name->resolved_type;
        if (array_type->kind != Type_ARRAY) {
            compile_error(ctx, expr_token, "attempt to index \"%s\" like it's an array, but it isn't");
            return false;
        }
        return do_types_match(ctx, type, expr->resolved_type);
    } break;

    case Expr_PAREN: {
        return does_type_describe_expr(ctx, type, ((AstParen *)expr)->sub_expr);
    } break;
    case Expr_NAME: {
        return do_types_match(ctx, type, expr->resolved_type);
    } break;

    case Expr_IMPORT:
        return (type == ctx->import_type);
    case Expr_NULL:
        return (type->kind == Type_POINTER);
    case Expr_INT:
        return is_type_numeric(type);
    case Expr_FLOAT:
        return (type->kind == Type_PRIMITIVE && (type->data.signage != Signage_UNSIGNED && type->data.signage != Signage_NaN));
    case Expr_STRING:
        return (type == ctx->type_string);
    case Expr_BOOL:
        return (type == ctx->type_bool);

    default: assert(false);
    }
    return false;
}

// Ensure a return statement is compatible with the procedure declaration
// that it resides inside. Uses `curr_checker_proc;` to compare against.
bool check_proc_return_value(Context *ctx, AstStmt *retnode) {
    Type *return_type = curr_checker_proc->return_type->as.type;

    Token tok = stmt_tok(retnode);
    AstReturn *r = &retnode->as._return;

    if (!r->expr) {
        return_type = maybe_unwrap_type_alias(return_type); // this allows type aliases of 'void' to work here.
        if (return_type != ctx->type_void) { // procedure return type is not void
            compile_error(ctx, tok, "only procedures declared as void can have value-less return statements");
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
        }
        return true;
    }

    // Return value was provided, type was just wrong.
    if (!does_type_describe_expr(ctx, return_type, r->expr)) {
        compile_error_start(ctx, tok, "type mismatch: procedure has return type ");
        print_type(return_type);
        compile_error_add_line(ctx, ", but return value is of type ");
        print_type(r->expr->resolved_type);
        compile_error_end();

        return false;
    }

    return true;
}

bool check_var(Context *ctx, AstDecl *node) {
    Token tok = decl_tok(node);
    AstVar *var = &node->as.var;

    if (var->flags & VAR_IS_INFERRED) {
        Type *unwrapped = maybe_unwrap_type_alias(var->value->resolved_type);
        if (var->value->tag == Expr_CALL && unwrapped == ctx->type_void) {
            compile_error(ctx, tok, "cannot assign void value (in the form of call to \"%s\") to variable \"%s\"", var->value->as.call.name->as.name->text, node->name->text);
            return false;
        }
    } else if ((var->flags & VAR_IS_INITED) && var->value->tag == Expr_IMPORT) {
        compile_error(ctx, tok, "type of import declaration must be inferred");
        return NULL;
    }

    Type *type = var->typename->as.type;

    if (type->kind == Type_ANON_STRUCT) {
        AstStruct *s = &type->data.user->as.stmt.as._struct;
        check_struct(ctx, s);
    }

    else if (type == ctx->type_void) {
        compile_error(ctx, tok, "only procedures may use the \"void\" type");
        return false;
    }

    else if (type->kind == Type_ALIAS && maybe_unwrap_type_alias(type) == ctx->type_void) {
        compile_error_start(ctx, tok, "variable is declared to have type ");
        print_type(type);
        compile_error_add_line(ctx, "which is an alias of void; only procedures may use the \"void\" type");
        compile_error_end();
        return false;
    }

    if (!(var->flags & VAR_IS_INITED)) return true;

    Type *value_type = var->value->resolved_type;
    assert(value_type);

    if (!does_type_describe_expr(ctx, type, var->value)) {
        compile_error_start(ctx, tok, "type mismatch: variable declared as type ");
        print_type(type);
        compile_error_add_line(ctx, ", but value was of type ");
        print_type(value_type);
        compile_error_end();

        return false;
    }

    return true;
}

void check_if(Context *ctx, AstStmt *node) {
    AstIf *iff = &node->as._if;
    if (iff->condition->resolved_type != ctx->type_bool && iff->condition->resolved_type->kind != Type_POINTER) {
        compile_error_start(ctx, stmt_tok(node), "'If' statement requires a condition which evaluates to a boolean, this one evaluates to ");
        print_type(iff->condition->resolved_type);
        compile_error_end();
        return;
    }
    if (iff->block_or_stmt->tag == Stmt_BLOCK) {
        check_block(ctx, &iff->block_or_stmt->as.block, Node_ZERO);
    } else check_statement(ctx, iff->block_or_stmt);
    if (iff->other_branch) check_statement(ctx, iff->other_branch);
}

void check_while(Context *ctx, AstStmt *node) {
    assert(node->tag == Stmt_WHILE);
    AstWhile *w = (AstWhile *)node;
    if (w->condition->resolved_type != ctx->type_bool && w->condition->resolved_type->kind != Type_POINTER) {
        compile_error_start(ctx, stmt_tok(node), "'while' statement requires a condition which evaluates to a boolean, this one evaluates to ");
        print_type(w->condition->resolved_type);
        compile_error_end();
        return;
    }
    check_block(ctx, (AstBlock *)w->block, Node_ZERO);
}

bool check_assignment(Context *ctx, AstExpr *expr, bool lhs_must_be_pointer) {
    Token tok = expr_tok(expr);
    if (expr->tag == Expr_UNARY) {
        AstUnary *u = (AstUnary *)expr;
        if (u->op != Token_STAR) {
            compile_error(ctx, tok, "unary expression on left hand side of assignment must be a dereference");
            return false;
        }
        return check_assignment(ctx, u->expr, true);
    }

    // expr is ensured to be either Expr_UNARY or Expr_BINARY

    AstBinary *binary = (AstBinary *)expr;

    Type *left_type = binary->left->resolved_type;
    Type *right_type = binary->right->resolved_type;

    if (binary->left->tag == Expr_PAREN) {
        left_type = ((AstParen *)binary->left)->sub_expr->resolved_type;
    }

    assert(left_type);
    assert(right_type);

    bool is_maths_assign = binary->op != Token_EQUAL; // is it a normal assignment? or *=, etc.
    if (is_maths_assign) {
        Type *real_left = maybe_unwrap_type_alias(left_type);
        if (!is_type_numeric(real_left) && !is_type_numeric(right_type) && real_left->kind != Type_POINTER && right_type->kind != Type_POINTER) {
            compile_error(ctx, tok, "type mismatch: arithmetic-assign operator expects numerical operands");
            return false;
        }
        return true;
    }

    if (lhs_must_be_pointer) {
        if (left_type->kind != Type_POINTER) {
            compile_error_start(ctx, tok, "cannot dereference type ");
            print_type(left_type);
            compile_error_end();
            return false;
        }
        left_type = left_type->data.base; // unwrap
    }

    if (!does_type_describe_expr(ctx, left_type, binary->right)) {
        compile_error_start(ctx, tok, "type mismatch: cannot assign value of type ");
        print_type(right_type);
        compile_error_add_line(ctx, " to variable of type ");
        print_type(left_type);
        compile_error_end();
        return false;
    }

    if (binary->left->tag == Expr_NAME) {
        /* TODO: check for const value modification without Name.resolved_decl */
        // Name *left_name = binary->left->as.name;
        // if (left_name->resolved_decl->tag == Decl_VAR) {
        //     AstVar *var = (AstVar *)left_name->resolved_decl;
        //     if (var->flags & VAR_IS_CONST) {
        //         compile_error(ctx, tok, "attempt to modify a const value");
        //         return false;
        //     }
        // }
    }

    return true;
}

void check_block(Context *ctx, AstBlock *block, AstNodeType restriction) {
    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        if (is_decl(stmt)) {
            assert(stmt->tag == Node_VAR);
            check_var(ctx, (AstDecl *)stmt); // NOTE modify for local procs
            continue;
        } else if (restriction != Node_ZERO && restriction != stmt->tag) {
            compile_error(ctx, stmt->token, "this statement is not allowed at this scope");
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
    case Stmt_WHILE:
        check_while(ctx, node);
        break;
    case Stmt_CALL:
        check_call(ctx, (AstNode *)node);
        break;
    case Stmt_ASSIGN: {
        check_assignment(ctx, &node->as.assign, false);
    } break;
    case Stmt_DEFER:
        check_statement(ctx, ((AstDefer *)node)->statement);
        break;
    case Stmt_RETURN: {
        if (check_proc_return_value(ctx, node)) {
            curr_checker_proc->flags |= PROC_RET_VALUE_CHECKED;
        }
    } break;
    }
}

void check_struct(Context *ctx, AstStruct *s) {
    AstBlock *block = &s->members->as.block;
    for (u64 i = 0; i < block->statements->len; i++) {
        AstNode *n = block->statements->nodes[i];
        assert(is_decl(n));
        check_var(ctx, (AstDecl *)n);
    }
}

void check_typedef(Context *ctx, AstDecl *node) {
    // AstTypedef *td = &node->as.typedefi;
    Type *type = node->as.type;
    AstNode *decl = type->data.user;
    if (decl->tag == Node_STRUCT) {
        check_struct(ctx, (AstStruct *)decl);
        return;
    }
    if (decl->tag == Node_TYPENAME) {
        if (type->kind == Type_ALIAS) {
            compile_error(ctx, decl->token, "you cannot create a type alias from another type alias");
        }
        return;
    }
    assert(decl->tag == Node_ENUM);
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
        case Node_PROCEDURE: {
            AstProcedure *proc = (AstProcedure *)node;
            if (proc->params) {
                for (int i = 0; i < proc->params->len; i++) {
                    check_var(ctx, (AstDecl *)proc->params->nodes[i]);
                }
            }
            if (proc->flags & PROC_IS_FOREIGN) continue;
            curr_checker_proc = proc;
            check_block(ctx, (AstBlock *)((AstProcedure *)node)->block, Node_ZERO);
            if (!(curr_checker_proc->flags & PROC_RET_VALUE_CHECKED)) {
                if (curr_checker_proc->return_type->as.type != ctx->type_void)
                    compile_error(ctx, node->token, "Non-void procedure has no 'return' statement");
            }
        } break;

        case Node_VAR:
            check_var(ctx, (AstDecl *)node);
            break;

        case Node_TYPEDEF:
            check_typedef(ctx, (AstDecl *)node);
            break;
        }
    }
}
