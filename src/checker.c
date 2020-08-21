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

void check_statement  (Context *, AstStmt *);
void check_block      (Context *, AstBlock *, AstNodeType);
void check_struct     (Context *, AstStruct *);
bool check_assignment (Context *, AstBinary *);
bool does_type_describe_expr(Context *ctx, Type *type, AstExpr *expr);

// Returns true if `a` and `b` point to the same Type, false otherwise.
// Does not print errors.
static bool do_pointer_types_match(Context *ctx, Type *a, Type *b) {
    if (a->data.base == ctx->type_void || b->data.base == ctx->type_void) {
        return true; // ^void is compatible with all pointer types
    } else if (a->data.base == b->data.base) {
        return true; // both types point to the same thing
    } else if (a == ctx->decoy_ptr || b == ctx->decoy_ptr) {
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
            compile_error(ctx, callnode->token, "call to \"%s\" specifies no arguments, but it's defined to expect %d of them", name, proc_arg_count);
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

static bool check_deref_assign(Context *ctx, const AstUnary *unary) {
    Token expr_token = ((AstNode *)unary)->token;
    AstBinary *assignment = NULL;
    assert(unary->expr->tag == Expr_BINARY);
    if (!is_assignment(unary->expr->as.binary)) {
        compile_error(ctx, expr_token, "expected pointer dereference to be the LHS of an assignment statement");
        return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
    }
    assignment = (AstBinary *)unary->expr;
    Type *lhs_type = assignment->left->resolved_type;
    if (lhs_type->kind != Type_POINTER) {
        compile_error_start(ctx, expr_token, "pointer dereference expects a pointer operand, but was given type ");
        print_type(lhs_type, stderr);
        compile_error_end();
        return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
    }
    Type *elem = lhs_type->data.base;
    if (!does_type_describe_expr(ctx, elem, assignment->right)) {
        compile_error_start(ctx, expr_token, "type mismatch: cannot assign value of type ");
        print_type(assignment->right->resolved_type, stderr);
        compile_error_add_line(ctx, " to ");
        print_type(elem, stderr);
        compile_error_add_line(ctx, " (derefernced from ");
        print_type(lhs_type, stderr);
        compile_error_add_line(ctx, ")");
        compile_error_end();
        return false;
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
            return false;

        Type *return_type = expr->resolved_type;

        // For return types, aliases must match exactly.
        if (return_type->kind == Type_ALIAS) {
            return (return_type == type);
        }

        // In other words, if the type is an int, allow any other integer type to be compatible with it.
        if (type == ctx->type_int) return (return_type->kind == Type_PRIMITIVE && return_type->data.signage != Signage_NaN);

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
            return check_assignment(ctx, binary);
        }
        if (binary->op == Token_DOT) {
            return do_types_match(ctx, expr->resolved_type, type);
        }

        Type *left_type = binary->left->resolved_type;
        Type *right_type = binary->right->resolved_type;
        if (left_type->kind != Type_POINTER || (left_type->kind == Type_PRIMITIVE && left_type->data.signage == Signage_NaN)) {
            compile_error(ctx, expr_token, "left hand side of arithmetic expression must be of one of these types: integer, floating point or pointer");
            return true; // We don't want more errors after this, so return true
        }
        if (right_type->kind != Type_POINTER || (right_type->kind == Type_PRIMITIVE && right_type->data.signage == Signage_NaN)) {
            compile_error(ctx, expr_token, "right hand side of arithmetic expression must be of one of these types: integer, floating point or pointer");
            return true; // We don't want more errors after this, so return true
        }
        return (type->kind == Type_PRIMITIVE && type->data.signage != Signage_NaN);
    } break;
    case Expr_UNARY: {
        const AstUnary *unary = &expr->as.unary;
        Type *sub_expr_type = unary->expr->resolved_type;

        if (unary->op == Token_BANG) {
            if (sub_expr_type->kind != Type_POINTER && sub_expr_type != ctx->type_bool) {
                compile_error(ctx, expr_token, "unary \"not\" expression must have a boolean or pointer operand");
                return false;
            }
            return (type == ctx->type_bool || type->kind == Type_POINTER);
        }

        if (unary->op == Token_MINUS) {
            if (sub_expr_type->kind != Type_PRIMITIVE && sub_expr_type->data.signage != Signage_NaN) {
                compile_error_start(ctx, expr_token, "unary negating expression expects integer or float operand, given ");
                print_type(unary->expr->resolved_type, stderr);
                compile_error_end();
                return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
            }
            return true;
        }

        if (unary->op == Token_STAR) { // dereference
            if (unary->expr->tag != Expr_BINARY) {
                check_assignment(ctx, (AstBinary *)unary->expr);
            }
            if (sub_expr_type->kind != Type_POINTER) {
                compile_error_start(ctx, expr_token, "pointer dereference expects a pointer operand, but was given type ");
                print_type(sub_expr_type, stderr);
                compile_error_end();
                return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
            }
            return true;
        }

        if (unary->op == Token_CARAT) { // address-of operator
            if (type->kind != Type_POINTER) return false;

            int addressof_depth = 1; // we'll use this a bit later on to create the real type of the expression.
            while (unary->expr->tag == Expr_UNARY && unary->expr->as.unary.op == Token_CARAT) {
                unary = &unary->expr->as.unary;
                addressof_depth++;
            }

            sub_expr_type = unary->expr->resolved_type;

            // It's a literal, you can't get a pointer a literal
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

            return do_pointer_types_match(ctx, type, resulting_type);
        }
    } break;
    case Expr_CAST: {
        AstCast *cast = &expr->as.cast;
        // TODO check if the casts expression can even be casted to the requested type
        bool matches = do_types_match(ctx, cast->typename->as.type, type);
        if (matches) {
            return true;
        }
        return false;
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

    case Expr_NULL:
        return (type->kind == Type_POINTER);
    case Expr_INT:
        return (type->kind == Type_PRIMITIVE && type->data.signage != Signage_NaN);
    case Expr_STRING:
        return (type == ctx->type_string);
    case Expr_BOOL:
        return (type == ctx->type_bool);

    default: assert(false);
    }
    return false;
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
            compile_error(ctx, tok, "only procedures declared as void can have value-less return statements");
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
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

    if (var->flags & VAR_IS_INFERRED) {
        Type *unwrapped = maybe_unwrap_type_alias(var->value->resolved_type);
        if (var->value->tag == Expr_CALL && unwrapped == ctx->type_void) {
            compile_error(ctx, tok, "cannot assign void value (in the form of call to \"%s\") to variable \"%s\"", var->value->as.call.name->as.name->text, node->name->text);
            return false;
        }
        return true;
    }

    Type *type = var->typename->as.type;

    if (type->kind == Type_ANON_STRUCT) {
        AstStruct *s = &type->data.user->as._struct;
        check_struct(ctx, s);
    }

    else if (type == ctx->type_void) {
        compile_error(ctx, tok, "only procedures may use the \"void\" type");
        return false;
    }

    else if (type->kind == Type_ALIAS && maybe_unwrap_type_alias(type) == ctx->type_void) {
        compile_error_start(ctx, tok, "variable is declared to have type \"%s\" ", type->name);
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

    Type *left_type  = binary->left->resolved_type;
    Type *right_type = binary->right->resolved_type;

    if (binary->left->tag == Expr_PAREN) {
        left_type = ((AstParen *)binary->left)->sub_expr->resolved_type;
    }

    Token tok = ((AstNode *)binary)->token;

    bool is_math_assign = binary->op != Token_EQUAL; // is it a normal assignment? or *=, etc.

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
    case Stmt_CALL:
        check_call(ctx, (AstNode *)node);
        break;
    case Stmt_ASSIGN: {
        // Guaranteed to be an assignment by parser
        AstExpr *e = (AstExpr *)node;
        assert(e->tag == Expr_UNARY || e->tag == Expr_BINARY);
        // if (e->tag == Expr_UNARY) {
        //     check_deref_assign(ctx, (AstUnary *)e);
        // } else if (e->tag == Expr_BINARY){
        //     check_assignment(ctx, (AstBinary *)e);
        // } else {
        //     compile_error(ctx, stmt_tok(node), "expected an assignment or declaration");
        // }
    } break;
    case Stmt_RETURN: {
        if (check_proc_return_value(ctx, node)) {
            ctx->curr_checker_proc->flags |= PROC_RET_VALUE_CHECKED;
        }
    } break;
    }
}

void check_struct(Context *ctx, AstStruct *s) { // TODO default struct values
    u64 struct_len = shlenu(s->members->as.block.symbols);
    for (int i = 0; i < struct_len; i++) {
        AstDecl *d = s->members->as.block.symbols[i].value;
        AstVar *v = (AstVar *)d;
        if (v->flags & VAR_IS_INITED) {
            compile_error(ctx, decl_tok(d), "struct fields cannot yet have default values");
        }
    }
    // AstBlock *fields = &s->members->as.block;
    // check_block(ctx, fields, Node_VAR);
}

void check_typedef(Context *ctx, AstDecl *node) {
    AstTypedef *td = &node->as.typedefi;
    if (td->of->tag == Node_STRUCT) {
        check_struct(ctx, (AstStruct *)td->of);
        return;
    }
    if (td->of->tag == Node_TYPENAME) {
        if (td->of->as.type->kind == Type_ALIAS) {
            compile_error(ctx, td->of->token, "you cannot create a type alias from another type alias");
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
        case Node_PROCEDURE: {
            AstProcedure *proc = (AstProcedure *)node;
            if (proc->params) {
                for (int i = 0; i < shlenu(proc->params); i++) {
                    check_var(ctx, proc->params[i].value);
                }
            }
            if (proc->flags & PROC_MOD_FOREIGN) continue;
            ctx->curr_checker_proc = proc;
            check_block(ctx, (AstBlock *)((AstProcedure *)node)->block, Node_ZERO);
            if (!(ctx->curr_checker_proc->flags & PROC_RET_VALUE_CHECKED)) {
                if (ctx->curr_checker_proc->return_type->as.type != ctx->type_void)
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
