// Invoked after symbol and type resolution, the checker ensures an AST is semantically correct.
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
void check_block      (Context *, AstBlock *);
void check_struct     (Context *, AstStruct *);
bool check_assignment(Context *ctx, AstExpr *expr, bool lhs_must_be_pointer);
bool does_type_describe_expr(Context *ctx, AstTypeDecl *type, AstExpr *expr);
bool check_decl(Context *ctx, AstDecl *decl);
void check_typedef(Context *ctx, AstTypeDecl *decl);
static bool do_types_match(Context *ctx, AstTypeDecl *a, AstTypeDecl *b);

// Returns true if `a` and `b` point to the same Type, false otherwise.
// Does not print errors.
static bool do_pointer_types_match(Context *ctx, AstTypeDecl *a, AstTypeDecl *b) {
    if (a->base_type == ctx->type_void || b->base_type == ctx->type_void) {
        return true; // ^void is compatible with all pointer types
    }

    if (a->base_type == b->base_type) {
        return true; // both types point to the same thing
    }

    return do_types_match(ctx, a->base_type, b->base_type);
}

static bool do_types_match(Context *ctx, AstTypeDecl *a, AstTypeDecl *b) {
    if (a == ctx->type_any || b == ctx->type_any) return true;

    if (a->expr_type == TypeDecl_ALIAS && b->expr_type == TypeDecl_ALIAS) return (a == b);

    // This allows us to ensure that literals can be compared with aliases,
    // but still be restrictive about alias vs alias matching.
    if (a->expr_type == TypeDecl_ALIAS) {
        a = a->base_type;
        return do_types_match(ctx, a, b);
    } else if (b->expr_type == TypeDecl_ALIAS) {
        b = b->base_type;
        return do_types_match(ctx, b, a);
    }

    if (a->expr_type == TypeDecl_INTEGER) {
        if (b->expr_type != TypeDecl_INTEGER) return false;
        if (a->size > b->size) return false;
        return true;
    }

    /*
    if (b->expr_type == TypeDecl_INTEGER) {
        if (a->expr_type == TypeDecl_FLOAT) return false;
        if (b->size < a->size) return true;
        if (a->expr_type == TypeDecl_INTEGER) return true;
        return false;
    }
    */

    if (a->expr_type == TypeDecl_POINTER && b->expr_type == TypeDecl_POINTER) return do_pointer_types_match(ctx, a, b);
    if (a->expr_type == TypeDecl_ARRAY && b->expr_type == TypeDecl_ARRAY) return do_types_match(ctx, a->base_type, b->base_type);
    if (a->expr_type != b->expr_type) return false;
    if (a != b) return false;

    return true;
}

static AstTypeDecl *maybe_unwrap_type_alias(AstTypeDecl *alias) {
    if (alias->expr_type != TypeDecl_ALIAS) return alias;
    return alias->base_type;
}

static AstTypeDecl *maybe_unwrap_array_type(AstTypeDecl *array) {
	if (array->expr_type != TypeDecl_ARRAY) return array;
	return array->base_type;
}

// Performs type checking on a call and ensures it's arguments are correct.
// Prints it's own errors.
static bool check_call(Context *ctx, AstCall *call) {
    char *name = NULL;

    if (call->name->tag == Node_IDENT) {
        name = ((AstIdent *)call->name)->name->text;
    } else if (call->name->tag == Node_BINARY) {
        auto binary = (AstBinary *)call->name;
        assert(binary->op == Token_DOT);
        assert(binary->left->tag == Node_IDENT);
        assert(binary->right->tag == Node_IDENT);
        name = ((AstIdent *)binary->right)->name->text;
    }

    AstProcedure *proc = call->calling;

	AstDecl *var_args = (proc->var_args_index > -1 ? (AstDecl *)proc->params->nodes[proc->var_args_index] : NULL);

    int proc_arg_count = (proc->params ? proc->params->len : 0);
    int call_arg_count = (call->params ? call->params->len : 0);

    if (call_arg_count > proc_arg_count && !var_args) {
        int diff = call_arg_count - proc_arg_count;
        compile_error(ctx, call->token, "%d too many arguments in call to '%s'", diff, name);
        return false;
    }

	if (var_args) proc_arg_count -= 1;

    if (call_arg_count < proc_arg_count) {
        int diff = proc_arg_count - call_arg_count;
        compile_error(ctx, call->token, "%d too few arguments in call to '%s'", diff, name);
        return false;
    }

    for (int i = 0; i < call_arg_count-1; i++) {
		AstTypeDecl *caller_type = ((AstExpr *)call->params->nodes[i])->resolved_type;

		if (i >= proc->var_args_index) continue;

		AstDecl *defn = (AstDecl *)proc->params->nodes[i];

        if (!do_types_match(ctx, caller_type, defn->given_type->resolved_type)) {
            compile_error_start(ctx, call->token, "type mismatch: argument %d of procedure \"%s\" is defined as type ", i+1, name);
            print_type(defn->given_type->resolved_type);
            compile_error_add_line(ctx, " but caller provided argument of type ");
            print_type(caller_type);
            compile_error_end();
            return true;
        }
    }

    return true;
}

static bool check_unary_against_type(Context *ctx, AstUnary *unary, AstTypeDecl *against, Token expr_token) {
    AstTypeDecl *sub_expr_type = unary->expr->resolved_type;

    if (unary->op == Token_BANG) {
        if (sub_expr_type->expr_type != TypeDecl_POINTER && sub_expr_type != ctx->type_bool) {
            compile_error(ctx, expr_token, "unary \"not\" expression must have a boolean or pointer operand");
            return false;
        }
        return (against == ctx->type_bool || against->expr_type == TypeDecl_POINTER);
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
        if (unary->expr->tag == Node_BINARY) {
            check_assignment(ctx, unary->expr, true);
        }
        if (sub_expr_type->expr_type != TypeDecl_POINTER) {
            compile_error_start(ctx, expr_token, "pointer dereference expects a pointer operand, but was given type ");
            print_type(sub_expr_type);
            compile_error_end();
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
        }
        return do_types_match(ctx, against, sub_expr_type->base_type);
    }

    if (unary->op == Token_CARAT) { // address-of operator
        if (against->expr_type != TypeDecl_POINTER) return false;

        int addressof_depth = 1; // we'll use this a bit later on to create the real type of the expression.
        while (unary->expr->tag == Node_UNARY && ((AstUnary *)unary)->op == Token_CARAT) {
            unary = (AstUnary *)unary->expr;
            addressof_depth++;
        }

        sub_expr_type = unary->expr->resolved_type;

        // It's a literal, you can't get a pointer to a literal
        if (unary->expr->tag > Node_LITERALS_START && unary->expr->tag < Node_LITERALS_END) {
            compile_error(ctx, expr_token, "cannot get the address of a literal value");
            return false;
        }

        else if (unary->expr->tag == Node_CALL) {
            if (!check_call(ctx, (AstCall *)unary->expr)) {
                return false;
            }
        }

        assert(sub_expr_type);

        // Both of these start out as the same value, but by the end
        // resulting_type will still point to the outer pointer type,
        // and inner_type will point to the inner-most pointer.
        AstTypeDecl *resulting_type = make_pointer_type(NULL);
        AstTypeDecl *inner_type = resulting_type;

        for (int i = 1; i < addressof_depth; i++) { // i starts at 1 because we're already 1 deep
            AstTypeDecl *inner_ptr = make_pointer_type(NULL);
            inner_type->base_type = inner_ptr;
            inner_type = inner_ptr;
        }
        inner_type->base_type = sub_expr_type;

        return do_pointer_types_match(ctx, against, resulting_type);
    }
    return false;
}

static bool can_type_cast_to_type(Context *ctx, AstExpr *site, AstTypeDecl *from, AstTypeDecl *to) {
    if (from->expr_type == TypeDecl_POINTER && from->base_type == ctx->type_void) {
        return (to->expr_type == TypeDecl_POINTER);
    }
    if (do_types_match(ctx, to, from)) {
        compile_warning(ctx, site->token, "unnecessary cast");
        return true;
    }
    if (from->expr_type == TypeDecl_FLOAT || from->expr_type == TypeDecl_INTEGER) {
        return (to != ctx->type_string);
    }
    return false;
}

// Returns true if `expr` is compatible with `type`.
bool does_type_describe_expr(Context *ctx, AstTypeDecl *type, AstExpr *expr) {
    Token expr_token = expr->token;

    // We want to check against the element type.
    if (type->expr_type == TypeDecl_ARRAY) {
        type = type->base_type;
    }

    if (expr->tag == Node_CALL) {
        if (!check_call(ctx, (AstCall *)expr))
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;

        AstTypeDecl *return_type = expr->resolved_type;

        // For return types, aliases must match exactly.
        if (return_type->expr_type == TypeDecl_ALIAS) {
            return (return_type == type);
        }

        // In other words, if the type is an int, allow any other integer type to be compatible with it.
        if (type == ctx->type_int) return is_type_numeric(return_type);

        return do_types_match(ctx, type, return_type);
    }

    type = maybe_unwrap_type_alias(type); // do this after checking calls since calls are more strict about aliases

    switch (expr->tag) {
    case Node_PROCEDURE: {
        AstTypeDecl *procedure_type = expr->resolved_type;
        auto proc = (AstProcedure *)expr;
        if (proc->params) {
            for (int i = 0; i < proc->params->len; i++) {
                check_decl(ctx, (AstDecl *)proc->params->nodes[i]);
            }
        }
        if (proc->flags & PROC_IS_FOREIGN) return do_types_match(ctx, proc->return_type->resolved_type, type);
        curr_checker_proc = proc;
        check_block(ctx, (AstBlock *)((AstProcedure *)expr)->block);
        if (!(curr_checker_proc->flags & PROC_RET_VALUE_CHECKED)) {
            if (curr_checker_proc->return_type->resolved_type != ctx->type_void)
                compile_error(ctx, expr_token, "Non-void procedure has no 'return' statement");
        }
        return do_types_match(ctx, expr->resolved_type, type);
    } break;

    case Node_BINARY: {
        AstBinary *binary = (AstBinary *)expr;
        if (is_binary_comparison(*binary)) {
            return (type == ctx->type_bool);
        }
        if (is_assignment(*binary)) {
            return check_assignment(ctx, expr, false);
        }
        if (binary->op == Token_DOT) {
            return do_types_match(ctx, expr->resolved_type, type);
        }

        AstTypeDecl *left_type = binary->left->resolved_type;
        AstTypeDecl *right_type = binary->right->resolved_type;
        if (!is_type_numeric(left_type) && !is_type_numeric(right_type) && left_type->expr_type != TypeDecl_POINTER && right_type->expr_type != TypeDecl_POINTER) {
            compile_error(ctx, expr_token, "operands of arithmetic expressions need to be numeric");
            return FAILED_BUT_DONT_ERROR_AT_CALL_SITE;
        }
        return is_type_numeric(type) || type->expr_type == TypeDecl_POINTER;
    } break;

    case Node_UNARY: {
        return check_unary_against_type(ctx, (AstUnary *)expr, type, expr_token);
    } break;

    case Node_CAST: {
        AstCast *cast = (AstCast *)expr;
        AstTypeDecl *as_type = cast->type->resolved_type;
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

    case Node_INDEX: {
        AstTypeDecl *array_type = ((AstArrayIndex *)expr)->name->resolved_type;
        if (array_type->expr_type != TypeDecl_ARRAY) {
            compile_error(ctx, expr_token, "attempt to index \"%s\" like it's an array, but it isn't");
            return false;
        }
        return do_types_match(ctx, type, expr->resolved_type);
    } break;

    case Node_PAREN: {
        return does_type_describe_expr(ctx, type, ((AstParen *)expr)->sub_expr);
    } break;

    case Node_IDENT: {
        return do_types_match(ctx, type, expr->resolved_type);
    } break;

    case Node_IMPORT:
        return (type == ctx->import_type);
    case Node_LIBRARY:
        return (type == ctx->type_library);
    case Node_NULL_LIT:
        return (type->expr_type == TypeDecl_POINTER);
    case Node_INT_LIT:
        return is_type_numeric(type);
    case Node_FLOAT_LIT:
        return (type->expr_type == TypeDecl_FLOAT);
    case Node_STRING_LIT:
        return (type == ctx->type_string);
    case Node_BOOL_LIT:
        return (type == ctx->type_bool);

    default: assert(false);
    }
    return false;
}

// Ensure a return statement is compatible with the procedure declaration
// that it resides inside. Uses `curr_checker_proc;` to compare against.
bool check_proc_return_value(Context *ctx, AstStmt *retnode) {
    AstTypeDecl *return_type = curr_checker_proc->return_type->resolved_type;

    Token tok = retnode->token;
    auto r = (AstReturn *)retnode;

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

bool check_decl(Context *ctx, AstDecl *decl) {
    Token tok = decl->token;

    if (decl->flags & DECL_IS_INFERRED) {
        AstTypeDecl *unwrapped = maybe_unwrap_type_alias(decl->expr->resolved_type);
        if (decl->expr->tag == Node_CALL && unwrapped == ctx->type_void) {
			Name *call_name = ((AstIdent *)((AstCall *)decl->expr)->name)->name;
            compile_error(ctx, tok, "cannot assign void value (in the form of call to \"%s\") to variable \"%s\"", call_name->text, decl->name->text);
            return false;
        }
    } else if (decl->expr && decl->expr->tag == Node_IMPORT) {
        compile_error(ctx, tok, "type of import declaration must be inferred");
        return NULL;
    }

    assert(decl->given_type->tag == Node_TYPENAME);
    auto type = decl->given_type->resolved_type;

    if (type == ctx->import_type && !(decl->flags & DECL_IS_TOP_LEVEL)) {
        compile_error(ctx, tok, "import declarations are not allowed at local scope");
        return NULL;
    }

    if (type == ctx->type_void && decl->expr->tag != Node_PROCEDURE) { // TODO: proper type resolution for procedures.
        compile_error(ctx, tok, "only procedures may use the \"void\" type");
        return false;
    }

    else if (type->expr_type == TypeDecl_ALIAS && maybe_unwrap_type_alias(type) == ctx->type_void) {
        compile_error_start(ctx, tok, "variable is declared to have type ");
        print_type(type);
        compile_error_add_line(ctx, "which is an alias of void; only procedures may use the \"void\" type");
        compile_error_end();
        return false;
    }

    if (!decl->expr) return true;

    AstTypeDecl *value_type = decl->expr->resolved_type;
    assert(value_type);

    if (!does_type_describe_expr(ctx, type, decl->expr)) {
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
    AstIf *iff = (AstIf *)node;
    if (iff->condition->resolved_type != ctx->type_bool && iff->condition->resolved_type->expr_type != TypeDecl_POINTER) {
        compile_error_start(ctx, node->token, "'If' statement requires a condition which evaluates to a boolean, this one evaluates to ");
        print_type(iff->condition->resolved_type);
        compile_error_end();
        return;
    }
    if (iff->block_or_stmt->tag == Node_BLOCK) {
        check_block(ctx, (AstBlock *)iff->block_or_stmt);
    } else check_statement(ctx, iff->block_or_stmt);
    if (iff->other_branch) check_statement(ctx, iff->other_branch);
}

void check_while(Context *ctx, AstStmt *node) {
    assert(node->tag == Node_WHILE);
    AstWhile *w = (AstWhile *)node;
    if (w->condition->resolved_type != ctx->type_bool && w->condition->resolved_type->expr_type != TypeDecl_POINTER) {
        compile_error_start(ctx, node->token, "'while' statement requires a condition which evaluates to a boolean, this one evaluates to ");
        print_type(w->condition->resolved_type);
        compile_error_end();
        return;
    }
    check_block(ctx, (AstBlock *)w->block);
}

bool check_assignment(Context *ctx, AstExpr *expr, bool lhs_must_be_pointer) {
    Token tok = expr->token;
    if (expr->tag == Node_UNARY) {
        AstUnary *u = (AstUnary *)expr;
        if (u->op != Token_STAR) {
            compile_error(ctx, tok, "unary expression on left hand side of assignment must be a dereference");
            return false;
        }
        return check_assignment(ctx, u->expr, true);
    }

    // expr is ensured to be either Node_UNARY or Node_BINARY

    AstBinary *binary = (AstBinary *)expr;

    AstTypeDecl *left_type = binary->left->resolved_type;
    AstTypeDecl *right_type = binary->right->resolved_type;

    if (binary->left->tag == Node_PAREN) {
        left_type = ((AstParen *)binary->left)->sub_expr->resolved_type;
    }

    assert(left_type);
    assert(right_type);

    bool is_maths_assign = binary->op != Token_EQUAL; // is it a normal assignment? or *=, etc.
    if (is_maths_assign) {
        AstTypeDecl *real_left = maybe_unwrap_type_alias(left_type);
        if (!is_type_numeric(real_left) && !is_type_numeric(right_type) && real_left->expr_type != TypeDecl_POINTER && right_type->expr_type != TypeDecl_POINTER) {
            compile_error(ctx, tok, "type mismatch: arithmetic-assign operator expects numerical operands");
            return false;
        }
        return true;
    }

    if (lhs_must_be_pointer) {
        if (left_type->expr_type != TypeDecl_POINTER) {
            compile_error_start(ctx, tok, "cannot dereference type ");
            print_type(left_type);
            compile_error_end();
            return false;
        }
        left_type = left_type->base_type; // unwrap
    }

    if (!does_type_describe_expr(ctx, left_type, binary->right)) {
        compile_error_start(ctx, tok, "type mismatch: cannot assign value of type ");
        print_type(right_type);
        compile_error_add_line(ctx, " to variable of type ");
        print_type(left_type);
        compile_error_end();
        return false;
    }

    if (binary->left->tag == Node_IDENT) {
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

void check_block(Context *ctx, AstBlock *block) {
    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        if (stmt->tag == Node_DECL) check_decl(ctx, (AstDecl *)stmt);
        else if (stmt->tag == Node_TYPE_DECL) check_typedef(ctx, (AstTypeDecl *)stmt);
        // TODO check_type_decl or something
        else check_statement(ctx, (AstStmt *)stmt);
    }
}

// Check a block-level statement.
void check_statement(Context *ctx, AstStmt *node) {
    switch (node->tag) {
    case Node_IF:
        check_if(ctx, node);
        break;
    case Node_WHILE: {
        check_while(ctx, node);
    } break;
    case Node_CALL: {
        auto call = (AstCallStmt *)node;
        check_call(ctx, call->expr);
    } break;
    case Node_ASSIGN: {
        auto assign = (AstAssignment *)node;
        check_assignment(ctx, assign->expr, false);
    } break;
    case Node_DEFER: {
        check_statement(ctx, ((AstDefer *)node)->statement);
    } break;
    case Node_RETURN: {
        check_proc_return_value(ctx, node);
		curr_checker_proc->flags |= PROC_RET_VALUE_CHECKED;
    } break;
    }
}

void check_struct(Context *ctx, AstStruct *s) {
    AstBlock *block = (AstBlock *)s->members;
    for (u64 i = 0; i < block->statements->len; i++) {
        AstNode *n = block->statements->nodes[i];
        assert(n->tag == Node_DECL || n->tag == Node_TYPE_DECL);
        check_decl(ctx, (AstDecl *)n);
    }
}

void check_typedef(Context *ctx, AstTypeDecl *decl) {
    if (!(decl->flags & DECL_IS_INFERRED)) {
		assert(decl->given_type->tag == Node_TYPENAME);
		auto given_type = (AstTypeDecl *)decl->given_type;
        if (given_type != ctx->type_type) {
            compile_error(ctx, decl->token, "type mismatch: declaration creates a type, but it was declared as '%s' (use 'Type' as the type of the declaration, or omit the type-name and I'll infer it for you)", given_type->name->text);
        }
    }

    switch (decl->expr_type) {
    case TypeDecl_STRUCT: check_struct(ctx, decl->struct_); return;
    case TypeDecl_ALIAS: return;
	case TypeDecl_TYPE: return;
    default: printf("decl_expr_type = %d\n", decl->expr_type);
    }
}

// Recursively perform semantic analysis and type-checking on an Ast.
void check_ast(Context *ctx, Ast *ast) {
    if (!ast || !ast->nodes || ast->len == 0) return;

    for (int i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (!node) return;

        if (node->tag == Node_DECL) check_decl(ctx, (AstDecl *)node);
        else if (node->tag == Node_TYPE_DECL) check_typedef(ctx, (AstTypeDecl *)node);
        else assert(false);
    }
}
