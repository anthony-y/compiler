#include "headers/passes.h"

#include <assert.h>
#include <stdio.h>

#include "headers/stb/stb_ds.h"

static int num_errs_printed = 0;

static void print_type_error(Context *c, AstNode *node);
void print_decl_errors(Context *c, AstDecl *decl);
static void print_expression_errors(Context *c, AstExpr *node);
static void print_statement_errors(Context *c, AstStmt *node);

static inline void print_error_node(Context *c, AstNode *err) {
    if (!err) return;
    u64 line = ((AstNode *)err)->token.line;
    fprintf(stderr, "%s:%lu: Error: %s\n", c->current_file_path, line, err->as.error.msg);
    num_errs_printed++;
    free(err);
}

static void print_expr_error(Context *c, AstExpr *err) {
    if (!err || err->tag != Expr_ERROR) return;
    u64 line = ((AstNode *)err)->token.line;
    fprintf(stderr, "%s:%lu: Error: %s\n", c->current_file_path, line, err->as.error.msg);
    num_errs_printed++;
}

static void print_stmt_error(Context *c, AstStmt *err) {
    if (!err || err->tag != Stmt_ERROR) return;
    u64 line = ((AstNode *)err)->token.line;
    fprintf(stderr, "%s:%lu: Error: %s\n", c->current_file_path, line, err->as.error.msg);
    num_errs_printed++;
}

static void print_node_errors(Context *c, AstNode *n) {
    if (n->tag == Node_ERROR) {
        print_error_node(c, n);
        return;
    }
    if (is_decl(n)) {
        print_decl_errors(c, (AstDecl *)n);
        return;
    }
    if (n->tag > Node_EXPRESSIONS_START && n->tag < Node_EXPRESSIONS_END) {
        print_expression_errors(c, (AstExpr *)n);
        return;
    }
    if (n->tag > Node_STATEMENTS_START && n->tag < Node_STATEMENTS_END) {
        print_statement_errors(c, (AstStmt *)n);
        return;
    }
    assert(false);
}

static void print_expression_errors(Context *c, AstExpr *node) {
    if (!node) return;

    switch (node->tag) {
    case Expr_NAME:
    case Expr_INT:
    case Expr_BOOL:
    case Expr_STRING:
    case Expr_ARRAY:
    case Expr_FLOAT:
    case Expr_NULL:
        break;
    case Expr_ERROR:
        print_expr_error(c, node);
        break;
    case Expr_PAREN:
        print_expression_errors(c, node->as.paren.sub_expr);
        break;
    case Expr_BINARY:
        print_expression_errors(c, node->as.binary.left);
        print_expression_errors(c, node->as.binary.right);
        break;
    case Expr_UNARY:
        print_expression_errors(c, node->as.unary.expr);
        break;
    case Expr_CALL:
        print_ast_errors(c, node->as.call.params);
        print_expression_errors(c, node->as.call.name);
        break;
    case Expr_INDEX:
        print_expression_errors(c, node->as.index.name);
        print_expression_errors(c, node->as.index.index);
        break;
    case Expr_CAST:
        print_type_error(c, node->as.cast.typename);
        print_expression_errors(c, node->as.cast.expr);
        break;

    default:
        printf("Internal compiler error: expression error not covered in printer. ");
        printf("Node type: %d\n", node->tag);
    }
}

static void print_statement_errors(Context *c, AstStmt *node) {
    if (!node) return;

    if (node->tag == Stmt_ERROR) {
        print_stmt_error(c, node);
        return;
    }

    else if (node->tag == Stmt_BLOCK) {
        const AstBlock *block = &node->as.block;
        u64 num_decls = shlenu(block->symbols);
        for (int i = 0; i < num_decls; i++) {
            print_decl_errors(c, block->symbols[i].value);
        }
        for (int i = 0; i < block->statements->len; i++) {
            print_statement_errors(c, (AstStmt *)block->statements->nodes[i]);
        }
    }

    else if (node->tag == Stmt_STRUCT) {
        const AstStruct *def = &node->as._struct;
        print_statement_errors(c, def->members);
    }

    else if (node->tag == Stmt_DEFER) {
        print_node_errors(c, node->as.defer.statement);
    }

    else if (node->tag == Stmt_RETURN) {
        print_expression_errors(c, node->as._return.expr);
    }

    else if (node->tag == Stmt_WHILE) {
        print_expression_errors(c, node->as._while.condition);
        print_statement_errors(c, node->as._while.block);
    }

    else if (node->tag == Stmt_IF) {
        print_expression_errors(c, node->as._if.condition);
        print_statement_errors(c, node->as._if.block_or_stmt);
    }

    else if (node->tag == Stmt_ASSIGN) {
        print_expression_errors(c, (AstExpr *)node);
    }

    else {
        fprintf(stderr, "Internal compiler error: print_statement_errors shouldn't have got here. Node type is %d.\n", node->tag);
    }
}

static void print_type_error(Context *c, AstNode *node) {
    Type *astype = node->as.type;
    if (astype->kind == Type_STRUCT || astype->kind == Type_ANON_STRUCT) {
        print_statement_errors(c, astype->data.user);
    }
}

void print_decl_errors(Context *c, AstDecl *decl) {
    switch (decl->tag) {
    case Decl_VAR: {
        const AstVar *def = &decl->as.var;
        if (!(def->flags & VAR_IS_INFERRED)) print_type_error(c, def->typename);
        if (def->flags & VAR_IS_INITED) print_expression_errors(c, def->value);
    } break;
    case Decl_PROC: {
        const AstProcedure *proc = &decl->as.proc;
        if (proc->params) {
            int param_len = shlenu(proc->params);
            for (int i = 0; i < param_len; i++) {
                print_decl_errors(c, proc->params[i].value);
            }
        }
        print_type_error(c, proc->return_type);
        print_statement_errors(c, proc->block);
    } break;
    case Decl_TYPEDEF: {
        const AstTypedef *def = &decl->as.typedefi;
        if (def->of->tag == Node_STRUCT) {
            print_statement_errors(c, (AstStmt *)def->of);
            return;
        }
        print_node_errors(c, def->of);
    } break;
    default: assert(false);
    }
}

void print_ast_errors(Context *c, Ast *ast) {
    if (!ast || !ast->nodes || ast->len == 0) return;

    for (int i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (!node) return;

        if (node->tag == Node_ERROR) {
            print_error_node(c, node);
            continue;
        }

        assert (is_decl(node));
        print_decl_errors(c, (AstDecl *)node);
    }
}
