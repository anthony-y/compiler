// Functions to create, delete and query AST (Abstract Syntax Tree) nodes.
#include "headers/ast.h"
#include "headers/token.h"
#include "headers/parser.h"
#include "headers/arena.h"
#include "headers/ast.h"
#include "headers/common.h"
#include "headers/context.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "headers/stb/stb_ds.h"

inline Token decl_tok(AstDecl *d) {return ((AstNode *)d)->token;}
inline Token expr_tok(AstExpr *e) {return ((AstNode *)e)->token;}
inline Token stmt_tok(AstStmt *s) {return ((AstNode *)s)->token;}

inline bool is_assignment(AstBinary b) {
    return (b.op > Token_ASSIGNMENTS_START && b.op < Token_ASSIGNMENTS_END);
}

inline bool is_binary_comparison(AstBinary b) {
    return (b.op > Token_BINARY_COMPARE_START && b.op < Token_BINARY_COMPARE_END);
}

inline bool is_decl(AstNode *n) {
    return (n->tag > Node_DECLS_START && n->tag < Node_DECLS_END);
}

inline bool is_literal(AstNode *n) {
    return (n->tag > Node_LITERALS_START && n->tag < Node_LITERALS_END);
}

void free_block(AstStmt *stmt) {
    if (!stmt || stmt->tag != Stmt_BLOCK) return;
    AstBlock *b = (AstBlock *)stmt;
    for (int i = 0; i < b->statements->len; i++) {
        AstNode *n = b->statements->nodes[i];
        switch (n->tag) {
        case Node_IF: {
            AstIf *iff = (AstIf *)n;
            if (iff->block_or_stmt) free_block(iff->block_or_stmt);
            if (iff->other_branch) free_block(iff->other_branch);
        } break;
        case Node_WHILE: {
            AstWhile *w = (AstWhile *)n;
            free_block(w->block);
        } break;
        case Node_VAR: {
            AstVar *v = (AstVar *)n;
            if (v->typename->as.type && v->typename->as.type->kind == Type_ANON_STRUCT) {
                free_block(((AstStruct *)v->typename->as.type->data.user)->members);
            }
        } break;
        case Node_CALL: {
            AstCall *call = (AstCall *)n;
            if (call->params) ast_free(call->params);
        } break;
		free(n);
        }
    }

    ast_free(b->statements);
    ast_free(b->deferred);
    b->parent = NULL;
}

void free_subtrees_and_blocks(Ast *ast) {
    for (int i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        switch (node->tag) {
        case Node_PROCEDURE: {
            AstProcedure *proc = (AstProcedure *)node;
            if (proc->params) ast_free(proc->params);
            free_block(proc->block);
        } break;
        }
    }
}

inline AstNode *ast_node(Context *c, AstNodeType tag, Token t) {
    AstNode *node = malloc(sizeof(AstNode));

    node->tag = tag;
    node->token = t;

    return node;
}

AstNode *ast_decl(Context *c, AstNodeType tag, Token t, const AstDecl *decl) {
    AstNode *node = ast_node(c, tag, t);
    node->as.decl = *decl;
    return node;
}

AstNode *ast_expr(Context *c, AstNodeType tag, Token t, const AstExpr *expr) {
    AstNode *node = ast_node(c, tag, t);
    node->as.expr = *expr;
    return node;
}

AstNode *ast_stmt(Context *c, AstNodeType tag, Token t, const AstStmt *stmt) {
    AstNode *node = ast_node(c, tag, t);
    node->as.stmt = *stmt;
    return node;
}

void ast_init(Ast *list, int cap) {
    list->nodes = malloc(cap * sizeof(AstNode *));
    list->cap   = cap;
    list->len   = 0;
}

void ast_free(Ast *list) {
    // free_subtrees_and_blocks(list);
    assert(list->nodes);
    free(list->nodes);
    list->cap = 0;
    list->len = 0;
}

void ast_add(Ast *list, AstNode *node) {
    if (node && list->cap < list->len) {
        list->cap *= 2;
        AstNode **tmp = realloc(list->nodes, list->cap * sizeof(AstNode *));
        if (tmp) {
			list->nodes = tmp;
        } else {
            fprintf(stderr, "Alloc failed\n");
            free(list->nodes);
            return;
        }
    }

    list->nodes[list->len] = node;
    list->len += 1;
}

AstExpr *ast_name(Context *c, Token t) {
    AstNode *n = ast_node(c, Node_IDENT, t);
    n->as.expr.tag = Expr_NAME;
    n->as.expr.as.name = make_name_from_token(c, t);
    return &n->as.expr;
}

AstExpr *ast_binary(Context *c, Token t, const AstBinary *binary) {
    AstNode *n = ast_node(c, Node_BINARY, t);
    n->as.expr.tag = Expr_BINARY;
    n->as.expr.as.binary = *binary;
    return &n->as.expr;
}

AstExpr *ast_unary(Context *c, Token t, const AstUnary *unary) {
    AstNode *n = ast_node(c, Node_UNARY, t);
    n->as.expr.tag = Expr_UNARY;
    n->as.expr.as.unary = *unary;
    return &n->as.expr;
}

AstExpr *ast_selector(Context *c, Token t, const AstSelector *sel) {
    AstNode *n = ast_node(c, Node_SELECTOR, t);
    n->as.expr.tag = Expr_SELECTOR;
    n->as.expr.as.selector = *sel;
    return &n->as.expr;
}

AstExpr *ast_paren(Context *c, Token t, const AstParen *paren) {
    AstNode *n = ast_node(c, Node_PAREN, t);
    n->as.expr.tag = Expr_PAREN;
    n->as.expr.as.paren = *paren;
    return &n->as.expr;
}

AstExpr *ast_cast(Context *c, Token t, const AstCast *cast) {
    AstNode *n = ast_node(c, Node_CAST, t);
    n->as.expr.tag = Expr_CAST;
    n->as.expr.as.cast = *cast;
    return &n->as.expr;
}

AstExpr *ast_index(Context *c, Token t, const AstArrayIndex *index) {
    AstNode *n = ast_node(c, Node_INDEX, t);
    n->as.expr.tag = Expr_INDEX;
    n->as.expr.as.index = *index;
    return &n->as.expr;
}

AstExpr *ast_var_args_expand(struct Context *c, Token t, const AstVarArgsExpand *expand) {
    AstNode *n = ast_node(c, Node_VAR_ARGS_EXPAND, t);
    n->as.expr.tag = Expr_VAR_ARGS_EXPAND;
    n->as.expr.as.var_args_expand = *expand;
    return &n->as.expr;
}

AstExpr *ast_import(Context *c, Token t, const AstImport *imp) {
    AstNode *n = ast_node(c, Node_IMPORT, t);
    n->as.expr.tag = Expr_IMPORT;
    n->as.expr.as.import = *imp;
    return &n->as.expr;
}

AstDecl *ast_proc(Context *c, Token t, Name *name, const AstProcedure *proc) {
    AstNode *n = ast_node(c, Node_PROCEDURE, t);
    n->as.decl.flags = 0;
    n->as.decl.status = Status_UNRESOLVED;
    n->as.decl.tag = Decl_PROC;
    n->as.decl.name = name;
    n->as.decl.as.proc = *proc;
    return &n->as.decl;
}

AstDecl *ast_var(Context *c, Token t, Name *name, const AstVar *var) {
    AstNode *n = ast_node(c, Node_VAR, t);
    n->as.decl.flags = 0;
    n->as.decl.status = Status_UNRESOLVED;
    n->as.decl.tag = Decl_VAR;
    n->as.decl.name = name;
    n->as.decl.as.var = *var;
    return &n->as.decl;
}

AstDecl *ast_typedefi(Context *c, Token t, Name *name, Type *type) {
    AstNode *n = ast_node(c, Node_TYPEDEF, t);
    n->as.decl.flags = 0;
    n->as.decl.status = Status_UNRESOLVED;
    n->as.decl.tag = Decl_TYPEDEF;
    n->as.decl.name = name;
    n->as.decl.as.type = type;
    return &n->as.decl;
}

AstStmt *ast_assignment(Context *c, Token t, const AstExpr *ass) {
    AstNode *n = ast_node(c, Node_ASSIGN, t);
    n->as.stmt.tag = Stmt_ASSIGN;
    n->as.stmt.as.assign = *ass;
    return &n->as.stmt;
}

AstStmt *ast_block(Context *c, Token t, const AstBlock *blk) {
    AstNode *n = ast_node(c, Node_BLOCK, t);
    n->as.stmt.tag = Stmt_BLOCK;
    n->as.stmt.as.block = *blk;
    return &n->as.stmt;
}

AstStmt *ast_if(Context *c, Token t, const AstIf *i) {
    AstNode *n = ast_node(c, Node_IF, t);
    n->as.stmt.tag = Stmt_IF;
    n->as.stmt.as._if = *i;
    return &n->as.stmt;
}

AstStmt *ast_while(Context *c, Token t, const AstWhile *w) {
    AstNode *n = ast_node(c, Node_WHILE, t);
    n->as.stmt.tag = Stmt_WHILE;
    n->as.stmt.as._while = *w;
    return &n->as.stmt;
}

AstStmt *ast_struct(Context *c, Token t, const AstStruct *s) {
    AstNode *n = ast_node(c, Node_STRUCT, t);
    n->as.stmt.tag = Stmt_STRUCT;
    n->as.stmt.as._struct = *s;
    return &n->as.stmt;
}

AstStmt *ast_enum(struct Context *c, Token t, const AstEnum *e) {
    AstNode *n = ast_node(c, Node_ENUM, t);
    n->as.stmt.tag = Stmt_ENUM;
    n->as.stmt.as._enum = *e;
    return &n->as.stmt;
}

AstStmt *ast_return(Context *c, Token t, const AstReturn *r) {
    AstNode *n = ast_node(c, Node_RETURN, t);
    n->as.stmt.tag = Stmt_RETURN;
    n->as.stmt.as._return = *r;
    return &n->as.stmt;
}

AstStmt *ast_defer(Context *c, Token t, const AstDefer *d) {
    AstNode *n = ast_node(c, Node_DEFER, t);
    n->as.stmt.tag = Stmt_DEFER;
    n->as.stmt.as.defer = *d;
    return &n->as.stmt;
}

AstStmt *ast_using(struct Context *c, Token t, const AstUsing *u) {
    AstNode *n = ast_node(c, Node_USING, t);
    n->as.stmt.tag = Stmt_USING;
    n->as.stmt.as.using = *u;
    return &n->as.stmt;
}

AstExpr *ast_call_expr(Context *c, Token t, const AstCall *call) {
    AstNode *n = ast_node(c, Node_CALL, t);
    n->as.expr.tag = Expr_CALL;
    n->as.expr.as.call = *call;
    return &n->as.expr;
}

AstStmt *ast_call_stmt(Context *c, Token t, const AstCall *call) {
    AstNode *n = ast_node(c, Node_CALL, t);
    n->as.stmt.tag = Stmt_CALL;
    n->as.stmt.as.call = *call;
    return &n->as.stmt;
}
