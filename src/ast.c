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

void free_block(AstStmt *stmt) {
    if (!stmt || stmt->tag != Stmt_BLOCK) return;
    AstBlock *b = (AstBlock *)stmt;

    u64 sym_len = shlenu(b->symbols);
    for (int i = 0; i < sym_len; i++) {
        AstDecl *d = b->symbols[i].value;
        if (d->tag == Decl_VAR) {
            AstVar *var = (AstVar *)d;
            if (var->typename->as.type->kind == Type_ANON_STRUCT) {
                free_block(((AstStruct *)var->typename->as.type->data.user)->members);
            }
        }
    }

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
        case Node_CALL: {
            AstCall *call = (AstCall *)n;
            ast_free(call->params);
        } break;
        }
    }

    ast_free(b->statements);
    shfree(b->symbols);
    b->parent = NULL;
}

void free_subtrees_and_blocks(Ast *ast) {
    for (int i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        switch (node->tag) {
        case Node_PROCEDURE: {
            AstProcedure *proc = (AstProcedure *)node;
            shfree(proc->params);
            free_block(proc->block);
        } break;
        }
    }
}

inline AstNode *ast_node(Parser *p, AstNodeType tag, Token t) {
    AstNode *node = arena_alloc(&p->node_allocator, sizeof(AstNode));

    node->tag = tag;
    node->token = t;

    p->node_count++;

    return node;
}

AstNode *ast_decl(Parser *p, AstNodeType tag, Token t, const AstDecl *decl) {
    AstNode *node = ast_node(p, tag, t);
    node->as.decl = *decl;
    return node;
}

AstNode *ast_expr(Parser *p, AstNodeType tag, Token t, const AstExpr *expr) {
    AstNode *node = ast_node(p, tag, t);
    node->as.expr = *expr;
    return node;
}

AstNode *ast_stmt(Parser *p, AstNodeType tag, Token t, const AstStmt *stmt) {
    AstNode *node = ast_node(p, tag, t);
    node->as.stmt = *stmt;
    return node;
}

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

/* These functions allocate and return AST nodes */
/* --------------------------------------------- */
void ast_init(Ast *list, int cap) {
    list->nodes = malloc(cap * sizeof(AstNode *));
    list->cap   = cap;
    list->len   = 0;
}

void ast_free(Ast *list) {
    assert(list->nodes);
    free(list->nodes);
    list->cap = 0;
    list->len = 0;
}

void ast_add(Ast *list, AstNode *node) {
    if (list->cap < list->len) {
        list->cap *= 2;
        AstNode **tmp = realloc(list->nodes, list->cap * sizeof(AstNode *));
        if (tmp) { list->nodes = tmp;
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
    AstNode *n = ast_node(&c->parser, Node_IDENT, t);
    n->as.expr.tag = Expr_NAME;
    n->as.expr.as.name = make_name(c, t);
    return &n->as.expr;
}

AstExpr *ast_binary(Parser *p, Token t, const AstBinary *binary) {
    AstNode *n = ast_node(p, Node_BINARY, t);
    n->as.expr.tag = Expr_BINARY;
    n->as.expr.as.binary = *binary;
    return &n->as.expr;
}

AstExpr *ast_unary(Parser *p, Token t, const AstUnary *unary) {
    AstNode *n = ast_node(p, Node_UNARY, t);
    n->as.expr.tag = Expr_UNARY;
    n->as.expr.as.unary = *unary;
    return &n->as.expr;
}

AstExpr *ast_selector(Parser *p, Token t, const AstSelector *sel) {
    AstNode *n = ast_node(p, Node_SELECTOR, t);
    n->as.expr.tag = Expr_SELECTOR;
    n->as.expr.as.selector = *sel;
    return &n->as.expr;
}

AstExpr *ast_paren(Parser *p, Token t, const AstParen *paren) {
    AstNode *n = ast_node(p, Node_PAREN, t);
    n->as.expr.tag = Expr_PAREN;
    n->as.expr.as.paren = *paren;
    return &n->as.expr;
}

AstExpr *ast_cast(Parser *p, Token t, const AstCast *cast) {
    AstNode *n = ast_node(p, Node_CAST, t);
    n->as.expr.tag = Expr_CAST;
    n->as.expr.as.cast = *cast;
    return &n->as.expr;
}

AstExpr *ast_index(Parser *p, Token t, const AstArrayIndex *index) {
    AstNode *n = ast_node(p, Node_INDEX, t);
    n->as.expr.tag = Expr_INDEX;
    n->as.expr.as.index = *index;
    return &n->as.expr;
}

AstDecl *ast_proc(Parser *p, Token t, Name *name, const AstProcedure *proc) {
    AstNode *n = ast_node(p, Node_PROCEDURE, t);
    n->as.decl.flags = 0;
    n->as.decl.status = Status_UNRESOLVED;
    n->as.decl.tag = Decl_PROC;
    n->as.decl.name = name;
    n->as.decl.as.proc = *proc;
    return &n->as.decl;
}

AstDecl *ast_var(Parser *p, Token t, Name *name, const AstVar *var) {
    AstNode *n = ast_node(p, Node_VAR, t);
    n->as.decl.flags = 0;
    n->as.decl.status = Status_UNRESOLVED;
    n->as.decl.tag = Decl_VAR;
    n->as.decl.name = name;
    n->as.decl.as.var = *var;
    return &n->as.decl;
}

AstDecl *ast_typedefi(Parser *p, Token t, Name *name, const AstTypedef *td) {
    AstNode *n = ast_node(p, Node_TYPEDEF, t);
    n->as.decl.flags = 0;
    n->as.decl.status = Status_UNRESOLVED;
    n->as.decl.tag = Decl_TYPEDEF;
    n->as.decl.name = name;
    n->as.decl.as.typedefi = *td;
    return &n->as.decl;
}

AstStmt *ast_assignment(Parser *p, Token t, const AstExpr *ass) {
    AstNode *n = ast_node(p, Node_ASSIGN, t);
    n->as.stmt.tag = Stmt_ASSIGN;
    n->as.stmt.as.assign = *ass;
    return &n->as.stmt;
}

AstStmt *ast_import(Parser *p, Token t, const AstImport *imp) {
    AstNode *n = ast_node(p, Node_IMPORT, t);
    n->as.stmt.tag = Stmt_IMPORT;
    n->as.stmt.as._import = *imp;
    return &n->as.stmt;
}

AstStmt *ast_block(Parser *p, Token t, const AstBlock *blk) {
    AstNode *n = ast_node(p, Node_BLOCK, t);
    n->as.stmt.tag = Stmt_BLOCK;
    n->as.stmt.as.block = *blk;
    return &n->as.stmt;
}

AstStmt *ast_if(Parser *p, Token t, const AstIf *i) {
    AstNode *n = ast_node(p, Node_IF, t);
    n->as.stmt.tag = Stmt_IF;
    n->as.stmt.as._if = *i;
    return &n->as.stmt;
}

AstStmt *ast_while(Parser *p, Token t, const AstWhile *w) {
    AstNode *n = ast_node(p, Node_WHILE, t);
    n->as.stmt.tag = Stmt_WHILE;
    n->as.stmt.as._while = *w;
    return &n->as.stmt;
}

AstStmt *ast_struct(Parser *p, Token t, const AstStruct *s) {
    AstNode *n = ast_node(p, Node_STRUCT, t);
    n->as.stmt.tag = Stmt_STRUCT;
    n->as.stmt.as._struct = *s;
    return &n->as.stmt;
}

AstStmt *ast_return(Parser *p, Token t, const AstReturn *r) {
    AstNode *n = ast_node(p, Node_RETURN, t);
    n->as.stmt.tag = Stmt_RETURN;
    n->as.stmt.as._return = *r;
    return &n->as.stmt;
}

AstStmt *ast_defer(Parser *p, Token t, const AstDefer *d) {
    AstNode *n = ast_node(p, Node_DEFER, t);
    n->as.stmt.tag = Stmt_DEFER;
    n->as.stmt.as.defer = *d;
    return &n->as.stmt;
}

AstExpr *ast_call_expr(Parser *p, Token t, const AstCall *call) {
    AstNode *n = ast_node(p, Node_CALL, t);
    n->as.expr.tag = Expr_CALL;
    n->as.expr.as.call = *call;
    return &n->as.expr;
}

AstStmt *ast_call_stmt(Parser *p, Token t, const AstCall *call) {
    AstNode *n = ast_node(p, Node_CALL, t);
    n->as.stmt.tag = Stmt_CALL;
    n->as.stmt.as.call = *call;
    return &n->as.stmt;
}
