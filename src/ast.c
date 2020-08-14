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

inline AstNode *ast_node(Parser *p, AstNodeType tag, Token t) {
    AstNode *node = arena_alloc(&p->node_allocator, sizeof(AstNode));

    node->tag = tag;
    node->token = t;

    p->node_count++;

    return node;
}

 // TODO should AstDecl and AstExpr, etc. have their own enums for tags?
 // I will have to see how I end up using the nodes in the checker

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

AstNode *make_error_node(Parser *p, Token tok, const char *msg) {
    AstNode *n = malloc(sizeof(AstNode));
    n->token = tok;
    n->tag = Node_ERROR;
    n->as.error.line = tok.line;
    n->as.error.column = tok.column;
    n->as.error.msg = arena_alloc(&p->error_msg_allocator, strlen(msg)+1);
    strcpy(n->as.error.msg, msg);

    p->error_count++;

    //printf("Error: line %lu in %s: %s\n", tok.line, NULL, n->error.msg);

    return n;
}

AstNode *make_ident_node(struct Context *c, Token tok) {
    AstNode *n = ast_node(&c->parser, Node_IDENT, tok);
    n->as.ident = make_name(c, tok);
    return n;
}

/*
void change_error_message(Parser *p, AstNode *node, const char *msg) {
    assert(node->tag == Node_ERROR);
    node->error.msg = arena_alloc(&p->error_msg_allocator, strlen(msg));
    strcpy(node->error.msg, msg);

    printf("Error: %s on line %lu: %s\n", NULL, node->error.line, node->error.msg);
}
*/

inline bool is_assignment(AstBinary b) {
    return (b.op > Token_ASSIGNMENTS_START && b.op < Token_ASSIGNMENTS_END);
}

inline bool is_binary_comparison(AstBinary b) {
    return (b.op > Token_BINARY_COMPARE_START && b.op < Token_BINARY_COMPARE_END);
}

Name *get_decl_name(AstNode *node) {
    // Not actually a decl
    if (!is_decl(node)) return NULL;
    Name *name = NULL;
    if (node->tag == Node_VAR) name = ((AstVar *)node)->name->as.ident;
    if (node->tag == Node_PROCEDURE) name = ((AstProcedure *)node)->name->as.ident;
    if (node->tag == Node_TYPEDEF) name = ((AstTypedef *)node)->name->as.ident;
    assert(name);
    return name;
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

AstNode *ast_name(Parser *p, Token t, Name *name) {
    AstNode *n = ast_node(p, Node_IDENT, t);
    n->as.ident = name;
    return n;
}

AstNode *ast_binary(Parser *p, Token t, const AstBinary *binary) {
    AstNode *n = ast_node(p, Node_BINARY, t);
    n->as.expr.as.binary = *binary;
    return n;
}

AstNode *ast_unary(Parser *p, Token t, const AstUnary *unary) {
    AstNode *n = ast_node(p, Node_UNARY, t);
    n->as.expr.as.unary = *unary;
    return n;
}

AstNode *ast_selector(Parser *p, Token t, const AstSelector *sel) {
    AstNode *n = ast_node(p, Node_SELECTOR, t);
    n->as.expr.as.selector = *sel;
    return n;
}

AstNode *ast_paren(Parser *p, Token t, const AstParen *paren) {
    AstNode *n = ast_node(p, Node_PAREN, t);
    n->as.expr.as.paren = *paren;
    return n;
}

AstNode *ast_cast(Parser *p, Token t, const AstCast *cast) {
    AstNode *n = ast_node(p, Node_CAST, t);
    n->as.expr.as.cast = *cast;
    return n;
}

AstNode *ast_call_expr(Parser *p, Token t, const AstCall *call) {
    AstNode *n = ast_node(p, Node_CALL, t);
    n->as.expr.as.call = *call;
    return n;
}

AstNode *ast_index(Parser *p, Token t, const AstArrayIndex *index) {
    AstNode *n = ast_node(p, Node_INDEX, t);
    n->as.expr.as.index = *index;
    return n;
}

AstNode *ast_proc(Parser *p, Token t, Name *name, const AstProcedure *proc) {
    AstNode *n = ast_node(p, Node_PROCEDURE, t);
    n->as.decl.name = name;
    n->as.decl.as.proc = *proc;
    return n;
}

AstNode *ast_var(Parser *p, Token t, Name *name, const AstVar *var) {
    AstNode *n = ast_node(p, Node_VAR, t);
    n->as.decl.name = name;
    n->as.decl.as.var = *var;
    return n;
}

AstNode *ast_typedefi(Parser *p, Token t, Name *name, const AstTypedef *td) {
    AstNode *n = ast_node(p, Node_TYPEDEF, t);
    n->as.decl.name = name;
    n->as.decl.as.typedefi = *td;
    return n;
}

AstNode *ast_assignment(Parser *p, Token t, const AstBinary *ass) {
    AstNode *n = ast_node(p, Node_BINARY, t);
    n->as.stmt.as.binary = *ass;
    return n;
}

AstNode *ast_import(Parser *p, Token t, const AstImport *imp) {
    AstNode *n = ast_node(p, Node_IMPORT, t);
    n->as.stmt.as._import = *imp;
    return n;
}

AstNode *ast_block(Parser *p, Token t, const AstBlock *blk) {
    AstNode *n = ast_node(p, Node_BLOCK, t);
    n->as.stmt.as.block = *blk;
    return n;
}

AstNode *ast_if(Parser *p, Token t, const AstIf *i) {
    AstNode *n = ast_node(p, Node_IF, t);
    n->as.stmt.as._if = *i;
    return n;
}

AstNode *ast_while(Parser *p, Token t, const AstWhile *w) {
    AstNode *n = ast_node(p, Node_WHILE, t);
    n->as.stmt.as._while = *w;
    return n;
}

AstNode *ast_struct(Parser *p, Token t, const AstStruct *s) {
    AstNode *n = ast_node(p, Node_STRUCT, t);
    n->as.stmt.as._struct = *s;
    return n;
}

AstNode *ast_return(Parser *p, Token t, const AstReturn *r) {
    AstNode *n = ast_node(p, Node_RETURN, t);
    n->as.stmt.as._return = *r;
    return n;
}

AstNode *ast_defer(Parser *p, Token t, const AstDefer *d) {
    AstNode *n = ast_node(p, Node_DEFER, t);
    n->as.stmt.as.defer = *d;
    return n;
}

AstNode *ast_call(Parser *p, Token t, const AstCall *call) {
    AstNode *n = ast_node(p, Node_CALL, t);
    n->as.call = *call;
    return n;
}
