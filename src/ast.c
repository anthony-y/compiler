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
