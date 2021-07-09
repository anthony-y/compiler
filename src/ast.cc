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

bool is_assignment(AstBinary b) {
    return (b.op > Token_ASSIGNMENTS_START && b.op < Token_ASSIGNMENTS_END);
}

bool is_binary_comparison(AstBinary b) {
    return (b.op > Token_BINARY_COMPARE_START && b.op < Token_BINARY_COMPARE_END);
}

bool is_literal(AstNode *n) {
    return (n->tag > Node_LITERALS_START && n->tag < Node_LITERALS_END);
}

void free_block(AstStmt *stmt) {
    if (!stmt || stmt->tag != Node_BLOCK) return;
    AstBlock *b = (AstBlock *)stmt;
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

bool node_arena_init(NodeArena *arena) {
    auto block = (NodeBlock *)malloc(sizeof(NodeBlock));
    if (!block) return false;
    *block = NodeBlock{};
    arena->first_block   = block;
    arena->current_block = block;
    return true;
}

void node_arena_free(NodeArena *arena) {
    NodeBlock *next = arena->first_block;
    while (next) {
        NodeBlock *b = next;
        next = b->next_block;
        free(b);
    }
}

AstNode *node_arena_alloc(NodeArena *arena) {
    if (arena->current_block->nodes_count >= NODEBLOCK_ARRAY_SIZE) {
        auto new_block = (NodeBlock *)malloc(sizeof(NodeBlock));
        assert(new_block);
        *new_block = NodeBlock{};
        arena->current_block->next_block = new_block;
        arena->current_block = new_block;
    }
    AstNode *ret = &arena->current_block->nodes[arena->current_block->nodes_count++];
    *ret = AstNode{};
    return ret;
}

/*
inline AstNode *ast_node(Context *c, AstNodeType tag, Token t) {
    AstNode *node = node_arena_alloc(&c->node_allocator);
    // AstNode *node = malloc(sizeof(AstNode));

    node->tag = tag;
    node->token = t;

    return node;
}
*/

AstDecl *ast_decl(Context *c, Token t, const AstDecl decl) {
    AstDecl *node = (AstDecl *)malloc(sizeof(AstDecl));
    *node = decl;
	node->tag = Node_DECL;
    return node;
}

void ast_init(Ast *list, int cap) {
    list->nodes = (AstNode **)malloc(cap * sizeof(AstNode *));
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
        AstNode **tmp = (AstNode **)realloc(list->nodes, list->cap * sizeof(AstNode *));
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

AstIdent *ast_name(Context *c, Token t) {
	auto n = (AstIdent *)malloc(sizeof(AstIdent));
	n->tag = Node_IDENT;
	n->token = t;
    n->name = make_name_from_token(c, t);
	n->resolved_decl = NULL;
    return n;
}

AstBinary *ast_binary(Context *c, Token t, const AstBinary *binary) {
    auto n = (AstBinary *)malloc(sizeof(AstBinary));
	*n = *binary;
	n->token = t;
    n->tag = Node_BINARY;
    return n;
}

AstUnary *ast_unary(Context *c, Token t, const AstUnary *unary) {
	auto n = (AstUnary *)malloc(sizeof(AstUnary));
	*n = *unary;
    n->tag = Node_UNARY;
	n->token = t;
    return n;
}

AstParen *ast_paren(Context *c, Token t, const AstParen *paren) {
	auto n = (AstParen *)malloc(sizeof(AstParen));
    *n = *paren;
    n->tag = Node_PAREN;
    n->token = t;
    return n;
}

AstCast *ast_cast(Context *c, Token t, const AstCast *cast) {
    auto n = (AstCast *)malloc(sizeof(AstCast));
    *n = *cast;
    n->tag = Node_CAST;
	n->token = t;
    return n;
}

AstArrayIndex *ast_index(Context *c, Token t, const AstArrayIndex *index) {
    auto n = (AstArrayIndex *)malloc(sizeof(AstArrayIndex));
    *n = *index;
    n->tag = Node_INDEX;
	n->token = t;
    return n;
}

AstVarArgsExpand *ast_var_args_expand(Context *c, Token t, const AstVarArgsExpand *expand) {
    auto n = (AstVarArgsExpand *)malloc(sizeof(AstVarArgsExpand));
    *n = *expand;
    n->tag = Node_VAR_ARGS_EXPAND;
	n->token = t;
    return n;
}

AstImport *ast_import(Context *c, Token t, const AstImport *imp) {
    auto n = (AstImport *)malloc(sizeof(AstImport));
    *n = *imp;
    n->tag = Node_IMPORT;
	n->token = t;
    return n;
}

AstLibrary *ast_library(Context *c, Token t, char *library, bool is_static) {
    auto n = (AstLibrary *)malloc(sizeof(AstLibrary));
    n->is_static = is_static;
    n->library = library;
    n->tag = Node_LIBRARY;
	n->token = t;
    return n;
}

AstProcedure *ast_procedure(Context *c, Token t, const AstProcedure *proc) {
    auto n = (AstProcedure *)malloc(sizeof(AstProcedure));
    *n = *proc;
    n->tag = Node_PROCEDURE;
	n->token = t;
    return n;
}

AstStruct *ast_struct(Context *c, Token t, const AstStruct *s) {
    auto n = (AstStruct *)malloc(sizeof(AstStruct));
    *n = *s;
    n->tag = Node_STRUCT;
	n->token = t;
    return n;
}

AstCall *ast_call_expr(Context *c, Token t, const AstCall *call) {
    auto n = (AstCall *)malloc(sizeof(AstCall));
    *n = *call;
    n->tag = Node_CALL;
    n->token = t;
    return n;
}

AstCallStmt *ast_call_stmt(Context *c, Token t, AstCall *expr) {
    auto n = (AstCallStmt *)malloc(sizeof(AstCallStmt));
    n->expr = expr;
    n->tag = Node_CALL;
    n->token = t;
    return n;
}

AstAssignment *ast_assignment(Context *c, Token t, AstExpr *ass) {
    auto n = (AstAssignment *)malloc(sizeof(AstAssignment));
    n->expr = ass;
    n->tag = Node_ASSIGN;
    n->token = t;
    return n;
}

AstBlock *ast_block(Context *c, Token t, const AstBlock *blk) {
    auto n = (AstBlock *)malloc(sizeof(AstBlock));
    *n = *blk;
    n->tag = Node_BLOCK;
    n->token = t;
    return n;
}

AstIf *ast_if(Context *c, Token t, const AstIf *i) {
    auto n = (AstIf *)malloc(sizeof(AstIf));
    *n = *i;
    n->tag = Node_IF;
    n->token = t;
    return n;
}

AstWhile *ast_while(Context *c, Token t, const AstWhile *w) {
    auto n = (AstWhile *)malloc(sizeof(AstWhile));
    *n = *w;
    n->tag = Node_WHILE;
    n->token = t;
    return n;
}

AstEnum *ast_enum(Context *c, Token t, const AstEnum *e) {
    auto n = (AstEnum *)malloc(sizeof(AstEnum));
    *n = *e;
    n->tag = Node_ENUM;
    n->token = t;
    return n;
}

AstReturn *ast_return(Context *c, Token t, const AstReturn *r) {
    auto n = (AstReturn *)malloc(sizeof(AstReturn));
    *n = *r;
    n->tag = Node_RETURN;
    n->token = t;
    return n;
}

AstDefer *ast_defer(Context *c, Token t, const AstDefer *d) {
    auto n = (AstDefer *)malloc(sizeof(AstDefer));
    *n = *d;
    n->tag = Node_DEFER;
    n->token = t;
    return n;
}

AstUsing *ast_using(Context *c, Token t, const AstUsing *u) {
    auto n = (AstUsing *)malloc(sizeof(AstUsing));
    *n = *u;
    n->tag = Node_USING;
    n->token = t;
    return n;
}
