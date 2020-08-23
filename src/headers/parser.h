#ifndef PARSER_h
#define PARSER_h

#include "common.h"
#include "arena.h"
#include "ast.h"
#include "token.h"

typedef struct Parser {
    Token *curr;
    Token *prev;

    Arena node_allocator;
    Arena tree_allocator;

    AstBlock *current_scope;

    u64 node_count;
} Parser;

struct SourceStats;

void parser_init(Parser *, const TokenList *, const struct SourceStats *);
void parser_free(Parser *, Ast *);
void parser_recover(Parser *p, TokenType tt);

#endif
