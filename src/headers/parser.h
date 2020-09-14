#ifndef PARSER_h
#define PARSER_h

#include "common.h"
#include "arena.h"
#include "ast.h"
#include "token.h"

typedef struct Parser {
    Token *curr;
    Token *prev;

    Arena tree_allocator;

    AstBlock *current_scope;
} Parser;

struct SourceStats;
struct Context;

void parser_init(Parser *, const TokenList *, const struct SourceStats *);
void parser_free(Parser *, Ast *);
Ast parse(struct Context *, Parser *);

#endif
