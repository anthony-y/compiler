#ifndef PARSER_h
#define PARSER_h

#include "common.h"
#include "arena.h"
#include "ast.h"
#include "token.h"

typedef struct Parser {
    Token *curr;
    Token *prev;
    AstBlock *current_scope;
    Ast *ast;
} Parser;

void parser_init(Parser *, const TokenList *);
void parser_free(Parser *, Ast *);

#endif
