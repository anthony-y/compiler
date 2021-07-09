#ifndef PARSER_h
#define PARSER_h

#include "common.h"
#include "arena.h"
#include "ast.h"
#include "token.h"

struct Module;
struct Context;

typedef struct Parser {
    Token *curr;
    Token *prev;
    AstBlock *current_scope;
    Module *module;
    bool in_type_instantiation = false; // TODO better name, it's a thing that tells us whether we're in a pointer or array typename thingy
} Parser;

void parser_init(Parser *, struct Module *);
void parse(struct Context *ctx, Parser *parser, char *path);

#endif
