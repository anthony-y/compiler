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

#define NODE_BLOCK_LENGTH 64

struct NodeBlock {
    AstNode data[NODE_BLOCK_LENGTH];
    u32 num_nodes;
    NodeBlock *next;
};

struct NodeAllocator {
    NodeBlock *first;
    NodeBlock *current;
    u64 num_blocks;
    u64 total_nodes;
};

bool     node_allocator_init(NodeAllocator *sa);
AstNode *node_allocator(NodeAllocator *sa);
void     node_allocator_free(NodeAllocator *sa);

void parser_init(Parser *, struct Module *);
void parse(struct Context *ctx, Parser *parser, char *path);

#endif
