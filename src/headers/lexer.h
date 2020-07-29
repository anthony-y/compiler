#ifndef LEXER_h
#define LEXER_h

#include "token.h"
#include "arena.h"

//#include <pthread.h>

#if 0
typedef struct Lexer {
    pthread_t id;
    const char *file_name;

    char *start; // Start of current token
    char *curr;
    char *end; // End of the chunks buffer

    u64 line;
    u32 column;

    TokenType last; // Type of the last lexed token.

    Pool string_allocator;
    TokenList output;
} Lexer;
#endif

typedef struct Lexer {
    const char *file_name;

    char *start;
    char *curr;
    
    u64 line;
    u32 column;

    TokenType last;
    Arena string_allocator;
} Lexer;

//void lexer_init(Lexer *, char *start, u64 line);
void lexer_init(Lexer *, const char *path, char *data);
void lexer_free(Lexer *);

struct SourceStats;

struct Context;
bool lexer_lex(struct Context *, TokenList *list, struct SourceStats *);
///void *lexer_lex(void *);

#endif
