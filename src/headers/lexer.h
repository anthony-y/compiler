#ifndef LEXER_h
#define LEXER_h

#include "token.h"
#include "arena.h"
#include "table.h"

typedef struct Lexer {
    const char *file_name;

    char *start;
    char *curr;
    
    u64 line;
    u32 column;

    TokenType last;
    Arena *string_allocator; // pointer to the string allocator in Context
} Lexer;

void lexer_init(Lexer *, const char *path, char *data);
void lexer_free(Lexer *);

bool lexer_lex(Lexer *, struct TokenList *list);
Token next_token(Lexer *);

#if 0
#include <pthread.h>
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
void lexer_init(Lexer *, char *start, u64 line);
#endif

#endif
