// Utility functions for working with the Token,
// the first major abstraction in the compilation pipeline.
#include "headers/token.h"
#include "headers/common.h"

#include <stdlib.h>
#include <stdio.h>

void token_list_init(TokenList *list) {
    list->len = 0;
    list->cap = 64;
    list->tokens = malloc(sizeof(Token) * 64);
}

void token_list_add(TokenList *list, Token token) {
    if (list->len >= list->cap) {
        list->cap *= 2;
        Token *tmp = realloc(list->tokens, list->cap * sizeof(Token));
        if (tmp) {
            list->tokens = tmp;
        } else {
            fprintf(stderr, "Error: out of memory.\n");
            free(list->tokens);
            exit(1);
        }
    }

    list->tokens[list->len++] = token;
}

void token_list_free(TokenList *list) {
    free(list->tokens);
    list->cap = 0;
    list->len = 0;
}

inline void token_list_print(const TokenList *list) {
    for (int i = 0; i < list->len; i++) token_print(list->tokens[i]);
}

void token_print(Token t) {
    static u64 c = 1;

    printf("%lu. ", c++);
    if (t.type == Token_SEMI_COLON)
        printf("; (%d) on line %lu\n", t.type, t.line);
    else
        printf("%s (%d) on line %lu\n", t.text, t.type, t.line);
}
