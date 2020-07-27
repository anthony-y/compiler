#ifndef TOKEN_h
#define TOKEN_h

#include "common.h"

typedef enum {
    Token_EOF,
    Token_END_OF_CHUNK,
    Token_ERROR,
    Token_UNKNOWN,
    Token_COMMENT,

    Token_VALUE_START,

        Token_IDENT,

        Token_INT_LIT,
        Token_STRING_LIT,
        Token_FLOAT_LIT,
        Token_TRUE,
        Token_FALSE,
        Token_NIL,

        Token_RESERVED_TYPE,

    Token_VALUE_END,

    /* Keywords */
    Token_IMPORT,
    Token_RETURN,
    Token_IF,
    Token_ELSE,
    Token_FOR,
    Token_WHILE,
    Token_TO,
    Token_PROC,
    Token_STRUCT,
    Token_TYPEDEF,
    Token_CAST,
    Token_DEFER,
    Token_THEN,

    Token_CLOSE_PAREN,
    Token_OPEN_BRACE,
    Token_CLOSE_BRACE,
    Token_CLOSE_BRACKET,
    Token_COMMA,
    Token_COLON,
    Token_SEMI_COLON,
    Token_BANG,
    Token_AMPERSAN,
    Token_BAR,
    Token_DOT_DOT,
    Token_CARAT,

    Token_BINOP_START,

        // Postfix operators
        Token_OPEN_PAREN,
        Token_OPEN_BRACKET,
        //

        Token_ASSIGNMENTS_START,
            Token_EQUAL,
            Token_MINUS_EQUAL,
            Token_PLUS_EQUAL,
            Token_STAR_EQUAL,
            Token_SLASH_EQUAL,
        Token_ASSIGNMENTS_END,

        Token_BINARY_COMPARE_START,
            Token_AMP_AMP,
            Token_BAR_BAR,
            Token_LESS,
            Token_GREATER,
            Token_BANG_EQUAL,
            Token_GREATER_EQUAL,
            Token_LESS_EQUAL,
            Token_EQUAL_EQUAL,
        Token_BINARY_COMPARE_END,

        Token_PLUS,
        Token_MINUS,
        Token_SLASH,
        Token_STAR,

        Token_DOT,

    Token_BINOP_END,

    Token_COUNT
} TokenType;

typedef struct Token {
    TokenType type;

    u32 length;
    s64 line;
    u32 column;

    char *text;
} Token;

typedef struct TokenList {
    Token *tokens;
    u64 len;
    u64 cap;
} TokenList;

void token_list_init(TokenList *list);
void token_list_add(TokenList *list, Token token);
void token_list_free(TokenList *list);
void token_list_print(const TokenList *list);

struct Lexer;

Token token_new(struct Lexer *, TokenType);
void token_print(Token t);

#endif
