// The lexer transforms a chunk of bytes
// (typically a loaded file) into an array
// of Tokens, ready to be handed off to the parser.
#include "headers/lexer.h"
#include "headers/token.h"
#include "headers/context.h"
#include "headers/table.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/* Initializes a Lexer */
void lexer_init(Lexer *tz, const char *path, char *data) {
    tz->file_name = path;

    tz->line = 1;
    tz->column = 1;

    tz->last = Token_EOF;

    tz->curr = data;
    tz->start = data;
}

void lexer_free(Lexer *tz) {
    // arena_free(tz->string_allocator);
    tz->curr = NULL;
    tz->line = 0;
}

/*
    Helpers
*/
static inline bool is_end(Lexer *tz) {
    return *tz->curr == '\0';
}

static inline char next_character(Lexer *tz) {
    if (*tz->curr == '\n') {
        tz->line++;
        tz->column = 1;

    } else tz->column++;

    return *(tz->curr++);
}

static inline bool is_whitespace(char what) {
    return (what == ' '     ||
            what == '\t'    ||
            what == '\v'    ||
            what == '\f'    ||
            what == '\r');
}

static inline bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static inline bool is_numeric(char c) {
    return (c >= '0' && c <= '9');
}

static inline bool is_alpha_numeric(char c) {
    return ( is_alpha(c) ) || ( is_numeric(c) );
}

/* Creates a token with the given type */
Token token_new(Lexer *tz, TokenType type) {
    Token t;

    t.type   = type;
    t.line   = tz->line;
    t.column = tz->column;
    t.length = (s32)(tz->curr - tz->start);
    t.text   = "";
    t.file = tz->file_name;
    
    tz->last = type;

    if (type == Token_IDENT || type == Token_INT_LIT || type == Token_STRING_LIT || type == Token_FLOAT_LIT || type == Token_RESERVED_TYPE) {
        if (type == Token_STRING_LIT) {
            t.text = arena_alloc(tz->string_allocator, t.length-1);
            strncpy(t.text, tz->start+1, t.length-2);
            t.text[t.length-2] = 0;
            return t;
        }
        t.text = arena_alloc(tz->string_allocator, t.length + 1);
        strncpy(t.text, tz->start, t.length);
        t.text[t.length] = 0;
    }

    return t;
}

/*
Util functions
**************
*/
static bool next_if_match(Lexer *tz, char c) {
    if (*tz->curr != c || is_end(tz)) return false;

    tz->curr++;
    return true;
}

static inline void skip_whitespace(Lexer *tz) {
    /* Skip blank characters */
    while (is_whitespace(*tz->curr) && !is_end(tz))
        next_character(tz);

    /* Skip comments */
    if (*tz->curr == '/' && tz->curr[1] == '/') {
        tz->curr += 2;
        while (*tz->curr != '\n' && !is_end(tz))
            next_character(tz);
    }
}

static Token tokenize_string(Lexer *tz) {
     u64 start_line = tz->line;
    while (*tz->curr != '"' && !is_end(tz)) {
        if (*tz->curr == '\n') {
            fprintf(stderr, "%s:%lu: Error: unterminated string.\n", tz->file_name, start_line);
            return token_new(tz, Token_ERROR);
        }
        next_character(tz);
    }
    next_character(tz);
    return token_new(tz, Token_STRING_LIT);
}

static Token tokenize_number(Lexer *tz) {
    TokenType t = Token_INT_LIT;

    while (true) {
        // This is just to reduce line length on the if
        bool first = (is_numeric(*tz->curr) || *tz->curr == '.');
        bool second = (*tz->curr != '\n' && !is_end(tz));

        if (!(first && second)) break;

        if (*tz->curr == '.') t = Token_FLOAT_LIT;

        next_character(tz);
    }

    return token_new(tz, t);
}

static Token tokenize_ident_or_keyword(Lexer *tz) {
    while ((is_alpha_numeric(*tz->curr) || *tz->curr == '_')
        && (!is_end(tz) && *tz->curr != '\n')) {
        
        next_character(tz);
    }

    if (*tz->start == 'u' || *tz->start == 's') {
        if (strncmp(tz->start+1, "8", 1) == 0 ||
        strncmp(tz->start+1, "16", 2)    == 0 ||
        strncmp(tz->start+1, "32", 2)    == 0 ||
        strncmp(tz->start+1, "64", 2)    == 0
        ) {
            return token_new(tz, Token_RESERVED_TYPE);
        }
    }

    u64 len = (u64)(tz->curr-tz->start);

    switch (*tz->start) {
    case 'a': {
        if (len == 3 && strncmp(tz->start+1, "ny", 2) == 0) return token_new(tz, Token_RESERVED_TYPE);
    } break;
    case 'c': {
        if (len == 4 && strncmp(tz->start+1, "ast", 3) == 0) return token_new(tz, Token_CAST);
        if (len == 5 && strncmp(tz->start+1, "onst", 4) == 0) return token_new(tz, Token_CONST);
    } break;
    case 'i': {
        if (len == 3 && strncmp(tz->start+1, "nt", 2) == 0) return token_new(tz, Token_RESERVED_TYPE);
        if (len == 2 && strncmp(tz->start+1, "f", 1) == 0) return token_new(tz, Token_IF);
        if (len != 6) break;
        if (strncmp(tz->start+1, "nline", 5) == 0) return token_new(tz, Token_INLINE);
        if (strncmp(tz->start+1, "mport", 5) == 0) return token_new(tz, Token_IMPORT);
    } break;

    case 'f': {
        if (len == 5 && strncmp(tz->start+1, "loat", 4) == 0) return token_new(tz, Token_RESERVED_TYPE);
        if (len == 5 && strncmp(tz->start+1, "alse", 4) == 0) return token_new(tz, Token_FALSE);
        if (len == 3 && strncmp(tz->start+1, "or", 2) == 0) return token_new(tz, Token_FALSE);
    } break;

    case 'u': {
        if (len == 5 && strncmp(tz->start+1, "sing", 4) == 0) return token_new(tz, Token_USING);
    } break;

    case 's': {
        if (tz->start[1] == 't') {
            if (len == 6 && strncmp(tz->start+2, "ring", 4) == 0) return token_new(tz, Token_RESERVED_TYPE);
            if (len == 6 && strncmp(tz->start+2, "ruct", 4) == 0) return token_new(tz, Token_STRUCT);
        }
    } break;

    case 't': {
        if (len != 4) break;
        if (strncmp(tz->start+1, "rue", 3) == 0) return token_new(tz, Token_TRUE);
        if (strncmp(tz->start+1, "hen", 3) == 0) return token_new(tz, Token_THEN);
        if (strncmp(tz->start+1, "ype", 3) == 0) return token_new(tz, Token_TYPEDEF);
    } break;

    case 'v': {
        if (len == 4 && strncmp(tz->start+1, "oid", 3) == 0) return token_new(tz, Token_RESERVED_TYPE);
    } break;

    case 'b': {
        if (len == 4 && strncmp(tz->start+1, "ool", 3) == 0) return token_new(tz, Token_RESERVED_TYPE);
    } break;

    case 'r': {
        if (len == 6 && strncmp(tz->start+1, "eturn", 5) == 0) return token_new(tz, Token_RETURN);
    } break;

    case 'p': {
        if (len == 4 && strncmp(tz->start+1, "roc", 3) == 0) return token_new(tz, Token_PROC);
    } break;

    case 'e': {
        if (len != 4) break;
        if (strncmp(tz->start+1, "lse", 3) == 0) return token_new(tz, Token_ELSE);
        if (strncmp(tz->start+1, "num", 3) == 0) return token_new(tz, Token_ENUM);
    } break;

    case 'n': {
        if (len == 4 && strncmp(tz->start+1, "ull", 3) == 0) return token_new(tz, Token_NIL);
    } break;

    case 'w': {
        if (len == 5 && strncmp(tz->start+1, "hile", 4) == 0) return token_new(tz, Token_WHILE);
    } break;

    case 'd': {
        if (len == 5 && strncmp(tz->start+1, "efer", 4) == 0) return token_new(tz, Token_DEFER);
    } break;
    }

    return token_new(tz, Token_IDENT);
}

Token next_token(Lexer *tz) {
    skip_whitespace(tz);

    tz->start = tz->curr;

    // next_character returns the current character and then increments the pointer.
    char c = next_character(tz);

    if (c == '\n') {
        // Automatic semi-colon insertion based on the rules described here:
        //    https://medium.com/golangspec/automatic-semicolon-insertion-in-go-1990338f2649
        switch (tz->last) {
        case Token_SEMI_COLON: return next_token(tz);

        case Token_RESERVED_TYPE:
        case Token_CLOSE_PAREN:
        case Token_IDENT:
            // In these cases, if we were to insert a semi-colon, it would cause issues
            // for scope delimeters. For example:
            //     proc example(): int[;] <--- semi-colon would be inserted here and would give an error
            //     {
            //         return 10
            //     }
            //
            if (*tz->curr == '{') return next_token(tz);

        case Token_STRING_LIT:
        case Token_FLOAT_LIT:
        case Token_TRUE:
        case Token_FALSE:
        case Token_NIL:
        case Token_INT_LIT:

        case Token_RETURN:

        case Token_CLOSE_BRACE:
        case Token_CLOSE_BRACKET: {
            // The above call to next_character incremented the line already
            // but if there was a real semi-colon it would of course be on the same line.
            Token tmp = token_new(tz, Token_SEMI_COLON);
            tmp.line--;
            return tmp;
        }
        
        // We increment the lexers line variable in next_character so that it happens inside of
        // specific lexing functions (e.g skip_whitespace) and not just at the root level.
        //
        // next_character doesn't skip over new lines though so we still might encounter it when
        // looking for the next token, however producing tokens from new lines creates verbosity
        // later on in the parser, so we just recurse until we get a token that isn't a newline.
        default:
            return next_token(tz);
        }
    }

    if (c == '\0') return token_new(tz, Token_EOF);

    if (is_numeric(c))           return tokenize_number(tz);
    if (is_alpha(c) || c == '_') return tokenize_ident_or_keyword(tz);

    switch(c) {
        case '(': return token_new(tz, Token_OPEN_PAREN);
        case ')': return token_new(tz, Token_CLOSE_PAREN);
        case '{': return token_new(tz, Token_OPEN_BRACE);
        case '}': return token_new(tz, Token_CLOSE_BRACE);
        case '[': return token_new(tz, Token_OPEN_BRACKET);
        case ']': return token_new(tz, Token_CLOSE_BRACKET);
        case ',': return token_new(tz, Token_COMMA);
        case ':': return token_new(tz, Token_COLON);
        case ';': return token_new(tz, Token_SEMI_COLON);
        case '^': return token_new(tz, Token_CARAT);
        case '#': return token_new(tz, Token_HASH);

        // Binary operators: +, *=, <, /=, !=, ==, etc.
        case '*': return token_new(tz, next_if_match(tz, '=') ? Token_STAR_EQUAL    : Token_STAR);
        case '/': return token_new(tz, next_if_match(tz, '=') ? Token_SLASH_EQUAL   : Token_SLASH);
        case '!': return token_new(tz, next_if_match(tz, '=') ? Token_BANG_EQUAL    : Token_BANG);
        case '=': return token_new(tz, next_if_match(tz, '=') ? Token_EQUAL_EQUAL   : Token_EQUAL);
        case '+': return token_new(tz, next_if_match(tz, '=') ? Token_PLUS_EQUAL    : Token_PLUS);
        case '>': return token_new(tz, next_if_match(tz, '=') ? Token_GREATER_EQUAL : Token_GREATER);
        case '<': return token_new(tz, next_if_match(tz, '=') ? Token_LESS_EQUAL    : Token_LESS);
        case '-': return token_new(tz, next_if_match(tz, '=') ? Token_MINUS_EQUAL   : Token_MINUS);

        case '&': return token_new(tz, next_if_match(tz, '&') ? Token_AMP_AMP       : Token_AMPERSAN);
        case '|': return token_new(tz, next_if_match(tz, '|') ? Token_BAR_BAR       : Token_BAR);

        case '.': return token_new(tz, next_if_match(tz, '.') ? Token_DOT_DOT       : Token_DOT);

        case '"': return tokenize_string(tz);
    }

    fprintf(stderr, "%s:%lu: Error: unknown character '%c'\n", tz->file_name, tz->line, c);
    return token_new(tz, Token_UNKNOWN);
}

bool lexer_lex(Lexer *l, TokenList *out) {
    assert(out);
    TokenList list;
    token_list_init(&list);

    assert(l->string_allocator);
    while (true) { 
        Token t = next_token(l);
        token_list_add(&list, t);
        if (t.type == Token_EOF) break;
    }
    *out = list;
    return true;
}
