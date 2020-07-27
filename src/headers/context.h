#ifndef CONTEXT_h
#define CONTEXT_h

#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "type.h"
#include "common.h"
#include "token.h"
#include "arena.h"

#include <stdarg.h>

#define CONTEXT_SCRATCH_SIZE 1024

typedef struct Context {
    const char *current_file_path;

    int error_count;

    Lexer lexer;
    Parser parser;
    Arena scratch;

    AstProcedure *curr_checker_proc;
    Token *deferred_names; // stb stretchy buffer

    // stb hash tables
    struct {char *key; AstNode *value;} *symbol_table;
    struct {char *key; Type    *value;} *type_table;

    /* Handles to types in the type table
       for easy comparison in type-checking, etc. */
    Type *type_int, *type_s64, *type_u64, *type_u32, *type_s32,
        *type_u16, *type_s16, *type_u8, *type_s8;

    Type *type_string, *type_void, *type_bool; // non-integer types
    Type *decoy_ptr; // for comparison and printing in errors
    Type *error_type; // returned during type/semantic checking in does_type_describe_expr if an error occurred.
} Context;

typedef struct SourceStats {
    // These values are estimates from lexical analysis
    u64 declared_types;
    u64 pointer_types;
    u64 argument_lists;
    u64 blocks;
    u64 structs;
} SourceStats;

void compile_error(Context *ctx, Token t, const char *fmt, ...);
void compile_error_start(Context *, Token, const char *fmt, ...);
void compile_error_add_line(Context *ctx, const char *fmt, ...);
void compile_error_end();

void init_context(Context *c, const char *file_path);

// From types.c, couldn't declare in types.h because of a circular dependency with context.h
void init_types(Context *, SourceStats *);
void free_types(Context *);

bool check_types_were_declared(Context *);

void add_symbol(Context *, AstNode *, char *name);
AstNode *lookup_local(AstNode *block, char *name);

#endif
