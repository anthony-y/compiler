#ifndef CONTEXT_h
#define CONTEXT_h

#include "lexer.h"
#include "parser.h"
#include "type.h"
#include "common.h"
#include "token.h"
#include "arena.h"
#include "ast.h"

#include <stdarg.h>

#define CONTEXT_SCRATCH_SIZE 1024

struct AstNode;

#if 1
typedef struct Symbol {
    struct AstNode *decl;
    enum {
        Sym_UNRESOLVED,
        Sym_RESOLVED,
        Sym_RESOLVING,
    } status;
} Symbol;
#endif

// An entry in the name table.
typedef struct Name {
    char *text;
} Name;

typedef struct SymbolTable {char *key; Symbol value;} SymbolTable;

// Central compiler context, a reference to an instance of this struct is
// passed to most functions in the compiler.
typedef struct Context {
    const char *current_file_path;

    int error_count;

    Lexer  lexer;
    Parser parser;
    Arena  scratch;

    AstProcedure *curr_checker_proc;

    Token *deferred_names; // stb stretchy buffer

    // stb hash tables
                       SymbolTable      *symbol_table;
    struct {char *key; Type    *value;} *type_table;
    struct {char *key; Name    *value;} *name_table;

    /* Handles to types in the type table
       for easy comparison in type-checking, etc. */
    Type *type_int, *type_s64, *type_u64, *type_u32, *type_s32,
        *type_u16, *type_s16, *type_u8, *type_s8;

    Type *type_string, *type_void, *type_bool; // non-integer types
    Type *decoy_ptr; // for comparison and printing in errors
    Type *error_type; // returned during type/semantic checking in does_type_describe_expr if an error occurred.
} Context;

// Contains statistics about the source code
// which can be used to compute allocation sizes.
// Filled out by the lexer.
typedef struct SourceStats {
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
void free_context(Context *c);

// From types.c, couldn't declare in types.h because of a circular dependency with context.h
void init_types(Context *, SourceStats *);
void free_types(Context *);

bool check_types_were_declared(Context *);

Name *make_name(Context *, Token from);

Symbol *lookup_local(AstProcedure *, Name *);
Symbol *lookup_struct_field(AstStruct *, Name *);
Symbol *lookup_in_block(AstBlock *, Name *);

void add_symbol(Context *, AstNode *decl, char *name);

#endif
