#ifndef CONTEXT_h
#define CONTEXT_h

#include "lexer.h"
#include "parser.h"
#include "type.h"
#include "common.h"
#include "token.h"
#include "arena.h"
#include "ast.h"
#include "table.h"

#include <stdarg.h>

#define CONTEXT_SCRATCH_SIZE 1024

// An entry in the name table.
typedef struct Name {char *text;} Name;

typedef struct Module {
	char *path;
    Table imports;
	Ast ast;

    // These are just here to be freed at the end.
    Parser parser;
    Lexer lexer;
} Module;

// Contains statistics about the source code
// which can be used to compute allocation sizes.
// Filled out by the lexer.
typedef struct SourceStats {
    u64 declared_types;
    u64 pointer_types;
    u64 argument_lists;
    u64 blocks;
    u64 structs;
    u64 number_of_lines;
} SourceStats;

// Central compiler context, a reference to an instance of this struct is
// passed to most functions in the compiler.
typedef struct Context {
    Arena string_allocator; // lexer
    Arena node_allocator; // parser
    Arena scratch;

    Table symbols;
    Table type_table;
    struct {
        char *key;
        Name *value;
    } *name_table;

	Module *current_module;
    AstDecl *decl_for_main;

    /* Handles to types in the type table
       for easy comparison in type-checking, etc. */
    Type *type_int, *type_s64, *type_u64, *type_u32, *type_s32,
        *type_u16, *type_s16, *type_u8, *type_s8;
    Type *type_f32, *type_f64;
    Type *type_string, *type_void, *type_bool; // non-integer types
    Type *null_type; // type of "null" literal
    Type *type_any; // any

    int error_count;
} Context;

void compile_error(Context *, Token, const char *fmt, ...);
void compile_error_start(Context *, Token, const char *fmt, ...);
void compile_error_add_line(Context *, const char *fmt, ...);
void compile_error_end();
void compile_warning(Context *, Token, const char *fmt, ...);

void init_context(Context *);
void free_context(Context *);

// From types.c, couldn't declare in types.h because of a circular dependency with context.h
void init_types(Context *, SourceStats *);
void free_types(Context *);

char *encode_module_into_name(Context *, char *raw);
char *get_encoded_name(char *module, char *raw);
Name *make_name_from_string(Context *, const char *txt);
Name *make_name_from_token(Context *, Token from);

AstDecl *lookup_local(Context *ctx, AstProcedure *proc, Name *name, AstBlock *start_from);
AstDecl *lookup_struct_field(AstStruct *, Name *);
AstDecl *lookup_in_block(AstBlock *, Name *);

void add_symbol(Context *, AstDecl *decl, char *name);

#endif
