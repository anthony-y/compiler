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

#define CONTEXT_SCRATCH_SIZE 1024 * 3

// An entry in the name table.
typedef struct Name {char *text;} Name;

// Central compiler context, a reference to an instance of this struct is
// passed to most functions in the compiler.
//
// This structure does probably not fit into a cache line so it's slow to get if it's not in cache,
// probs gets cached anyway tho.
typedef struct Context {
    Arena string_allocator; // lexer
    Arena node_allocator; // parser
    Arena scratch;

    // The name table is basically just a big string table,
    // it means I can compare strings by pointer instead of strcmp().
    struct {
        char *key;
        Name *value;
    } *name_table;

    struct {
        char *key;
        Ast  *value;
    } *modules;

    struct {
        char *key;
        Type *value;
    } *builtin_types;

    AstDecl *decl_for_main;

    /* Handles to types in the type table
       for easy comparison in type-checking, etc. */
    Type *type_int, *type_s64, *type_u64, *type_u32, *type_s32,
        *type_u16, *type_s16, *type_u8, *type_s8;
    Type *type_f32, *type_f64;
    Type *type_string, *type_void, *type_bool; // non-integer types
    Type *null_type; // type of "null" literal
    Type *type_any; // any
    Type *import_type;

    int error_count;
} Context;

Ast *get_module(Context *ctx, Name *name, Ast *in, Token site);

void compile_error(Context *, Token, const char *fmt, ...);
void compile_error_start(Context *, Token, const char *fmt, ...);
void compile_error_add_line(Context *, const char *fmt, ...);
void compile_error_end();
void compile_warning(Context *, Token, const char *fmt, ...);

void init_context(Context *);
void free_context(Context *);

Name *make_name_from_string(Context *, const char *txt);
Name *make_name_from_token(Context *, Token from);

AstDecl *lookup_local(Context *ctx, AstProcedure *proc, Name *name, AstBlock *start_from, Ast *file_scope);
AstDecl *find_type_decl(Ast *ast, Name *name);
AstDecl *lookup_struct_field(AstStruct *, Name *);
AstDecl *lookup_in_block(AstBlock *, Name *);
AstDecl *find_decl(Ast *ast, Name *name);

void add_symbol(Context *, AstDecl *decl, char *name);
char *read_file(const char *path);

#endif
