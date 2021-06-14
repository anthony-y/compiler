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

//
// This structure almost certainly does not fit into a single cache line, so it's slow to access if it gets evicted from cache.
// It is used in almost all functions in the compiler though, so I find it unlikely that that will happen often, if at all.
//
typedef struct Context {
    int error_count;

    Arena string_allocator;
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

    // Handles to types in the type table for easy comparison in type-checking, etc.
    Type *type_int;
    Type *type_s64;
    Type *type_u64;
    Type *type_u32;
    Type *type_s32;
    Type *type_u16;
    Type *type_s16;
    Type *type_u8;
    Type *type_s8;
    Type *type_f32;
    Type *type_f64;
    Type *type_string;
    Type *type_void;
    Type *type_bool;
    Type *null_type;
    Type *type_any;
    Type *import_type;
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

char *read_file(const char *path);

#endif
