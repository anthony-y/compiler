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

#define CONTEXT_SCRATCH_SIZE 1024 * 3
#define CONTEXT_STACK_SIZE   32

// An entry in the name table.
struct Name {char *text;};

struct Module {
    Ast       ast;
    TokenList tokens;
};

struct BlockStack {
    AstBlock *data[CONTEXT_STACK_SIZE];
    u64       top;
};

struct ProcedureStack {
    AstProcedure *data[CONTEXT_STACK_SIZE];
    u64           top;
};

//
// This structure almost certainly does not fit into a single cache line, so it's slow to access if it gets evicted from cache.
// It is used in almost all functions in the compiler though, so I find it unlikely that that will happen often, if at all.
//
struct Context {
    int error_count;

    Arena     string_allocator;
    NodeArena node_allocator;

    Name    *entry_point_name;
    AstDecl *decl_for_main;

    BlockStack     block_stack;
    ProcedureStack proc_stack;

    // The name table is basically just a big string table,
    // it means I can compare strings by pointer instead of strcmp().
    struct { char *key;    Name        *value; } *name_table;

    struct { char *key;    Module      *value; } *modules;

    struct { char *key;    AstTypeDecl *value; } *builtin_types;

    // Handles to types in the type table for easy comparison in type-checking, etc.
    AstTypeDecl *type_int;
    AstTypeDecl *type_s64;
    AstTypeDecl *type_u64;
    AstTypeDecl *type_u32;
    AstTypeDecl *type_s32;
    AstTypeDecl *type_u16;
    AstTypeDecl *type_s16;
    AstTypeDecl *type_u8;
    AstTypeDecl *type_s8;
    AstTypeDecl *type_f32;
    AstTypeDecl *type_f64;
    AstTypeDecl *type_string;
    AstTypeDecl *type_void;
    AstTypeDecl *type_bool;
    AstTypeDecl *null_type;
    AstTypeDecl *type_any;
    AstTypeDecl *import_type;
    AstTypeDecl *type_type;
    AstTypeDecl *type_library;
};

void compile_error(Context *, Token, const char *fmt, ...);
void compile_error_start(Context *, Token, const char *fmt, ...);
void compile_error_add_line(Context *, const char *fmt, ...);
void compile_error_end();
void compile_warning(Context *, Token, const char *fmt, ...);

void init_context(Context *);
void free_context(Context *);

Name *make_name_string(Context *, const char *txt);
Name *make_name_from_token(Context *, Token from);

void block_stack_push(BlockStack *bs, AstBlock *block);
AstBlock *block_stack_pop(BlockStack *bs);
AstBlock *block_stack_top(BlockStack bs);

void proc_stack_push(ProcedureStack *, AstProcedure *);
AstProcedure *proc_stack_pop(ProcedureStack *);
AstProcedure *proc_stack_top(ProcedureStack);

Module *get_module(Context *ctx, Name *name, Module *in, Token site);
AstDecl *find_in_block(AstBlock *block, Name *name);
AstDecl *find_top_level_decl(Module *in, Name *name);
AstDecl *find_decl_from_local_scope_upwards(Context *ctx, Name *name, Module *file_scope);
AstDecl *find_struct_field(AstStruct *, Name *);

char *read_file(const char *path);

#endif
