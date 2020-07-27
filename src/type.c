#include "headers/common.h"
#include "headers/passes.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/arena.h"
#include "headers/ast.h"

#include "headers/stb/stb_ds.h"
#include "headers/stb/stretchy_buffer.h"

#include <stdio.h>
#include <assert.h>

static Arena type_arena;

// checker.c
Type *type_from_expr(Context *ctx, AstNode *expr);

// Allocates and initializes a Type from the type_arena,
// and returns a reference to it.
Type *make_type(TypeKind kind, char *name, u64 size) {
    Type *t = arena_alloc(&type_arena, sizeof(Type));
    t->kind = kind;
    t->name = name;
    t->size = size;
    return t;
}

Type *make_pointer_type(Type *base) {
    Type *out = make_type(Type_POINTER, "", sizeof(void*));
    out->data.base = base;
    return out;
}

// Allocates a builtin/primitive type and places it into the type table.
static inline Type *make_primitive(Context *ctx, char *name, u64 size, Signage signage) {
    Type *t = make_type(Type_PRIMITIVE, name, size);
    t->data.signage = signage;
    shput(ctx->type_table, name, t);
    return t;
}

void free_types(Context *ctx) {
    arena_free(&type_arena);
    shfree(ctx->type_table);
    sb_free(ctx->deferred_names);
}

// Initialize the type table and add the primitive types to it.
// Then, create some handles to internal types.
// Uses a const SourceStats * to compute the type arenas allocation size.
void init_types(Context *ctx, SourceStats *stats) {
    const int num_builtins = 13;

    // We need room for two potential allocations per type;
    // one for the actual type, and an extra for its placeholder
    // if it is used before its declaration.
    //
    // TODO this might change when I do proper symbol resolution.
    stats->declared_types *= 2;

    u64 total_types = (num_builtins + stats->pointer_types + stats->declared_types);
    arena_init(&type_arena, total_types, sizeof(Type), 8);

    sh_new_arena(ctx->type_table); // initialize the type table as a string hash map

    // The following `sizeof` expressions do not 
    ctx->type_int = make_primitive(ctx, "int", sizeof(s64), Signage_SIGNED);

    ctx->type_s64 = make_primitive(ctx, "s64", sizeof(s64), Signage_SIGNED);
    ctx->type_u64 = make_primitive(ctx, "u64", sizeof(u64), Signage_UNSIGNED);

    ctx->type_s32 = make_primitive(ctx, "s32", sizeof(s32), Signage_SIGNED);
    ctx->type_u32 = make_primitive(ctx, "u32", sizeof(u32), Signage_UNSIGNED);

    ctx->type_s16 = make_primitive(ctx, "s16", sizeof(s16), Signage_SIGNED);
    ctx->type_u16 = make_primitive(ctx, "u16", sizeof(u16), Signage_UNSIGNED);

    ctx->type_s8 = make_primitive(ctx, "s8", sizeof(s8), Signage_SIGNED);
    ctx->type_u8 = make_primitive(ctx, "u8", sizeof(u8), Signage_UNSIGNED);

    ctx->type_bool = make_primitive(ctx, "bool", sizeof(u8), Signage_NaN);
    ctx->type_void = make_primitive(ctx, "void", 0, Signage_NaN);
    ctx->type_string = make_primitive(ctx, "string", sizeof(StringType), Signage_NaN);

    // This isn't inserted into the type table, it's only referred to directly by its pointer handle.
    ctx->decoy_ptr = make_type(Type_POINTER, "pointer", 0);

    // Might need this again later but probs not
    // ctx->type_unknown_named = arena_alloc(&type_arena, sizeof(Type));
    // *ctx->type_unknown_named = (Type){0};
}

// During parse-time, names of types which were as of yet undeclared, were accumulated.
// This code runs after parse-time and prints an error if a type still isn't in the type table.
bool check_types_were_declared(Context *ctx) {
    u64 errors_before = ctx->error_count;
    if (!ctx->deferred_names) return true;
    for (int i = 0; i < sb_len(ctx->deferred_names); i++) {
        const Token *t = &ctx->deferred_names[i];
        if (shgeti(ctx->type_table, t->text) == -1) {
            compile_error(ctx, *t, "Undeclared type \"%s\"", t->text);
        }
    }
    if (errors_before == ctx->error_count) return true;
    return false;
}

void fill_in_deferred_type(Context *c, Type **type) {
    switch ((*type)->kind) {
    default: return;
    case Type_DEFERRED_NAMED: {
        // check_types_were_declared() is called before this function
        // to ensure that all the names have been assigned to types at some point in the source code
        // This ensures that shget() will not fail.
        *type = shget(c->type_table, (*type)->name);
        return;
    }
    case Type_POINTER:
    case Type_ARRAY: {
        // Recurse into the base types.
        fill_in_deferred_type(c, &(*type)->data.base);
        return;
    }
    case Type_ALIAS:
        fill_in_deferred_type(c, &(*type)->data.alias_of);
        return;
    }
}

void infer_type(Context *c, AstNode *n) {
    switch (n->tag) {
    case Node_VAR: {
        AstVar *var = &n->as.var;
        if (!(var->flags & VAR_IS_INFERRED)) break;
        var->typename->as.type = type_from_expr(c, var->value);
    } break;
    }
}

void fill_in_types(Context *c, Ast *ast) {
    if (!ast || !ast->nodes || ast->len == 0) return;

    for (int i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (!node) return;

        switch (node->tag) {
        default: continue;
        case Node_VAR: {
            AstVar *var = &node->as.var;
            if (!(var->flags & VAR_IS_INFERRED)) {
                fill_in_deferred_type(c, &var->typename->as.type);
                continue;
            }
            infer_type(c, node);
        } break;
        case Node_PROCEDURE: {
            AstProcedure *proc = &node->as.procedure;
            fill_in_types(c, proc->block->as.block.statements);
            fill_in_deferred_type(c, &proc->return_type->as.type);
        } break;
        case Node_TYPEDEF: {
            AstTypedef *td = &node->as.typedef_;
            if (td->of->tag == Node_STRUCT) {
                fill_in_types(c, td->of->as.struct_.members->as.block.statements);
            }
        } break;
        }
    }
}

// TODO most of the time this will be stderr, so maybe just enforce that
void print_type(Type *type, FILE *stream) {
    if (!type) {
        fprintf(stream, "unknown");
        return;
    }

    switch (type->kind) {
    case Type_PRIMITIVE:
    case Type_STRUCT:
        fprintf(stream, "%s", type->name);
        return;
    case Type_POINTER:
        if (!type->data.base) { // it's the decoy_ptr (context.hpp)
            fprintf(stream, "pointer");
            return;
        }
        fprintf(stream, "^");
        print_type(type->data.base, stream);
        return;
    case Type_ALIAS:
        fprintf(stream, "%s (alias of ", type->name);
        print_type(type->data.alias_of, stream);
        fprintf(stream, ")");
        return;
    case Type_DEFERRED_NAMED:
        fprintf(stderr, "Internal compiler error: Type_DEFERRED_NAMED is not supposed to be printed.\n");
        return;
    default:
        fprintf(stderr, "Internal compiler error: type not covered in print_type switch; type kind is %d\n", type->kind);
        break;
    }
}
