// Functions for working with the compilers
// internal representation of a data-type.
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

static Arena type_arena; // the structure which types are allocated from

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
    Type *out = make_type(Type_POINTER, NULL, sizeof(void*));
    out->data.base = base;
    return out;
}

Type *make_array_type(Type *base) {
    Type *out = make_type(Type_ARRAY, NULL, sizeof(ArrayType));
    out->data.base = base;
    return out;
}

inline bool is_type_numeric(Type *t) {
    return (t->kind == Type_PRIMITIVE && t->data.signage != Signage_NaN);
}

// Allocates and initializes a primitive type, and returns it.
static inline Type *make_and_insert_primitive(Context *ctx, char *name, u64 size, Signage signage) {
    Type *t = make_type(Type_PRIMITIVE, name, size);
    t->data.signage = signage;
    shput(ctx->type_table, name, t);
    return t;
}

void free_types(Context *ctx) {
    arena_free(&type_arena);
    shfree(ctx->type_table);
}

// Initialize the type table and add the primitive types to it.
// Then, create some handles to internal types.
// Uses a const SourceStats * to compute the type arenas allocation size.
void init_types(Context *ctx, SourceStats *stats) {
    const int num_builtins = 14;

    // We need room for two potential allocations per type;
    // one for the actual type, and an extra for its placeholder
    // if it is used before its declaration.
    stats->declared_types *= 2;

    u64 total_types = (num_builtins + stats->pointer_types + (stats->declared_types * 2));
    arena_init(&type_arena, total_types, sizeof(Type), 8);

    sh_new_arena(ctx->type_table); // initialize the type table as a string hash map

    ctx->type_int = make_and_insert_primitive(ctx, "int", sizeof(s64), Signage_SIGNED);

    ctx->type_s64 = make_and_insert_primitive(ctx, "s64", sizeof(s64), Signage_SIGNED);
    ctx->type_u64 = make_and_insert_primitive(ctx, "u64", sizeof(u64), Signage_UNSIGNED);

    ctx->type_s32 = make_and_insert_primitive(ctx, "s32", sizeof(s32), Signage_SIGNED);
    ctx->type_u32 = make_and_insert_primitive(ctx, "u32", sizeof(u32), Signage_UNSIGNED);

    ctx->type_s16 = make_and_insert_primitive(ctx, "s16", sizeof(s16), Signage_SIGNED);
    ctx->type_u16 = make_and_insert_primitive(ctx, "u16", sizeof(u16), Signage_UNSIGNED);

    ctx->type_s8 = make_and_insert_primitive(ctx, "s8", sizeof(s8), Signage_SIGNED);
    ctx->type_u8 = make_and_insert_primitive(ctx, "u8", sizeof(u8), Signage_UNSIGNED);

    ctx->type_f64 = make_and_insert_primitive(ctx, "f64", sizeof(double), Signage_SIGNED_FLOATING);
    ctx->type_f32 = make_and_insert_primitive(ctx, "f32", sizeof(float), Signage_SIGNED_FLOATING);

    ctx->type_bool = make_and_insert_primitive(ctx, "bool", sizeof(u8), Signage_NaN);
    ctx->type_void = make_and_insert_primitive(ctx, "void", 0, Signage_NaN);
    ctx->type_string = make_and_insert_primitive(ctx, "string", sizeof(StringType), Signage_NaN);

    ctx->type_any = make_and_insert_primitive(ctx, "any", sizeof(AnyType), Signage_NaN);

    ctx->null_type = make_pointer_type(NULL);

    //Type *string = make_type(Type_STRUCT, "string", sizeof(StringType));
}

// Utility function to unwrap a pointer to it's ultimate base type.
// Returns the unwrapped pointer, and returns the depth to `out_depth`.
// Does not print errors.
inline Type *unwrap_pointer_type(Type *ptr, int *out_depth) {
    assert(ptr->kind == Type_POINTER);
    int depth = 0;
    while (ptr->kind == Type_POINTER) {
        ptr = ptr->data.base;
        depth++;
    }
    if (out_depth) *out_depth = depth;
    return ptr;
}

void print_type(Type *type) {
    FILE *stream = stderr;
    if (!type) {
        fprintf(stream, "unknown");
        return;
    }

    switch (type->kind) {
    case Type_PRIMITIVE:
    case Type_STRUCT:
        fprintf(stream, "%s", type->name);
        return;
    case Type_ANON_STRUCT:
        fprintf(stream, "anonyous struct");
        return;
    case Type_POINTER:
        if (!type->data.base) { // generic pointer type, probably from a null literal
            fprintf(stream, "pointer");
            return;
        }
        fprintf(stream, "^");
        print_type(type->data.base);
        return;
    case Type_ALIAS:
        fprintf(stream, "%s (alias of ", type->name);
        print_type(type->data.alias_of);
        fprintf(stream, ")");
        return;
    case Type_ARRAY:
        fprintf(stream, "[]");
        print_type(type->data.base);
        return;
    case Type_UNRESOLVED:
        fprintf(stream, "Internal compiler error: Type_UNRESOLVED is not supposed to be printed.\n");
        return;
    default:
        fprintf(stderr, "Internal compiler error: type not covered in print_type switch; type kind is %d\n", type->kind);
    }
}
