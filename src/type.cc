// Functions for working with the compilers internal representation of a data-type.
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

// Allocates and initializes a AstTypeDecl from the type_arena,
// and returns a reference to it.
AstTypeDecl *make_type(TypeDeclExprType kind, Name *name, u64 size) {
    AstTypeDecl *node = (AstTypeDecl *)malloc(sizeof(AstTypeDecl)); // TODO: use the NodeArena
    node->tag       = Node_TYPE_DECL;
    node->token     = Token{};
    node->expr_type = kind;
    node->name      = name;
    node->size      = size;
    node->status    = Status_UNRESOLVED;
    return node;
}

AstTypeDecl *make_pointer_type(AstTypeDecl *base) {
    AstTypeDecl *out = make_type(TypeDecl_POINTER, NULL, sizeof(void*));
    out->base_type = base;
    return out;
}

AstTypeDecl *make_array_type(AstTypeDecl *base) {
    AstTypeDecl *out = make_type(TypeDecl_ARRAY, NULL, sizeof(ArrayType));
    out->base_type = base;
    return out;
}

AstTypeDecl *make_procedure_type(AstTypeDecl *return_type, AstTypeDecl **argument_types) {
    AstTypeDecl *base = make_type(TypeDecl_PROCEDURE, NULL, sizeof(ProcedureType));
    /*
    base->data.procedure.return_type = return_type;
    base->data.procedure.argument_types = NULL; // TODO lol
    */
    // TODO
    return base;
}

AstTypeDecl *make_placeholder_type(Name *name) {
    AstTypeDecl *placeholder = make_type(TypeDecl_PLACEHOLDER, name, 0);
    return placeholder;
}

AstTypeDecl *make_type_alias(AstTypeDecl *of) {
    assert(false);
    AstTypeDecl *out = make_type(TypeDecl_ALIAS, NULL, of->size);
    out->base_type = of;
    return out;
}

bool is_type_numeric(AstTypeDecl *t) {
    return (t->expr_type == TypeDecl_INTEGER || t->expr_type == TypeDecl_FLOAT);
}

// Allocates and initializes a primitive type, and returns it.
static inline AstTypeDecl *make_and_insert_primitive(TypeDeclExprType tag, Context *ctx, char *sname, u64 size, bool is_signed) {
    Name *name = make_name_string(ctx, sname);
    AstTypeDecl *t = make_type(tag, name, size);
    t->status = Status_RESOLVED;
    if (is_signed) t->flags |= TYPE_DECL_IS_SIGNED_INTEGER;
    shput(ctx->builtin_types, sname, t);
    return t;
}

void free_types(Context *ctx) {
    for (int i = 0; i < shlen(ctx->builtin_types); i++)
        free(ctx->builtin_types[i].value);
    shfree(ctx->builtin_types);
}

// Initialize the type table and add the primitive types to it.
// Then, create some handles to internal types.
// Uses a const SourceStats * to compute the type arenas allocation size.
void init_types(Context *ctx) {
    sh_new_arena(ctx->builtin_types);

    ctx->type_int = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "int", sizeof(s64), true);

    ctx->type_s64 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "s64", sizeof(s64), true);
    ctx->type_u64 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "u64", sizeof(u64), false);

    ctx->type_s32 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "s32", sizeof(s32), true);
    ctx->type_u32 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "u32", sizeof(u32), false);

    ctx->type_s16 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "s16", sizeof(s16), true);
    ctx->type_u16 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "u16", sizeof(u16), false);

    ctx->type_s8 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "s8", sizeof(s8), true);
    ctx->type_u8 = make_and_insert_primitive(TypeDecl_INTEGER, ctx, "u8", sizeof(u8), false);

    ctx->type_f64 = make_and_insert_primitive(TypeDecl_FLOAT, ctx, "f64", sizeof(double), false);
    ctx->type_f32 = make_and_insert_primitive(TypeDecl_FLOAT, ctx, "f32", sizeof(float), false);

    ctx->type_bool = make_and_insert_primitive(TypeDecl_BOOL, ctx, "bool", sizeof(u8), false);
    ctx->type_void = make_and_insert_primitive(TypeDecl_VOID, ctx, "void", 0, false);
    ctx->type_string = make_and_insert_primitive(TypeDecl_STRING, ctx, "string", sizeof(StringType), false);

    ctx->type_any = make_and_insert_primitive(TypeDecl_ANY, ctx, "Any", sizeof(AnyType), false);
    ctx->type_type = make_and_insert_primitive(TypeDecl_TYPE, ctx, "Type", sizeof(AstTypeDecl), false);
    ctx->import_type = make_type(TypeDecl_TYPE, make_name_string(ctx, "__Import"), 0);
    ctx->type_library = make_type(TypeDecl_TYPE, make_name_string(ctx, "__Library"), 0);
    ctx->null_type = make_pointer_type(NULL);
}

// Utility function to unwrap a pointer to it's ultimate base type.
// Returns the unwrapped pointer, and returns the depth to `out_depth`.
// Does not print errors.
inline AstTypeDecl *unwrap_pointer_type(AstTypeDecl *ptr, int *out_depth) {
    assert(ptr->expr_type == TypeDecl_POINTER);
    int depth = 0;
    while (ptr->expr_type == TypeDecl_POINTER) {
        ptr = ptr->base_type;
        depth++;
    }
    if (out_depth) *out_depth = depth;
    return ptr;
}

void print_type(AstTypeDecl *type) {
    FILE *stream = stderr;
    if (!type) {
        fprintf(stream, "unknown");
        return;
    }

    switch (type->expr_type) {
    case TypeDecl_STRING:
    case TypeDecl_INTEGER:
    case TypeDecl_FLOAT:
    case TypeDecl_ENUM:
    case TypeDecl_STRUCT:
    case TypeDecl_VOID:
    case TypeDecl_ANY:
        fprintf(stream, "%s", type->name->text);
        return;
    case TypeDecl_POINTER:
        if (!type->base_type) { // generic pointer type, probably from a null literal
            fprintf(stream, "pointer");
            return;
        }
        fprintf(stream, "^");
        print_type(type->base_type);
        return;
    case TypeDecl_ALIAS:
        fprintf(stream, "%s (alias of ", type->name->text);
        print_type(type->alias->resolved_type);
        fprintf(stream, ")");
        return;
    case TypeDecl_ARRAY:
        fprintf(stream, "[]");
        print_type(type->base_type);
        return;
    default:
        fprintf(stderr, "Internal compiler error: type not covered in print_type switch; type kind is %d\n", type->expr_type);
    }
}
