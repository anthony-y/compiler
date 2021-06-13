#ifndef TYPE_h
#define TYPE_h

#include "common.h"
#include <stdio.h>

typedef enum {
    Type_PRIMITIVE,
    Type_STRUCT,
    Type_ENUM,
    Type_ANON_STRUCT,
    Type_POINTER,
    Type_ARRAY,
    Type_ALIAS,
    Type_IMPORT, // returned from #import expression, can't be typed out by programmer

    Type_UNRESOLVED,
        // deferred checking until end of parsing, when all types have been collected
} TypeKind;

typedef enum {
    Signage_UNSIGNED,
    Signage_SIGNED,
    Signage_SIGNED_FLOATING,
    Signage_NaN,
} Signage;

typedef struct {
    u64 len;
    char *data;
} StringType;

typedef struct {
    u64 len;
    void *data;
} ArrayType;

typedef struct {
    void *data;
    TypeKind kind;
} AnyType;

struct AstNode;

typedef struct Type {
    char *name; // for error printing
    u64 size;

    TypeKind kind;
    union {
        struct AstNode *user; // TODO: rename to decl.
        struct Type *alias_of; // for typedef aliases
        struct Type *base; // arrays and pointers
        Signage signage; // primitive integer types
    } data;
} Type;

struct Context;

Type *make_type(TypeKind kind, char *name, u64 size);
Type *make_pointer_type(Type *base_type);
Type *make_array_type(Type *base);
void print_type(Type *);
Type *unwrap_pointer_type(Type *ptr, int *out_depth);
bool is_type_numeric(Type *);

// Gonna declare these in main.c before or I'll get a circular dependency
// because of the argument type.
//    void init_types(Context *)
//    void free_types(Context *);

#endif
