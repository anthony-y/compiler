#ifndef TYPE_h
#define TYPE_h

#include "common.h"
#include <stdio.h>

typedef enum {
    Type_PRIMITIVE,
    Type_STRUCT,
    Type_ANON_STRUCT,
    Type_POINTER,
    Type_ARRAY,
    Type_ALIAS,

    Type_UNRESOLVED,
        // deferred checking until end of parsing, when all types have been collected
} TypeKind;

typedef enum {
    Signage_UNSIGNED,
    Signage_SIGNED,
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

struct AstNode;

typedef struct Type {
    char *name; // for error printing
    u64 size;

    TypeKind kind;
    union {
        /* TODO decide on Type array or struct body pointer for this */
        // struct Type *user;
        struct AstStmt *user; // array of types for enums and structs

        struct Type *alias_of; // for typedef aliases
        struct Type *base; // arrays and pointers
        Signage signage; // primitive integer types
    } data;
} Type;

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
