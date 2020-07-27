#ifndef TYPE_h
#define TYPE_h

#include "common.h"
#include <stdio.h>

typedef enum {
    Type_PRIMITIVE,
    Type_STRUCT,
    Type_POINTER,
    Type_ARRAY,
    Type_ALIAS,

    Type_DEFERRED_NAMED,
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
        struct AstNode *user; // array of types for enums and structs

        struct Type *alias_of; // for typedef aliases
        struct Type *base; // arrays and pointers
        Signage signage; // primitive integer types
    } data;
} Type;

Type *make_type(TypeKind kind, char *name, u64 size);
void print_type(Type *, FILE *);

// Gonna declare these in main.c before or I'll get a circular dependency
// because of the argument type.
//    void init_types(Context *)
//    void free_types(Context *);

#endif
