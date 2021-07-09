#ifndef TYPE_h
#define TYPE_h

#include "common.h"
#include "ast.h"
#include <stdio.h>

struct Name;

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
    TypeDeclExprType kind;
} AnyType;

typedef struct {
    AstTypeDecl *return_type;
    void *argument_types; // TODO
} ProcedureType;

AstTypeDecl *make_type(TypeDeclExprType kind, struct Name *name, u64 size);
AstTypeDecl *make_pointer_type(AstTypeDecl *base_type);
AstTypeDecl *make_array_type(AstTypeDecl *base);
AstTypeDecl *make_procedure_type(AstTypeDecl *return_type, struct AstTypeDecl **argument_types);
AstTypeDecl *make_type_alias(AstTypeDecl *of);
AstTypeDecl *make_placeholder_type(Name *name);

void print_type(struct AstTypeDecl *);
struct AstTypeDecl *unwrap_pointer_type(struct AstTypeDecl *ptr, int *out_depth);
bool is_type_numeric(struct AstTypeDecl *);

struct Context;
void init_types(struct Context *ctx);
void free_types(struct Context *ctx);

#endif
