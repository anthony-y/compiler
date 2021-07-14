#ifndef TYPE_h
#define TYPE_h

#include "common.h"
#include "ast.h"
#include <stdio.h>

struct Name;

struct StringType {
    u64 len;
    char *data;
};

struct ArrayType {
    u64 len;
    void *data;
};

struct AnyType {
    void *data;
    TypeDeclExprType kind;
};

struct ProcedureType {
    AstTypename *return_type;
    Ast argument_types; // TODO
};

AstTypeDecl *make_type(TypeDeclExprType kind, struct Name *name, u64 size);
AstTypeDecl *make_pointer_type(AstTypeDecl *base_type);
AstTypeDecl *make_array_type(AstTypeDecl *base);
AstTypeDecl *make_procedure_type(AstTypename *return_type, Ast argument_types);
AstTypeDecl *make_type_alias(AstTypeDecl *of);
AstTypeDecl *make_placeholder_type(Name *name);

void print_type(struct AstTypeDecl *);
struct AstTypeDecl *unwrap_pointer_type(struct AstTypeDecl *ptr, int *out_depth);
bool is_type_numeric(struct AstTypeDecl *);

struct Context;
void init_types(struct Context *ctx);
void free_types(struct Context *ctx);

#endif
