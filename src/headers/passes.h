#ifndef ERROR_PARSE_h
#define ERROR_PARSE_h

#include "ast.h"
#include "context.h"

void check_ast(Context *, Ast *); // checker.c
void resolve_module(Context *, Module *); // resolve.c
AstTypeDecl *resolve_decl(AstDecl *decl, Context *ctx, Module *module); // not really a pass but whatever
void free_subtrees_and_blocks(Ast *); // ast.c
char *generate_and_write_c_code(Context *ctx, Ast *ast, const char *file_name);

#endif
