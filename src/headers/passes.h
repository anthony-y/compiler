#ifndef ERROR_PARSE_h
#define ERROR_PARSE_h

#include "ast.h"
#include "context.h"

void check_ast(Context *, Ast *); // checker.c
void resolve_main_module(Context *, Module *main_module); // resolve.c
void resolve_module(Context *); // resolve.c
void free_subtrees_and_blocks(Ast *); // ast.c
char *generate_and_write_c_code(Context *, Ast *); // codegen.c

#endif
