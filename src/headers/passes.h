#ifndef ERROR_PARSE_h
#define ERROR_PARSE_h

#include "ast.h"
#include "context.h"

void print_ast_errors(Context *, Ast *); // error_pass.c
void check_ast(Context *, Ast *); // checker.c
void resolve_top_level(Context *); // resolve.c
void free_subtrees_and_blocks(Ast *); // ast.c

#endif
