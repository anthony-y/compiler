#ifndef ERROR_PARSE_h
#define ERROR_PARSE_h

#include "ast.h"
#include "context.h"

void check_ast(Context *, Ast *); // checker.c
void resolve_program(Context *); // resolve.c
void free_subtrees_and_blocks(Ast *); // ast.c

#endif
