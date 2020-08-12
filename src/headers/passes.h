#ifndef ERROR_PARSE_h
#define ERROR_PARSE_h

#include "ast.h"
#include "context.h"

void print_ast_errors(Context *, Ast *);
void check_ast(Context *, Ast *);
void resolve_top_level(Context *);

#endif
