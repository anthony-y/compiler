#ifndef ERROR_PARSE_h
#define ERROR_PARSE_h

#include "ast.h"
#include "context.h"

void print_ast_errors(Context *, Ast *);
void check_ast(Context *, Ast *);

// Fill in types for both:
//   inferred variable declarations
//   unresolved struct types
//
void resolve_and_infer_types(Context *, Ast *);

void resolve_top_level(Context *);

#endif
