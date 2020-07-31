#include "headers/passes.h"

#include <assert.h>
#include <stdio.h>

static int num_errs_printed = 0;

static inline void print_error_node(Context *c, AstNode *err) {
    if (!err || err->tag != Node_ERROR) return;
    fprintf(stderr, "%s:%lu: Error: %s\n", c->current_file_path, err->token.line, err->as.error.msg);
    num_errs_printed++;
}

static void print_expression_errors(Context *c, AstNode *node) {
    if (!node) return;

    switch (node->tag) {
    case Node_IDENT:
    case Node_INT_LIT:
    case Node_BOOL_LIT:
    case Node_STRING_LIT:
    case Node_ARRAY_LIT:
    case Node_FLOAT_LIT:
    case Node_NIL:
        break;
    case Node_ERROR:
        print_error_node(c, node);
        break;
    case Node_ENCLOSED:
        print_expression_errors(c, node->as.enclosed.sub_expr);
        break;
    case Node_BINARY:
        print_expression_errors(c, node->as.binary.left);
        print_expression_errors(c, node->as.binary.right);
        break;
    case Node_UNARY:
        print_expression_errors(c, node->as.unary.expr);
        break;
    case Node_CALL:
        print_ast_errors(c, node->as.function_call.params);
        print_expression_errors(c, node->as.function_call.name);
        break;
    case Node_INDEX:
        print_expression_errors(c, node->as.array_index.name);
        print_expression_errors(c, node->as.array_index.index);
        break;
    case Node_CAST:
        print_error_node(c, node->as.cast.typename);
        print_expression_errors(c, node->as.cast.expr);
        break;

    default:
        printf("Internal compiler error: expression error not covered in printer. ");
        printf("Node type: %d\n", node->tag);
    }
}

static void print_statement_errors(Context *c, AstNode *node) {
    if (!node) return;

    if (node->tag == Node_ERROR) {
        print_error_node(c, node);
        return;
    }

    else if (node->tag == Node_PROCEDURE) {
        const AstProcedure *proc = &node->as.procedure;
        print_ast_errors(c, proc->params);
        print_error_node(c, proc->return_type);
        print_statement_errors(c, proc->block);
    }

    else if (node->tag == Node_BLOCK) {
        const AstBlock *block = &node->as.block;
        print_ast_errors(c, block->statements);
    }

    else if (node->tag == Node_STRUCT) {
        const AstStruct *def = &node->as.struct_;
        print_statement_errors(c, def->members);
    }

    else if (node->tag == Node_TYPEDEF) {
        const AstTypedef *def = &node->as.typedef_;
        if (def->of->tag == Node_STRUCT) {
            print_statement_errors(c, def->of);
            return;
        }
        print_statement_errors(c, def->of);
    }

    else if (node->tag == Node_VAR) {
        const AstVar *def = &node->as.var;
        print_error_node(c, def->typename);
        if (def->flags & VAR_TYPE_IS_ANON_STRUCT) print_statement_errors(c, def->typename);
        if (def->flags & VAR_IS_INITED) print_error_node(c, def->value);
    }

    else if (node->tag == Node_DEFER) {
        print_statement_errors(c, node->as.defer.statement);
    }

    else if (node->tag == Node_RETURN) {
        print_error_node(c, node->as.return_.expr);
    }

    else if (node->tag == Node_WHILE) {
        print_expression_errors(c, node->as.while_.condition);
        print_statement_errors(c, node->as.while_.block);
    }

    else if (node->tag == Node_IF) {
        print_expression_errors(c, node->as.if_.condition);
        print_statement_errors(c, node->as.if_.block_or_stmt);
    }

    else if (node->tag == Node_TYPENAME) {
        Type *astype = node->as.type;
        if (astype->kind == Type_STRUCT) {
            print_statement_errors(c, astype->data.user);
        }
    }

    else {
        fprintf(stderr, "Internal compiler error: print_statement_errors shouldn't have got here. Node type is %d.\n", node->tag);
    }
}

void print_ast_errors(Context *c, Ast *ast) {
    if (!ast || !ast->nodes || ast->len == 0) return;

    for (int i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (!node) return;

        if (node->tag == Node_ERROR) {
            print_error_node(c, node);
            continue;
        }

        if (node->tag > Node_STATEMENTS_START && node->tag < Node_STATEMENTS_END)
            print_statement_errors(c, node);
        else print_expression_errors(c, node);
    }
}
