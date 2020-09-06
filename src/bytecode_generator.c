// bytecode_generator.c contains code which transforms an abstract syntax tree
// into bytecode instructions which can be executed by the interpreter in
// bytecode_interp.c.
#include "headers/common.h"
#include "headers/bytecode.h"
#include "headers/context.h"
#include "headers/ast.h"

#include <stdlib.h>
#include <assert.h>

#include "headers/stb/stb_ds.h"

#if 0
void interp_free(Interp *interp) {
    stbds_arrfree(interp->call_stack);
    stbds_arrfree(interp->code);
    stbds_arrfree(interp->stack);
}

static void interp_pop_proc(Interp *interp) {
    stbds_arrpop(interp->call_stack);
}

static void emit_instruction(Interp *interp, u8 op, u64 arg) {
    Instruction i;
    i.op = op;
    i.arg = arg;
    stbds_arrpush(interp->code, i);
}

static void compile_top_level_var_to_bytecode(Interp *, AstVar *);

static void compile_expression_to_bytecode(Interp *interp, AstExpr *expr) {
    if (expr->tag > Expr_LITERALS_START && expr->tag < Expr_LITERALS_END) {
        AstLiteral *literal = &expr->as.literal;
        switch (expr->tag) {
        case Expr_INT: emit_instruction(interp, IPUSH, literal->data.integer); break;
        case Expr_FLOAT: emit_instruction(interp, IPUSH, literal->data.floating); break;
        case Expr_NULL: emit_instruction(interp, PPUSH, 0); break;
        case Expr_STRING: {
            // TODO how do I do this properly?
            char *data = literal->data.string;

            // -2 for the quotes
            // TODO: hack: fix this at lexer level
            u64 len = expr_tok((AstExpr *)literal).length-2;

            emit_instruction(interp, IPUSH, len);
            emit_instruction(interp, PPUSH, (u64)data);
        } break;
        }
        return;
    }
    switch (expr->tag) {
    case Expr_NAME: {
        Name *name = expr->as.name;
        AstVar *var = (AstVar *)name->resolved_decl;
        if (var->value->resolved_type->kind == Type_PRIMITIVE && var->value->resolved_type->data.signage == Signage_NaN) { // TODO include ctx to compare against type_string.

            // for strings, pop the size and pointer
            emit_instruction(interp, STACK_POP, 0);
        }
        emit_instruction(interp, STACK_POP, 0);
    } break;
    case Expr_UNARY: {
        AstUnary *unary = &expr->as.unary;
        compile_expression_to_bytecode(interp, unary->expr);
        if (unary->op == Token_MINUS) emit_instruction(interp, INEG, 0);
        else if (unary->op == Token_CARAT) emit_instruction(interp, IPUSH_REF, 0);
        else if (unary->op == Token_STAR) emit_instruction(interp, IDEREF, 0);
    } break;
    case Expr_BINARY: {
        AstBinary *binary = &expr->as.binary;
        if (binary->op == Token_PLUS) {
            compile_expression_to_bytecode(interp, binary->right);
            compile_expression_to_bytecode(interp, binary->left);
            emit_instruction(interp, IADD, 0);
        }
    } break;
    default: {
        printf("%d\n", expr->tag);
        assert(false);
    }
    }
}

static void compile_top_level_var_to_bytecode(Interp *interp, AstVar *var) {
    if (!(var->flags & VAR_IS_INITED)) return;
    compile_expression_to_bytecode(interp, var->value);
}

static void compile_proc_to_bytecode(Interp *interp, AstProcedure *proc) {
    
}

#endif
Interp compile_to_bytecode(Context *ctx, Ast *ast) {
    Interp interp;
    interp.code       = NULL;
    interp.call_stack = NULL;
    interp.stack      = NULL;

    for (int i = 0; i < ast->len; i++) {
        assert(is_decl(ast->nodes[i]));
        AstDecl *decl = (AstDecl *)ast->nodes[i];

        // This declaration was never used, so we don't need to compile it.
        if (decl->status == Status_UNRESOLVED) continue;

        switch (decl->tag) {
        case Decl_VAR:
            //compile_top_level_var_to_bytecode(&interp, (AstVar *)decl);
            break;
        case Decl_PROC:
            //compile_proc_to_bytecode(&bc, (AstProcedure *)decl);
            break;
        case Decl_TYPEDEF:
            break;
        }
    }

    //emit_instruction(&interp, PRINT_VALUE_INT, 0);
    //emit_instruction(&interp, PRINT_VALUE_PTR, 0);
    //emit_instruction(&interp, HALT, 0);

    return interp;
}
