#include "headers/common.h"
#include "headers/bytecode.h"
#include "headers/context.h"
#include "headers/ast.h"

#include <stdlib.h>
#include <assert.h>

#include "headers/stb/stb_ds.h"

void interp_free(Interp *interp) {
    stbds_arrfree(interp->reg);
    stbds_arrfree(interp->call_stack);
    stbds_arrfree(interp->code);
    arena_free(&interp->data);
}

static void interp_add_reg(Interp *interp, Register reg) {
    stbds_arrpush(interp->reg, reg);
}

static inline u64 interp_reserve_reg(Interp *interp) {
    return stbds_arrlenu(interp->reg);
}

static void interp_pop_proc(Interp *interp) {
    stbds_arrpop(interp->call_stack);
}

static void emit_instruction(Interp *interp, u8 op, u64 src, u64 dest) {
    Instruction i;
    i.op = op;
    i.src = src;
    i.dest = dest;
    stbds_arrpush(interp->code, i);
}

static void compile_top_level_var_to_bytecode(Context *ctx, Interp *interp, AstVar *var) {
    if (!(var->flags & VAR_IS_INITED)) return;

    Register reg = (Register){0};
    u64 register_index = interp_reserve_reg(interp);

    AstExpr *value = var->value;
    if (value->tag > Expr_LITERALS_START && value->tag < Expr_LITERALS_END) {
        AstLiteral *literal = &value->as.literal;
        switch (value->tag) {
        case Expr_INT: reg.integer = literal->data.integer; break;
        }
        interp_add_reg(interp, reg);
        var->register_index = register_index;
        return;
    }

    switch (value->tag) {
    case Expr_NAME: {
        Name *name = value->as.name;
        u64 from = ((AstVar *)name->resolved_decl)->register_index;
        emit_instruction(interp, COPY_REGISTER, from, register_index);
    } break;
    default: assert(false);
    }
    interp_add_reg(interp, reg);
    var->register_index = register_index;
}

Interp compile_to_bytecode(Context *ctx, Ast *ast) {
    Interp interp;
    arena_init(&interp.data, 1024, 1, 8); // TODO make dynamic
    interp.rP         = (Register){0};
    interp.code       = NULL;
    interp.call_stack = NULL;
    interp.reg        = NULL;

    for (int i = 0; i < ast->len; i++) {
        assert(is_decl(ast->nodes[i]));
        AstDecl *decl = (AstDecl *)ast->nodes[i];

        // This declaration was never used, so we don't need to compile it.
        if (decl->status == Status_UNRESOLVED) continue;

        switch (decl->tag) {
        case Decl_VAR:
            compile_top_level_var_to_bytecode(ctx, &interp, (AstVar *)decl);
            break;
        case Decl_PROC:
            //compile_proc_to_bytecode(&bc, (AstProcedure *)decl);
            break;
        case Decl_TYPEDEF:
            break;
        }
    }

    u64 nreg = stbds_arrlenu(interp.reg);
    for (int i = 0; i < nreg; i++) {
        emit_instruction(&interp, IPRINT_REG, i, 0);
    }
    emit_instruction(&interp, HALT, 0, 0);

    return interp;
}
