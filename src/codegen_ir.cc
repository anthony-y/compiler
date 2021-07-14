#include "headers/bytecode.h"
#include "headers/ast.h"
#include "headers/common.h"
#include "headers/arena.h"
#include "headers/context.h"

#include <stdlib.h>
#include <assert.h>

void grow_constants_if_needed(Interp *interp);
u16 add_constant_64(Interp *interp, s64 n);
u16 add_constant_32(Interp *interp, s32 n);
u16 add_constant_16(Interp *interp, s16 n);
u16 add_constant_8(Interp *interp, s8 n);
u16 add_constant_ptr(Interp *interp, void *n);

void grow_code_if_needed(Interp *interp);
void addinstr(Interp *interp, Instruction i);

void emit_ir_proc(Interp *interp, AstProcedure *proc);

Interp compile_to_bytecode(Context *ctx, Ast *ast, Module *in) {
    Interp out;
    out.code      = (Instruction *)calloc(sizeof(Instruction), out.code_capacity);
    out.constants = (Constant *)calloc(sizeof(Constant), out.constants_capacity);

    for (u64 i = 0; i < ast->len; i++) {
        auto decl = (AstDecl *)ast->nodes[i];
        if (decl->expr && decl->expr->tag == Node_PROCEDURE) {
            auto proc = (AstProcedure *)decl->expr;
            emit_ir_proc(&out, proc);
        }
    }

    addinstr(&out, Instruction{Op::PRINTREG, REG_ZER, 0, 0}); // should print 0
    addinstr(&out, Instruction{Op::PRINTREG, REG_RET, 0, 0}); // should print a low value
    addinstr(&out, Instruction{Op::PRINTREG, REG_ACC, 0, 0}); // should print 0 also

    out.code[out.code_count].op = Op::HALT;
    return out;
}

void emit_ir_call(Interp *interp, AstExpr *call) {
    printf("doing call for line %ld\n", call->token.line);
    u16 index = add_constant_64(interp, interp->code_count);
    addinstr(interp, Instruction{Op::LOAD64, REG_RET, index, 0});
}

void emit_ir_expr(Interp *interp, AstExpr *expr) {
    switch (expr->tag) {
    case Node_CALL: {
        emit_ir_call(interp, expr);
    } break;
    default: break;
    }
}

void emit_ir_block(Interp *interp, AstBlock *block) {
    for (u64 i = 0; i < block->statements->len; i++) {
        AstStmt *s = (AstStmt *)block->statements->nodes[i];
        switch (s->tag) {
        case Node_BINARY: break;
        case Node_CALL: {
            auto call = (AstCallStmt *)s;
            emit_ir_call(interp, call->expr);
        } break;
        default: break;
        }
    }
}

void emit_ir_proc(Interp *interp, AstProcedure *proc) {
    if (!proc->block) return;
    assert(proc->block->tag == Node_BLOCK);
    emit_ir_block(interp, (AstBlock *)proc->block);
}




void grow_code_if_needed(Interp *interp) {
    if (interp->code_count+1 > interp->code_capacity) {
        interp->code_capacity *= 2;
        interp->code = (Instruction *)realloc(interp->code, interp->code_capacity);
    }
}

void addinstr(Interp *interp, Instruction i) {
    grow_code_if_needed(interp);
    interp->code[interp->code_count++] = i;
}

void grow_constants_if_needed(Interp *interp) {
    if (interp->constants_count+1 > interp->constants_capacity) {
        interp->constants_capacity *= 2;
        interp->constants = (Constant *)realloc(interp->constants, interp->constants_capacity);
    }
}

u16 add_constant_64(Interp *interp, s64 n) {
    grow_constants_if_needed(interp);
    interp->constants[interp->constants_count] = Constant{
        n,
        Constant::S64,
    }; 
    return interp->constants_count++;
}

u16 add_constant_32(Interp *interp, s32 n) {
    grow_constants_if_needed(interp);
    interp->constants[interp->constants_count] = Constant{
        n,
        Constant::S32,
    };
    return interp->constants_count++;
}

u16 add_constant_16(Interp *interp, s16 n) {
    grow_constants_if_needed(interp);
    interp->constants[interp->constants_count] = Constant{
        n,
        Constant::S16,
    }; 
    return interp->constants_count++;
}

u16 add_constant_8(Interp *interp, s8 n) {
    grow_constants_if_needed(interp);
    interp->constants[interp->constants_count] = Constant{
        n,
        Constant::S8,
    }; 
    return interp->constants_count++;
}

u16 add_constant_ptr(Interp *interp, void *n) {
    grow_constants_if_needed(interp);
    Constant c;
    c.ptr = n;
    c.tag = Constant::PTR;
    interp->constants[interp->constants_count] = c;
    return interp->constants_count++;
}
