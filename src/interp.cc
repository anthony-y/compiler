#include "headers/bytecode.h"

#include <stdlib.h>
#include <assert.h>

void interp_run(Interp *interp) {
    assert(interp->code);
    for (u64 i = 0; i < interp->code_count; i++) {
        Instruction instr = interp->code[i];
        switch (instr.op) {
        case Op::HALT: break;

        case Op::LOAD64: {
            u16 target      = instr.arg1;
            u16 value_index = instr.arg2;
            interp->regs[target] = interp->constants[value_index].s64_;
        } break;
        case Op::LOAD32: {
            u16 target      = instr.arg1;
            u16 value_index = instr.arg2;
            interp->regs[target] = interp->constants[value_index].s32_;
        } break;
        case Op::LOAD16: {
            u16 target      = instr.arg1;
            u16 value_index = instr.arg2;
            interp->regs[target] = interp->constants[value_index].s16_;
        } break;
        case Op::LOAD8: {
            u16 target      = instr.arg1;
            u16 value_index = instr.arg2;
            interp->regs[target] = interp->constants[value_index].s8_;
        } break;
        case Op::LOADPTR: {
            u16 target      = instr.arg1;
            u16 value_index = instr.arg2;
            interp->regs[target] = (u64)interp->constants[value_index].ptr;
        } break;
        case Op::PRINTREG: {
            u16 reg = interp->regs[instr.arg1];
            printf("reg%d = %d\n", instr.arg1, reg);
        } break;

        default: break;
        }
    }
}

void interp_free(Interp *interp) {
    free(interp->code);
    free(interp->constants);
}
