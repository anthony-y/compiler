#include "headers/interp.h"

void interp_init(Interp *interp) {

}

int interp_run(Interp *interp, InterpProc *proc) {
    if (!proc || !proc->instrs) return 0; // TODO remove
    s32 *ip = proc->instrs;

    while (ip) {
        s32 instr = *ip++;
        switch (instr) {
            case HALT: break;

            case LOADS32: {
                s64 reg = *ip++;
                Register r;
                r.i32 = *ip++;
                interp->regs[reg] = r;
            } break;
        }
    }

    return 0; // success, but no return value
}
