#include "headers/bytecode.h"
#include "headers/context.h"

#include "headers/stb/stb_ds.h"

static void interp_push_proc(Interp *interp, AstProcedure *proc) {
    stbds_arrpush(interp->call_stack, ((Name *)proc->name)->text);
}

void interp_run(Interp *interp) {
    Instruction *ip = interp->code;

    while (ip) {
        Instruction instr = *ip++;
        switch (instr.op) {
        case HALT:
            return;
    
        case ISET_IMMEDIATE:
            interp->reg[instr.dest].integer = instr.src;
            break;

        case ISET_REFERENCE:
            interp->reg[instr.dest].ptr = &(interp->reg[instr.src].integer);
            break;

        case IADD:
            interp->reg[instr.dest].integer = interp->reg[instr.dest].integer + interp->reg[instr.src].integer;
            break;
        case ISUB:
            interp->reg[instr.dest].integer = interp->reg[instr.dest].integer - interp->reg[instr.src].integer;
            break;
        case IMUL:
            interp->reg[instr.dest].integer = interp->reg[instr.dest].integer * interp->reg[instr.src].integer;
            break;
        case IDIV:
            interp->reg[instr.dest].integer = interp->reg[instr.dest].integer / interp->reg[instr.src].integer;
            break;
        case IMOD:
            interp->reg[instr.dest].integer = interp->reg[instr.dest].integer % interp->reg[instr.src].integer;
            break;

        case COPY_REGISTER:
            interp->reg[instr.dest] = interp->reg[instr.src];
            break;
        case IDEREFERENCE_AND_COPY:
            interp->reg[instr.dest].integer = *((u64 *)interp->reg[instr.src].ptr);
            break;

        case CALL: {
            int procedure_offset = instr.src;
            u64 argument_count = instr.dest;
        } break;

        case IPRINT_REG:
            printf("%lu\n", (interp->reg[instr.src].integer));
            break;
        case PPRINT_REG:
            printf("%p\n", (interp->reg[instr.src].ptr));
            break;
        }
    }
}
