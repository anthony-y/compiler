// bytecode_interp.c contains code which executes bytecode instructions
// which were generated by code in bytecode_interp.c.
#include "headers/bytecode.h"
#include "headers/context.h"

#include "headers/stb/stb_ds.h"

#if 0
static void interp_push_proc(Interp *interp, AstProcedure *proc) {
    stbds_arrpush(interp->call_stack, ((Name *)proc->name)->text);
}

static Value interp_pop(Interp *interp) {
    Value value = stbds_arrlast(interp->stack);
    stbds_arrpop(interp->stack);
    return value;
}

static void interp_push_int(Interp *interp, u64 i) {
    stbds_arrpush(interp->stack, (Value){.integer=i});
}

static void interp_push_ptr(Interp *interp, void *p) {
    stbds_arrpush(interp->stack, (Value){.ptr=p});
}

static void interp_push_float(Interp *interp, double f) {
    stbds_arrpush(interp->stack, (Value){.floating=f});
}

static void print_instructions(Interp *interp) {
    for (int i = 0; i < stbds_arrlen(interp->code); i++) {
        Instruction instr = interp->code[i];
        switch (instr.op) {
        case IPUSH: printf("IPUSH"); break;
        case FPUSH: printf("FPUSH"); break;
        case PPUSH: printf("PPUSH"); break;
        case STACK_POP: printf("STACK_POP"); break;
        case IPUSH_REF: printf("IPUSH_REF"); break;
        case IDEREF: printf("IDEREF"); break;
        case IADD: printf("IADD"); break;
        case ISUB: printf("ISUB"); break;
        case IMUL: printf("IMUL"); break;
        case IDIV: printf("IDIV"); break;
        case IMOD: printf("IMOD"); break;
        case INEG: printf("INEG"); break;
        case JMP: printf("JMP"); break;
        case JMPZ: printf("JMPZ"); break;
        case JMPNZ: printf("JMPNZ"); break;
        case CALL: printf("CALL"); break;
        case PRINT_VALUE_PTR: printf("IPRINT_REG"); break;
        case PRINT_VALUE_INT: printf("PRINT_VALUE_INT"); break;
        case HALT: printf("HALT"); break;
        }
        printf(" %ld\n", instr.arg);
    }
}

void interp_run(Interp *interp) {
    print_instructions(interp);

    Instruction *ip = interp->code;
    while (ip) {
        switch (ip->op) {
        case HALT:
            return;

        case IPUSH:
            interp_push_int(interp, ip->arg);
            break;
        case PPUSH:
            interp_push_ptr(interp, (void *)ip->arg);
            break;
        case FPUSH:
            interp_push_float(interp, (double)ip->arg);
            break;
        
        case IPUSH_REF: {
            Value value = interp_pop(interp);
            interp_push_ptr(interp, &value.integer);
            // TODO this may be problematic, if a value is removed from the stack
            // a pointer to it may remain.
        } break;
        case IDEREF: {
            Value value = interp_pop(interp);
            interp_push_int(interp, *(u64 *)value.ptr);
        } break;

        case IADD: {
            u64 first  = interp_pop(interp).integer;
            u64 second = interp_pop(interp).integer;
            interp_push_int(interp, first + second);
        } break;
        case ISUB:
            
            break;
        case IMUL:
            
            break;
        case IDIV:
            
            break;
        case IMOD:
            
            break;
        
        case INEG:
            
            break;

        case CALL: {
        } break;

        case PRINT_VALUE_PTR: {
            Value value = interp_pop(interp);
            printf("%p\n", value.ptr);
        } break;
        case PRINT_VALUE_INT: {
            Value value = interp_pop(interp);
            printf("%lu\n", value.integer);
        } break;
        }
        ip++;
    }
}
#endif