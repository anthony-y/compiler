#ifndef INTERP_h
#define INTERP_h

#include "common.h"
#include "arena.h"
#include "ast.h"
#include "type.h"

typedef struct Interp {
    char **call_stack; // stb_sb
    u8 *code; // stb_sb
    u8 *stack;
} Interp;

void interp_run(Interp *);
void interp_free(Interp *);

struct Context;

Interp compile_to_bytecode(struct Context *ctx, Ast *ast);

// Opcodes
enum {
    STACK_POP,

    IPUSH,
    IPUSH_REF,
    IDEREF,

    IADD,
    ISUB,
    IMUL,
    IDIV,
    IMOD,

    INEG,

    JMP,
    JMPZ,
    JMPNZ,

    CALL,
    
    PRINT_VALUE_PTR,
    PRINT_VALUE_INT,

    FPUSH,
    PPUSH,

    HALT,
};

#endif
