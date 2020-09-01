#ifndef INTERP_h
#define INTERP_h

#include "common.h"
#include "arena.h"
#include "ast.h"

typedef union Register {
    void *ptr;
    u64 integer;
    double floating;
} Register;

typedef struct Instruction {
    u8  op;
    u64 src;
    u64 dest;
} Instruction;

typedef struct Interp {
    char **call_stack; // stb_sb
    Register *reg; // stb_sb
    Register rP; // procedure parameter count
    Arena data; // aka constants pool, where data is kept while its not in a register
    Instruction *code; // stb_sb
} Interp;

void interp_run(Interp *);
void interp_free(Interp *);

struct Context;

Interp compile_to_bytecode(struct Context *ctx, Ast *ast);

// Opcodes
enum {
    SET_PTR_IMMEDIATE, // set a register to a pointer value

    ISET_IMMEDIATE,

    ISET_REFERENCE, // sets dest to the address of src

    IDEREFERENCE_AND_COPY,

    IADD,
    ISUB,
    IMUL,
    IDIV,
    IMOD,

    JMP,
    JMPZ,
    JMPNZ,

    COPY_REGISTER,

    CALL,

    IPRINT_REG,
    PPRINT_REG,

    HALT,
};

#endif
