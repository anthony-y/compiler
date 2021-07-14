#ifndef INTERP_h
#define INTERP_h

#include "common.h"
#include "arena.h"
#include "ast.h"
#include "type.h"
#include "context.h"

#define UINT16_MAX 0x7fff

// Opcodes
enum Op: u16 {
    // Load
    // arg1: destination
    // arg2: constant index
    // arg3: unused
    LOAD64,
    LOAD32,
    LOAD16,
    LOAD8,
    LOADPTR,

    IADD,
    ISUB,
    IMUL,
    IDIV,
    IMOD,
    INEG,
    FADD,
    FSUB,
    FMUL,
    FDIV,
    FMOD,
    FNEG,
    JMP,
    JMPZ,
    JMPNZ,
    CALL,
    PRINTREG,
    HALT,
};

//  8 bytes
// 64 bits
struct Instruction {
    Op  op; // u16
    u16 arg1;
    u16 arg2;
    u16 arg3;
};

struct Constant {
    union {
        s64 s64_;
        s32 s32_;
        s16 s16_;
        s8  s8_;
        void *ptr;
    };
    enum: u32 {
        S64,
        S32,
        S16,
        S8,
        PTR,
    } tag;
};

struct BytecodeBlock {
    // Start and end as indices into Interp::code
    u64 start = 0;
    u64 end   = 0;

    Constant *constants          = NULL;
    u16       constants_count    = 0;
    u16       constants_capacity = 64;
};

constexpr int REG_ZER = 0; // always zero
constexpr int REG_RET = 1; // return pointer
constexpr int REG_ACC = 2; // accumulator

struct Interp {
    Instruction *code          = NULL;
    u64          code_count    = 0;
    u64          code_capacity = 128;

    Constant *constants          = NULL;
    u16       constants_count    = 0;
    u16       constants_capacity = 64;

    u64 regs[UINT16_MAX];
};

void interp_run(Interp *);
void interp_free(Interp *);

struct Context;

Interp compile_to_bytecode(Context *ctx, Ast *ast, Module *in);

#endif
