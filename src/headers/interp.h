#ifndef INTERP_h
#define INTERP_h

#include "common.h"
#include "ast.h"

typedef union Register {
    //union {
        void *ptr;
        s64 i64;
        u64 ui64;
        s32 i32;
        u32 iu32;
        s16 i16;
        u16 ui16;
        s8  i8;
        u8  ui8;
    //} _;
} Register;

enum {
    R0 = 1, R1, R2, R3, R4, R5, R6, R7,
    Registers_COUNT,
};

// Interpreter used for compile-time code execution
typedef struct Interp {
    Register regs[Registers_COUNT];
} Interp;

// A precompiled procedure which is ready to be
// invoked by the interpreter
typedef struct InterpProc {
    s32 *instrs;
} InterpProc;

void interp_init(Interp *);
int interp_run(Interp *, InterpProc *entry_point);

// Instructions
enum {
    LOADPTR,

    LOADS32,
    LOADU32,

    LOADS64,
    LOADU64,

    LOADS16,
    LOADU16,

    LOADS8,
    LOADU8,

    HALT,
};

#endif
