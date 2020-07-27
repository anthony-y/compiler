#ifndef ARENA_h
#define ARENA_h

#include "common.h"

typedef struct {
    u8 *block;
    u64 pos;
    u64 bytes_left;
    u64 capacity; // just for book-keeping, don't modify after arena_init
    int alignment;
} Arena;

bool arena_init(Arena *, u64 capacity, u64 elem_size, int alignment);
bool arena_grow(Arena *);
void arena_free(Arena *);
void arena_clear(Arena *);
void *arena_alloc(Arena *, u64);

#endif
