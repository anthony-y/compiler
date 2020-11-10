// A fixed size "arena" allocator/linear allocator.
// Used throughout the compiler, the parser uses it to
// allocate AST nodes, for example.
// Adapted from Jonathan Blow's arena allocator, 
// used in the Jai compiler.
//
// The commented out code at the bottom of the file
// is my naive old approach, without alignment.
#include "headers/arena.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

bool arena_init(Arena *a, u64 capacity, u64 elem_size, int alignment) {
    u64 space = (capacity * elem_size) + (alignment * capacity);
    u8 *tmp = (u8 *)malloc(space);
    if (!tmp) {
        return false;
    }

    a->block      = tmp;
    a->alignment  = alignment;
    a->pos        = 0;
    a->bytes_left = space;
    a->capacity   = capacity;

    return true;
}

void arena_clear(Arena *a) {
    memset(a->block, 0, a->pos);
    a->pos = 0;
    a->bytes_left = a->capacity;
}

void arena_free(Arena *a) {
    free(a->block);
    a->block = NULL;

    a->pos        = 0;
    a->alignment  = 0;
    a->bytes_left = 0;
}

void *arena_alloc(Arena *a, u64 size) {
    u64 extra = a->alignment - (size % a->alignment);
    size += extra;

    assert(a->bytes_left >= size);
    void *ret = (void *)(a->block + a->pos);
    a->pos += size;
    a->bytes_left -= size;
    return ret;
}
