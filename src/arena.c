#include "headers/arena.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

bool arena_init(Arena *a, u64 capacity, u64 elem_size, int alignment) {
    u64 space = (capacity * elem_size) + (alignment * capacity);
    u8 *tmp = (u8 *)malloc(space);
    if (!tmp) return false;

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


#if 0
bool arena_grow(Arena *pool) {
    pool->capacity = (pool->capacity * 2) + 8;

    void *tmp = realloc(pool->block, pool->capacity);
    if (!tmp) {
        fprintf(stderr, "Reallocation failed\n");
        free(pool->block);
        return false;
    }

    pool->block = tmp;

    return true;
}

void arena_init(Arena *pool, u64 capacity, bool fixed_size) {
    pool->block = calloc(capacity, 1);

    pool->capacity = capacity;
    pool->pos      = 0;

    pool->is_fixed_size = fixed_size;
    pool->resize_callback = arena_grow;
}

void arena_free(Arena *pool) {
    free(pool->block);

    pool->capacity = 0;
    pool->pos      = 0;

    pool->resize_callback = NULL;
}

/* TODO (anthony) : alignment */
void *arena_alloc(Arena *pool, u64 size) {
    if (!pool->is_fixed_size && (pool->pos + size > pool->capacity)) {
        pool->resize_callback(pool);
    }

    void *block = (void *)(pool->block + pool->pos);
    pool->pos += size;

    return block;
}
#endif
