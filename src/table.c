#include "headers/table.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

u32 table_hash_key(char *key) {
	static const u32 fnv_offset = 2166136261;
	static const u32 fnv_prime  = 16777619;

	u32 hash = fnv_offset;
	for (; *key != 0; key++) {
		hash = hash * fnv_prime;
		hash = hash ^ *key;
	}

	return hash;
}

void init_table(Table *table) {
	table->capacity = 32;
	table->num_entries = 0;
	table->pairs = calloc(table->capacity, sizeof(TablePair));
}

TableIter table_get_iterator(const Table *t) {
	TableIter it;
	it.num_entries = t->num_entries;
	it.pairs = malloc(it.num_entries * (sizeof(char*) + sizeof(void*)));
	assert(it.pairs);
	
	u64 it_i = 0;
	for (u64 i = 0; i < t->capacity; i++) {
		TablePair p = t->pairs[i];
		if (p.key == NULL && p.value == NULL) continue;
		it.pairs[it_i].key = p.key;
		it.pairs[it_i++].value = p.value;
		if (p.collisions) {
			for (u64 j = 0; j < p.num_collisions; j++) {
				it.pairs[it_i].key = p.collisions[j].key;
				it.pairs[it_i++].value = p.collisions[j].value;
			}
		}
	}
	return it;
}

bool table_add(Table *table, char *key, void *value) {
	// Does having a 32-bit hash mean I can only address 32-bits worth of array indexes?
	u32 hash  = table_hash_key(key);
	u64 index = hash % table->capacity;
	if (index == 0) {
		// printf("Full, resizing\n");
		table->capacity *= 2;
		table->pairs = realloc(table->pairs, table->capacity);
		assert(table->pairs);
		hash = table_hash_key(key);
		index = hash % table->capacity;
	}
	if (table->pairs[index].value != NULL && table->pairs[index].key != NULL) {
		TablePair *existing_pair = &table->pairs[index];
		// printf("table_get: already a pair for %s: %s.\n", key, existing_pair->key);
		if (!existing_pair->collisions) {
			existing_pair->collisions = calloc(existing_pair->collision_capacity, sizeof(void*) + sizeof(char *));
		}
		existing_pair->collisions[existing_pair->num_collisions].key = key;
		existing_pair->collisions[existing_pair->num_collisions++].value = value;
		table->num_entries++;
		return true;
	}
	table->pairs[index].key = key;
	table->pairs[index].value = value;
	table->num_entries++;
	return true;
}

void *table_get(Table *table, char *key) {
	u32 hash = table_hash_key(key);
	u64 index = hash % table->capacity;
	if (index == 0) return NULL;

	TablePair *pair = &table->pairs[index];
	if (pair->value == NULL && pair->key == NULL) {
		return NULL;
	} else if (strcmp(key, pair->key) == 0) {
		return pair->value;
	} else if (!pair->collisions) {
		return NULL;
	}

	for (int i = 0; i < pair->num_collisions; i++) {
		if (strcmp(key, pair->collisions[i].key) == 0) {
			// printf("Found %s in collision list of %s\n", key, pair->key);
			return pair->collisions[i].value;
		}
	}

	return NULL;
}

void free_table(Table *table) {
	for (u64 i = 0; i < table->capacity; i++) {
		if (table->pairs[i].key == NULL && table->pairs[i].value == NULL) continue;
		TablePair *p = &table->pairs[i];
		if (p->collisions) free(p->collisions);
	}
	free(table->pairs);
}
