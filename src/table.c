#include "headers/table.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

static unsigned int hash_key(char *key) {
	const unsigned int fnv_offset = 2166136261;
	const unsigned int fnv_prime  = 16777619;

	unsigned int hash = fnv_offset;
	for (; *key != 0; key++) {
		hash = hash * fnv_prime;
		hash = hash ^ *key;
	}

	return hash;
}

void init_table(Table *table) {
	table->capacity = 32;
	table->num_entries = 0;
	table->keys = calloc(table->capacity, sizeof(char*));
	table->values = calloc(table->capacity, sizeof(void*));
}

bool table_add(Table *table, char *key, void *value) {
	// Does having a 32-bit hash mean I can only address 32-bits worth of array indexes?
	unsigned int hash  = hash_key(key);
	long long int index = hash % table->capacity;

	if (index == 0) {
		printf("Full.\n");
		table->capacity *= 2;
		table->keys = realloc(table->keys, table->capacity);
		table->values = realloc(table->values, table->capacity);
		assert(table->keys && table->values);

		return false;
	}

	table->keys[index] = key;
	table->values[index] = value;
	return true;
}

void *table_get(Table *table, char *key) {
	unsigned int hash = hash_key(key);
	long long int index = hash % table->capacity;
	if (index == 0) {
		return NULL;
	}
	return table->values[index];
}

void free_table(Table *table) {
	free(table->values);
	free(table->keys);
}
