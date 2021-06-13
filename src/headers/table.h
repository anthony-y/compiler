#ifndef TABLE_h
#define TABLE_h

#include "common.h"

typedef struct TablePair {
	char *key;
	void *value;
	struct {
		char *key;
		void *value;
	} *collisions;
	u64 num_collisions;
	u64 collision_capacity;
} TablePair;

typedef struct Table {
	TablePair *pairs;
	u64 num_entries;
	u64 capacity;
} Table;

typedef struct FlatTable {
	struct{char *key; void *value;} *pairs;
	u64 num_entries;
} FlatTable;

u32 table_hash_key(char *key);
void table_init(Table *table);
bool table_add(Table *table, char *key, void *value);
void *table_get(Table *table, char *key);
FlatTable table_flatten(const Table *);
char **table_get_keys(const Table *);
void table_free(Table *table);

#endif
