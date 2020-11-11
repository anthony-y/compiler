#ifndef TABLE_h
#define TABLE_h

#include "common.h"

typedef struct Table {
	char **keys;
	void **values;
	u64 num_entries;
	u64 capacity;
} Table;

void init_table(Table *table);
bool table_add(Table *table, char *key, void *value);
void *table_get(Table *table, char *key);
void free_table(Table *table);

#endif
