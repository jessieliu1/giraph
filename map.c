#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* A single node of the adjacency list for a single vertex. */
struct map_node {
	struct map_node *next;
	unsigned int key;
	void *value;
};

struct map_node *get_node(unsigned int key, void *value) {
	struct map_node *out = (struct map_node *) malloc(sizeof(struct map_node));
	out->key = key;
	out->value = value;
	out->next = NULL;
	return out;
}

/* returns pointer to map */
void *make_map() {
	/* (size - 1) is hashable. 432 is the the 83rd prime number (431) plus 1. */
	int default_size = 432;
	struct map_node **map = (struct map_node **) malloc(sizeof(struct map_node *) * default_size);
	memset(map, 0, sizeof(struct map_node *) * default_size);
	int *size = (int *) malloc(sizeof(int));
	*size = default_size;
	map[0] = get_node(0, size);
	return map;
}

/* hash function: hash 0 is reserved for table size */
int hash(unsigned int in, int size) {
	return 1 + ((in * 997) % (size - 1));
}

void put(void *map_in, int *key, void *value) {
	struct map_node **map = (struct map_node **) map_in;
	int size = *((int *) map[0]->value);
	unsigned int for_hash = (unsigned int) key;
	int hash_val = hash(for_hash, size);

	struct map_node *bucket = map[hash_val];
	if (!bucket) {
		map[hash_val] = get_node((unsigned int) key, value);
		return;
	}
	while (bucket->next) {
		bucket = bucket->next;
	}
	bucket->next = get_node((unsigned int) key, value);
}

/* returns NULL if not found */
void *get(void *map_in, int *key) {
	struct map_node **map = (struct map_node **) map_in;
	int size = *((int *) map[0]->value);
	unsigned int for_hash = (unsigned int) key;
	int hash_val = hash(for_hash, size);
	struct map_node *bucket = map[hash_val];
	if (!bucket) {
		return NULL;
	}
	while (bucket) {
		if (bucket->key == (unsigned int) key) {
			return bucket->value;
		}
		bucket = bucket->next;
	}
	return NULL;
}

int contains_key(void *map_in, int *key) {
	struct map_node **map = (struct map_node **) map_in;
	int size = *((int *) map[0]->value);
	unsigned int for_hash = (unsigned int) key;
	int hash_val = hash(for_hash, size);
	struct map_node *bucket = map[hash_val];
	if (!bucket) {
		return 0;
	}
	while (bucket) {
		if (bucket->key == (unsigned int) key) {
			return 1;
		}
		bucket = bucket->next;
	}
	return 0;
}

/*
int main() {
	void *map = make_map();
	int *a = (int *) malloc(sizeof(int));
	int *b = (int *) malloc(sizeof(int));
	int *c = (int *) malloc(sizeof(int));
	int *d = (int *) malloc(sizeof(int));
	int * e = (int *) 0;

	int *a_val = (int *) malloc(sizeof(int));
	int *b_val = (int *) malloc(sizeof(int));
	int *c_val = (int *) malloc(sizeof(int));
	int *d_val = (int *) malloc(sizeof(int));
	int *e_val = (int *) malloc(sizeof(int));

	*a_val = 1;
	*b_val = 2;
	*c_val = 3;
	*d_val = 4;
	*e_val = 50;

	put(map, a, a_val);
	put(map, b, b_val);
	put(map, c, c_val);
	put(map, d, d_val);
	put(map, e, e_val);

	printf("%d %d %d %d\n", *((int *) get(map, a)), *((int *) get(map, b)), *((int *) get(map, c)), *((int *) get(map, d))); 

	printf("%d %d %d %d\n", contains_key(map, a), contains_key(map, 0), contains_key(map, c), contains_key(map, (int *) 42)); 
}*/