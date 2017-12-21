/* Authors: 
Daniel Benett deb2174
Seth Benjamin sjb2190
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* for use with generic graph/node types */
union data_type {
	int i;
	float f;
	char *s;
	void *v;
};

/* so we can cast void *'s to floats/ints in maps without triggering undefined
   behavior 
   shoulda done the whole map thing with a union but hindsight is 20/20 */
union extract_float {
    float vf;
    void *vp;
};
union extract_int {
    int vi;
    void *vp;
};

/* Terminology-wise, we've painted ourselves into a corner here. 
   Elsewhere in in this project, "node" refers to a single node in a graph.
   That is NOT true in this file. In this file, "vertex" refers to a node in
   a graph, and "node" refers to a single node of a linked list. */

//////////////////////// STRUCTS ////////////////////////

/* A single node of the adjacency list for a single vertex. */
struct adj_list_node {
	struct vertex_list_node *vertex;
	struct adj_list_node *next;
	int weight;
};

/* a single node in the edge list containing relevant edge information */
struct edge_list_node {
	struct vertex_list_node *from;
	struct vertex_list_node *to;
	struct edge_list_node *next;
	int weight;	
};

/* A single vertex in a graph. */
struct vertex_list_node {
	void *data;
	struct adj_list_node *adjacencies;
	struct vertex_list_node *next;
};

/* A graph. */
struct graph {
	struct vertex_list_node *head;
};

/* node for queue */
struct queue_list_node {
	struct vertex_list_node *v;
	struct queue_list_node *next;
};

/* node for stack */
struct stack_list_node {
	struct vertex_list_node *v;
	struct stack_list_node *next;
};

/* node for map */
struct map_node {
	struct map_node *next;
	unsigned int key;
	void *value;
};

////////////////////// END STRUCTS //////////////////////


////////////////////////// MAP //////////////////////////
/* A single node of the adjacency list for a single vertex. */

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

void free_map(void *map_in) {
	struct map_node **map = (struct map_node **) map_in;
	int size = *((int *) map[0]->value);
	for (int i = 0; i < size; i++) {
		struct map_node *bucket = map[i];
		if (bucket) {
			struct map_node *next = bucket->next;
			free(bucket);
			while (next) {
				bucket = next;
				next = bucket->next;
				free(bucket);
			}
		}
	}
	free(map);
}

/* hash function: hash 0 is reserved for table size */
int hash(unsigned int in, int size) {
	return 1 + ((in * 997) % (size - 1));
}

/* if putting into a key already in map, replace value */
void put(void *map_in, void *key, void *value) {
	struct map_node **map = (struct map_node **) map_in;
	int size = *((int *) map[0]->value);
	unsigned int for_hash = (unsigned int) key;
	int hash_val = hash(for_hash, size);
	struct map_node *bucket = map[hash_val];
	if (!bucket) {
		map[hash_val] = get_node((unsigned int) key, value);
		return;
	}
	if (bucket->key == (unsigned int) key) {
		bucket->value = value;
		return;
	}
	while (bucket->next) {
		if (bucket->key == (unsigned int) key) {
			bucket->value = value;
			return;
		}
		bucket = bucket->next;
	}
	bucket->next = get_node((unsigned int) key, value);
}

/* returns NULL if not found */
void *get(void *map_in, void *key) {
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

int contains_key(void *map_in, void *key) {
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

/* The following functions implement put() for the built-in types in giraph. */

void put_int(void *map_in, void *key, int value) {
	union extract_int ei;
	ei.vi = value;
	put(map_in, key, ei.vp);
}

void put_int_ptr(void *map_in, void *key, void *value) {
	put(map_in, key, (void *) value);
}

void put_char_ptr(void *map_in, void *key, char *value) {
	put(map_in, key, (void *) value);
}

void put_float(void *map_in, void *key, float value) {
	union extract_float ef;
	ef.vf = value;
	put(map_in, key, ef.vp);
}

/* The following functions implement get() for the built-in types in giraph. */

int get_int(void *map_in, void *key) {
	union extract_int ei;
	ei.vp = get(map_in, key);
	return ei.vi;
}

int *get_int_ptr(void *map_in, void *key) {
	return (int *) get(map_in, key);
}

char *get_char_ptr(void *map_in, void *key) {
	return (char *) get(map_in, key);
}

float get_float(void *map_in, void *key) {
	union extract_float ef;
	ef.vp = get(map_in, key);
	return ef.vf;
}




//////////////////////// END MAP ////////////////////////


////////////////////// EDGE METHODS /////////////////////

void *edge_from(void *e) {
	return ((struct edge_list_node *) e)->from->data;
}

void *edge_to(void *e) {
	return ((struct edge_list_node *) e)->to->data;
}

int edge_weight(void *e) {
	return ((struct edge_list_node *) e)->weight;
}

void undirected_edge_set_weight(void *e_in, int new_weight) {
	struct edge_list_node *e = (struct edge_list_node *) e_in;
	struct adj_list_node *from_adj = e->from->adjacencies;
	struct adj_list_node *to_adj = e->to->adjacencies;
	while (from_adj) {
		if (from_adj->vertex == e->to) {
			from_adj->weight = new_weight;
		}
		from_adj = from_adj->next;
	}
	while (to_adj) {
		if (to_adj->vertex == e->from) {
			to_adj->weight = new_weight;
		}
		to_adj = to_adj->next;
	}
	((struct edge_list_node *) e)->weight = new_weight;
}

void edge_set_weight(void *e_in, int new_weight) {
	struct edge_list_node *e = (struct edge_list_node *) e_in;
	struct adj_list_node *from_adj = e->from->adjacencies;
	while (from_adj) {
		if (from_adj->vertex == e->to) {
			from_adj->weight = new_weight;
		}
		from_adj = from_adj->next;
	}
	((struct edge_list_node *) e)->weight = new_weight;
}

//////////////////// END EDGE METHODS ///////////////////


////////////////////// NODE METHODS /////////////////////

/* Change the data stored in a node<int> data pointer. */
void set_data_int(void *data_ptr, int data_val) {
	((union data_type *) data_ptr)->i = data_val;
}

/* Change the data stored in a node<float> data pointer. */
void set_data_float(void *data_ptr, float data_val) {
	((union data_type *) data_ptr)->f = data_val;
}

/* Change the data stored in a node<string> data pointer. */
void set_data_char_ptr(void *data_ptr, char *data_val) {
	((union data_type *) data_ptr)->s = data_val;
}

/* Change the data stored in a data pointer for all other node types. */
void set_data_void_ptr(void *data_ptr, void *data_val) {
	/* *((void **) data_ptr) = data_val; */
	((union data_type *) data_ptr)->v = data_val;
}

/* Get data stored in a node<int> data pointer. */
int get_data_int(void *data_ptr) {
	return ((union data_type *) data_ptr)->i;
}

/* Get data stored in a node<float> data pointer. */
float get_data_float(void *data_ptr) {
	return ((union data_type *) data_ptr)->f;
}

/* Get data stored in a node<string> data pointer. */
char *get_data_char_ptr(void *data_ptr) {
	return ((union data_type *) data_ptr)->s;
}

/* Get data stored in a data pointer for all other node types. */
void *get_data_void_ptr(void *data_ptr) {
	/* return *((void **) data_ptr); */
	return ((union data_type *) data_ptr)->v;
}

//////////////////// END NODE METHODS ///////////////////


////////////////// GRAPH HELPER METHODS /////////////////
/* Find and return the vertex_list_node associated with a data pointer.
   Returns null if there is none. */
void *find_vertex(void *g_in, void *data_ptr) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		if (vertex->data == data_ptr) {
			return vertex;
		}
		vertex = vertex->next;
	}
	return NULL;
}

/* Returns a pointer to a new graph. */
void *new_graph() {
	struct graph *g = malloc(sizeof(struct graph));
	g->head = NULL;

	return (void *) g;
}

/* Allocate a new unique data pointer. */
void *new_data() {
	return (void *) malloc(sizeof(union data_type));
}

/* Add a new vertex to the end of the vertex list in a graph, and return a
   pointer to the new vertex. */
void *add_vertex(void *graph_ptr, void *data_ptr) {
	struct vertex_list_node *vertex = malloc(sizeof(struct vertex_list_node));
	vertex->data = data_ptr;
	vertex->adjacencies = NULL;
	vertex->next = NULL;

	struct graph *g = (struct graph *) graph_ptr;
	if (g->head == NULL) {
		g->head = vertex;
	} else {
		struct vertex_list_node *last_node = g->head;
		while (last_node->next) {
			last_node = last_node->next;
		}
		last_node->next = vertex;
	}

	return (void *) vertex;
}

/* Add a directed, weighted edge between two vertices */
void add_wedge(void *from_ptr, void *to_ptr, int w) {
	struct vertex_list_node *from = (struct vertex_list_node *) from_ptr;
	struct vertex_list_node *to = (struct vertex_list_node *) to_ptr;
	if (from->adjacencies == NULL) {
		from->adjacencies = malloc(sizeof(struct adj_list_node));
		from->adjacencies->vertex = to;
		from->adjacencies->weight = w;
		from->adjacencies->next = NULL;
	} else {
		struct adj_list_node *last_node = from->adjacencies;
		while (last_node->next) {
			last_node = last_node->next;
		}
		last_node->next = malloc(sizeof(struct adj_list_node));
		last_node->next->vertex = to;
		last_node->next->weight = w;
		last_node->next->next = NULL;
	}
}

/* Add a (directed) edge between two vertices. Give default weight 0. */
void add_edge(void *from_ptr, void *to_ptr) {
	add_wedge(from_ptr, to_ptr, 0);
}
//////////////// END GRAPH HELPER METHODS ///////////////


///////////////////// GRAPH METHODS /////////////////////

/* Given a graph and a data pointer, checks if the graph has a vertex associated
   with the data pointer, and if not, creates one and adds it to the graph.
   Corresponds to add_node method in giraph. */
void add_vertex_if_not_present(void *g_in, void *data_ptr) {
	struct graph *g = (struct graph *) g_in;
	if (find_vertex(g_in, data_ptr) == NULL) {
		add_vertex(g_in, data_ptr);
	}
}

/* Given a graph and a data pointer, finds the vertex in the graph associated
   with the data pointer and removes it from the vertex list and all adjacency
   lists. If no such vertex exists, does nothing.
   Corresponds to remove_node method in giraph. */
void remove_vertex(void *g_in, void *data_ptr) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *remove = (struct vertex_list_node *) find_vertex(g_in, data_ptr);
	if (remove == NULL) {
		return;
	}

	/* Iterate through all vertices and remove this vertex from any adjacency
	   lists. */
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		if (vertex->adjacencies) {
			/* if we need to remove the first adjacency, set vertex's
			   "adjacencies" pointer to be the next adjacency */
			struct adj_list_node *curr_e = vertex->adjacencies;
			if (curr_e->vertex == remove) {
				vertex->adjacencies = curr_e->next;
				free(curr_e); /* woooaaahh */
			} else {
				/* else, just remove appropriate adj_list_node from list
				   by reconnecting surrounding nodes */
				struct adj_list_node *prev_e = vertex->adjacencies;
				curr_e = prev_e->next;

				while (curr_e) {
					if (curr_e->vertex == remove) {
						prev_e->next = curr_e->next;
						free(curr_e);
						break;
					}
					prev_e = curr_e;
					curr_e = curr_e->next;
				}
			}
		}
		vertex = vertex->next;
	}

	/* Remove vertex from vertex list. */
	struct vertex_list_node *curr_v = g->head;
	/* If it's the first vertex, connect g->head to next vertex. */
	if (curr_v == remove) {
		g->head = curr_v->next;
		free(curr_v);
		return;
	}

	/* Else, remove from vertex list by reconnecting surrounding nodes. */
	struct vertex_list_node *prev_v = g->head;
	curr_v = prev_v->next;
	while (curr_v) {
		if (curr_v == remove) {
			prev_v->next = curr_v->next;
			free(curr_v);
			return;
		}
		prev_v = curr_v;
		curr_v = curr_v->next;
	}
}

/* Given a graph and two data pointers, adds a directed, weighted edge between the
   vertices corresponding to each data pointer. If either of such vertices
   does not exist, they are created. If the edge already exists, does nothing.
   Corresponds to add_edge method in giraph. */
void add_wedge_method(void *g_in, void *from_data_ptr, void *to_data_ptr, int w) {
	struct graph *g = (struct graph *) g_in;
	void *from = find_vertex(g_in, from_data_ptr);
	if (from == NULL) {
		from = add_vertex(g_in, from_data_ptr);
	}
	void *to = find_vertex(g_in, to_data_ptr);
	if (to == NULL) {
		to = add_vertex(g_in, to_data_ptr);
	}
	/* Check if from->to edge already exists - if so, return. */
	struct adj_list_node *curr_adj = ((struct vertex_list_node *) from)->adjacencies;
	while (curr_adj) {
		if (curr_adj->vertex == to) {
			return;
		}
		curr_adj = curr_adj->next;
	}

	add_wedge(from, to, w);
}

/* calls add_wedge_method with default weight of 0 */
void add_edge_method(void *g_in, void *from_data_ptr, void *to_data_ptr) {
	add_wedge_method(g_in, from_data_ptr, to_data_ptr, 0);
}

/* Given a graph and two data pointers, removes the directed edge between the
   vertices corresponding to each data pointer. If either of such vertices
   does not exist, or if the edge does not exist, does nothing.
   Corresponds to remove_edge method in giraph. */
void remove_edge(void *g_in, void *from_data_ptr, void *to_data_ptr) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *from = (struct vertex_list_node *) find_vertex(g_in, from_data_ptr);
	struct vertex_list_node *to = (struct vertex_list_node *) find_vertex(g_in, to_data_ptr);
	if (from == NULL || to == NULL) {
		return;
	}

	/* Remove adj_list_node for "to" from adjacency list of "from" */
	if (from->adjacencies) {
		/* if we need to remove the first adjacency, set from's
		   "adjacencies" pointer to be the next adjacency */
		struct adj_list_node *curr = from->adjacencies;
		if (curr->vertex == to) {
			from->adjacencies = curr->next;
			free(curr);
		} else {
			/* else, just remove appropriate adj_list_node from list
			   by reconnecting surrounding nodes */
			struct adj_list_node *prev = from->adjacencies;
			curr = prev->next;

			while (curr) {
				if (curr->vertex == to) {
					prev->next = curr->next;
					free(curr);
					break;
				}
				prev = curr;
				curr = curr->next;
			}
		}
	}
}

/* Checks if a graph contains a vertex. Returns 1 if so, 0 otherwise.
   Corresponds to graph.has_node() method in giraph. */
int has_vertex(void *g_in, void *data_ptr) {
	return (find_vertex(g_in, data_ptr) != NULL);
}

/* Checks if a graph contains an edge between two vertices. 
   Returns 1 if so, 0 otherwise.
   Corresponds to graph.has_edge() method in giraph. */
int has_edge(void *g_in, void *from_data_ptr, void *to_data_ptr) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *from = (struct vertex_list_node *) find_vertex(g_in, from_data_ptr);
	struct vertex_list_node *to = (struct vertex_list_node *) find_vertex(g_in, to_data_ptr);

	/* If either of the vertices is not in the graph, neither is the edge. */
	if (from == NULL || to == NULL) {
		return 0;
	}

	struct adj_list_node *curr_adj = ((struct vertex_list_node *) from)->adjacencies;
	while (curr_adj) {
		if (curr_adj->vertex == to) {
			return 1;
		}
		curr_adj = curr_adj->next;
	}
	return 0;
}

/* return a graph pointer to a graph containing every neighboring vertex */
void *graph_neighbors(void *g_in, void *data_ptr) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *v = find_vertex(g_in, data_ptr);

	struct graph *g_out = (struct graph *) malloc(sizeof(struct graph));
	/* if there are no adjs, or if data_ptr is not in g, return graph * with NULL head */
	g_out->head = NULL;
	if (v == NULL) {
		return (void *)g_out;
	}

	struct adj_list_node *curr_adj = (struct adj_list_node *) v->adjacencies;

	if (curr_adj) {
		/* add first vertex in new graph with data in first adjacency */
		g_out->head = (struct vertex_list_node *) malloc(sizeof(struct vertex_list_node));
		struct vertex_list_node *curr_g_out = g_out->head;
		curr_g_out->next = NULL;
		curr_g_out->adjacencies = NULL;
		curr_g_out->data = curr_adj->vertex->data;
		curr_adj = curr_adj->next;

		while (curr_adj) {
			/* add all vertices in new graph with data in subsequent adjacencies */
			curr_g_out->next = (struct vertex_list_node *) malloc(sizeof(struct vertex_list_node));
			curr_g_out->next->data = curr_adj->vertex->data;
			curr_g_out->next->next = NULL;
			curr_g_out->next->adjacencies = NULL;
			curr_adj = curr_adj->next;
			curr_g_out = curr_g_out->next;
		}
	}

	return (void *)g_out;
}

int graph_get_edge_weight(void *g_in, void *from_data_ptr, void *to_data_ptr) {
	struct vertex_list_node *from = (struct vertex_list_node *) find_vertex(g_in, from_data_ptr);
	if (from == NULL) {
		return 0;
	}
	struct vertex_list_node *to = (struct vertex_list_node *) find_vertex(g_in, to_data_ptr);
	if (to == NULL) {
		return 0;
	}
	struct adj_list_node *curr_adj = from->adjacencies;
	while (curr_adj) {
		if (curr_adj->vertex == to) {
			return curr_adj->weight;
		}
		curr_adj = curr_adj->next;
	}
	return 0;
}

void graph_set_undirected_edge_weight(void *g_in, void *from_data_ptr, void *to_data_ptr, int new_weight) {
	struct vertex_list_node *from = (struct vertex_list_node *) find_vertex(g_in, from_data_ptr);
	if (from == NULL) {
		return;
	}
	struct vertex_list_node *to = (struct vertex_list_node *) find_vertex(g_in, to_data_ptr);
	if (to == NULL) {
		return;
	}
	struct adj_list_node *curr_adj = from->adjacencies;
	while (curr_adj) {
		if (curr_adj->vertex == to) {
			curr_adj->weight = new_weight;
		}
		curr_adj = curr_adj->next;
	}
	curr_adj = to->adjacencies;
	while (curr_adj) {
		if (curr_adj->vertex == from) {
			curr_adj->weight = new_weight;
		}
		curr_adj = curr_adj->next;
	}
}

void graph_set_edge_weight(void *g_in, void *from_data_ptr, void *to_data_ptr, int new_weight) {
	struct vertex_list_node *from = (struct vertex_list_node *) find_vertex(g_in, from_data_ptr);
	if (from == NULL) {
		return;
	}
	struct vertex_list_node *to = (struct vertex_list_node *) find_vertex(g_in, to_data_ptr);
	if (to == NULL) {
		return;
	}
	struct adj_list_node *curr_adj = from->adjacencies;
	while (curr_adj) {
		if (curr_adj->vertex == to) {
			curr_adj->weight = new_weight;
		}
		curr_adj = curr_adj->next;
	}
}

//////////////////// END GRAPH METHODS //////////////////


//////////////////////// FOR NODE ///////////////////////

/* iterate through graph to get num vertices */
int num_vertices(void *g_in) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *vertex = g->head;
	int counter = 0;
	while (vertex) {
		counter++;
		vertex = vertex->next;
	}

	return counter;
}

/* return head of graph */
void *get_head_vertex(void *g_in) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *head = g->head;

	return (void *) head;
}

/* given a vertex, returns next vertex from graph's list */
void *get_next_vertex(void *v_in) {
	struct vertex_list_node *v = (struct vertex_list_node *) v_in;

	return (void *) v->next;
}

/* return the data pointer stored in a vertex */
void *get_data_from_vertex(void *v_in) {
	struct vertex_list_node *v = (struct vertex_list_node *) v_in;

	return v->data;
}

////////////////////// END FOR NODE /////////////////////


//////////////////////// FOR EDGE ///////////////////////

/* construct a list of edge_list_node's and return head */
void *construct_edge_list(void *g_in) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *v = g->head;
	struct edge_list_node *head = NULL;
	int first = 1;
	struct edge_list_node *prev; 
	while (v) {
		struct adj_list_node *adjacency = v->adjacencies;
		while (adjacency) {
			struct edge_list_node *e = (struct edge_list_node *) malloc(sizeof(struct edge_list_node));
			e->from = v;
			e->to = adjacency->vertex;
			e->weight = adjacency->weight;
			e->next = NULL;
			if (first) {
				head = e;
				first = 0;
			}
			else {
				prev->next = e;
			}
			prev = e;
			adjacency = adjacency->next;
		}
		v = v->next;
	}
	if (!head) {
		return NULL;
	}
	return head;
}

/* construct a list of edge_list_node's and return head */
void *construct_undirected_edge_list(void *g_in) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *v = g->head;
	struct edge_list_node *head = NULL;
	int first = 1;
	struct edge_list_node *prev;
	void *map = make_map();
	while (v) {
		struct adj_list_node *adjacency = v->adjacencies;
		while (adjacency) {
			struct adj_list_node *to_adj_list = get(map, (void *) adjacency->vertex);
			int opposite_edge_exists = 0;
			while (to_adj_list) {
				/*  if this v is not in an already edge-ified adj list */
				if (to_adj_list->vertex == v) {
					opposite_edge_exists = 1;
				}
				to_adj_list = to_adj_list->next;
			}
			if (!opposite_edge_exists) {
				struct edge_list_node *e = (struct edge_list_node *) malloc(sizeof(struct edge_list_node));
				e->from = v;
				e->to = adjacency->vertex;
				e->weight = adjacency->weight;
				e->next = NULL;
				if (first) {
					head = e;
					first = 0;
				}
				else {
					prev->next = e;
				}
				prev = e;
			}
			adjacency = adjacency->next;
		}
		put(map, (void *) v, (void *) v->adjacencies);
		v = v->next;
	}
	free_map(map);
	if (!head) {
		return NULL;
	}
	return head;
}

/* return size of list of edge_list_node's */
int num_edges(void *e_head) {
	struct edge_list_node *e = (struct edge_list_node *) e_head;
	int count = 0;
	while (e) {
		count++;
		e = e->next;
	}
	return count;
}

/* get next edge_list_node in list */
void *get_next_edge(void *e_in) {
	struct edge_list_node *e = (struct edge_list_node *)e_in;
	return e->next;
}

////////////////////// END FOR EDGE /////////////////////


////////////////////// BFS and DFS //////////////////////

/* allocate an array of vertex pointers of size (num_vertices + 1), and 
store num_vertices in a dummy vertex at the first index */
void *get_visited_array(void *g_in) {
	int *size = malloc(sizeof(int));
	*size = num_vertices(g_in);
	struct vertex_list_node **visited = 
		(struct vertex_list_node **) malloc(sizeof(struct vertex_list_node *) * (*size + 1));
	memset(visited, 0, sizeof(struct vertex_list_node *) * (*size + 1));
	/* store num nodes in the graph in the first entry in the array */
	struct vertex_list_node *dummy_size_node = 
		(struct vertex_list_node *) malloc(sizeof(struct vertex_list_node));
	memset(visited, 0, sizeof(struct vertex_list_node));
	dummy_size_node->data = size;
	visited[0] = dummy_size_node;
	return visited;
}

/* check if a vertex pointer is already in the visited array */
int unvisited(struct vertex_list_node *v, struct vertex_list_node **visited) {
	int size = *(int *) visited[0]->data;
	for (int i = 1; i <= size; i++) {
		if (visited[i] == v) {
			return 0;
		}
		if (!visited[i]) {
			break;
		}
	}
	return 1;
}

/* add a vertex pointer to the visited array  */
void add_visited(struct vertex_list_node **visited, struct vertex_list_node *v) {
	int size = *(int *) visited[0]->data;
	for (int i = 1; i <= size; i++) {
		if (!visited[i]) {
			visited[i] = v;
			return;
		}
	}
}

//////////////////// BFS ////////////////////

/* create a bfs_queue, and push the first vertex pointer onto it */
void *get_bfs_queue(void *first_v, void *visited) {
	struct queue_list_node *q = malloc(sizeof(struct queue_list_node));
	memset(q, 0, sizeof(struct queue_list_node));
	q->v = (struct vertex_list_node *) first_v;
	q->next = NULL;
	add_visited(visited, q->v);
	return q;
}

/* create a queue_list_node with given vertex pointer and add it to the back of the queue */
void push_queue(struct vertex_list_node *vertex, struct queue_list_node *queue) {
	/* if empty */
	if (!queue->v) {
		queue->v = vertex;
		queue->next = NULL;
		return;
	}
	/*else add to end */
	while (queue->next) {
		queue = queue->next;
	}
	struct queue_list_node *new_q = (struct queue_list_node *) malloc(sizeof(struct queue_list_node));
	memset(new_q, 0, sizeof(struct queue_list_node));
	new_q->next = NULL;
	new_q->v = vertex;
	queue->next = new_q;
}

/* pop a vertex pointer from the queue */
void *pop_queue(struct queue_list_node *queue) {
	struct vertex_list_node *out = queue->v;
	struct queue_list_node *tofree = queue->next;
	if (queue->next) {
		queue->v = queue->next->v;
		queue->next = queue->next->next;
		free(tofree);
	}
	else {
		queue->v = NULL;
	}
	return out;
}

void cleanup_bfs(void *visited_in, void *queue_in) {
	struct vertex_list_node **visited = (struct vertex_list_node **) visited_in;
	struct queue_list_node *queue = (struct queue_list_node *) queue_in;
	free(visited[0]->data);
	free(visited[0]);
	free(visited_in);
	/* if empty */
	if (!queue->v) {
		free(queue);
	}
	/*else add to end */
	while (queue->next) {
		void *temp = queue;
		queue = queue->next;
		free(temp);
	}
}

/* get the next graph vertex in bfs order, updating visited array and bfs queue */
void *get_next_bfs_vertex(void *visited_in, void *queue) {
	struct vertex_list_node **visited = (struct vertex_list_node **) visited_in;
	struct vertex_list_node *v = pop_queue(queue);
	/* if queue empty we are done */
	if (!v) {
		cleanup_bfs(visited_in, queue);
		return NULL;
	}
	struct adj_list_node *adjacency = v->adjacencies;
	while (adjacency) {
		if (unvisited(adjacency->vertex, visited)) {
			push_queue(adjacency->vertex, queue);
			add_visited(visited, adjacency->vertex);
		}
		adjacency = adjacency->next;
	}
	return v;
}

/* test if the vertex is null to determine if bfs has finished */
int bfs_done(void *curr_v) {
	if (curr_v == NULL) {
		return 1;
	}
	return 0;
}

//////////////////// DFS ////////////////////

/* create a stack_list_node with given vertex, connect to top of stack */
void push_stack(struct vertex_list_node *vertex, struct stack_list_node *s) {
	struct stack_list_node *new_s = (struct stack_list_node *) malloc(sizeof(struct stack_list_node));
	memset(new_s, 0, sizeof(struct stack_list_node));
	new_s->v = vertex;
	new_s->next = s->next;
	s->next = new_s;
}

/* pop a vertex pointer from stack */
void *pop_stack(struct stack_list_node *s) {
	if (s->next) {
		struct stack_list_node *t = s->next;
		s->next = t->next;
		struct vertex_list_node *out = t->v;
		free(t);
		return out;
	}
	return NULL;
}

/* create a dfs_stack, and push the first vertex pointer onto it */
void *get_dfs_stack(void *first_v, void *visited) {
	struct vertex_list_node *vertex = (struct vertex_list_node *) first_v;
	struct stack_list_node *s_static_top = malloc(sizeof(struct stack_list_node));
	memset(s_static_top, 0, sizeof(struct stack_list_node));
	s_static_top->v = NULL;
	push_stack((struct vertex_list_node *) vertex, s_static_top);
	return s_static_top;
}

void cleanup_dfs(void *visited_in, void *stack_in) {
	struct vertex_list_node **visited = (struct vertex_list_node **) visited_in;
	struct stack_list_node *stack = (struct stack_list_node *) stack_in;
	free(visited[0]->data);
	free(visited[0]);
	free(visited_in);
	while (stack->next) {
		void *temp = stack;
		stack = stack->next;
		free(temp);
	}
}

/* get the next graph vertex in bfs order, updating visited array and bfs queue */
void *get_next_dfs_vertex(void *visited_in, void *stack) {
	struct vertex_list_node **visited = (struct vertex_list_node **) visited_in;
	struct vertex_list_node *v = pop_stack(stack);
	while (v && unvisited(v, visited) == 0) {
		v = pop_stack(stack);
	}
	/* if stack empty we are done */
	if (!v) {
		cleanup_dfs(visited_in, stack);
		return NULL;
	}
	add_visited(visited, v);
	struct adj_list_node *adjacency = v->adjacencies;
	while (adjacency) {
		push_stack(adjacency->vertex, (struct stack_list_node *) stack);
		adjacency = adjacency->next;
	}
	return v;
}

/* test if the vertex is null to determine if bfs has finished */
int dfs_done(void *curr_v) {
	if (curr_v == NULL) {
		return 1;
	}
	return 0;
}

////////////////////// END BFS and DFS //////////////////////

//////////////////////// PRINT ///////////////////////

void print_int(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %d\n", ((union data_type *) vertex->data)->i);
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("(%d, weight: %d) ",
				   ((union data_type *) adjacency->vertex->data)->i,
				   adjacency->weight);
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}

void print_float(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %f\n", ((union data_type *) vertex->data)->f);
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("(%f, weight: %d) ",
				   ((union data_type *) adjacency->vertex->data)->f,
				   adjacency->weight);
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}

void print_char_ptr(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %s\n", ((union data_type *) vertex->data)->s);
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("(%s, weight: %d) ",
				   ((union data_type *) adjacency->vertex->data)->s,
				   adjacency->weight);
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}

void print_unweighted_int(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %d\n", ((union data_type *) vertex->data)->i);
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("%d ", ((union data_type *) adjacency->vertex->data)->i);
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}

void print_unweighted_float(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %f\n", ((union data_type *) vertex->data)->f);
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("%f ", ((union data_type *) adjacency->vertex->data)->f);
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}

void print_unweighted_char_ptr(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %s\n", ((union data_type *) vertex->data)->s);
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("%s ", ((union data_type *) adjacency->vertex->data)->s);
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}

char *get_bool_str(int val) {
	if (val) {
		return "true";
	} else {
		return "false";
	}
}

void print_bool(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %s\n", get_bool_str(((union data_type *) vertex->data)->i));
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("(%s, weight: %d) ",
				   get_bool_str(((union data_type *) adjacency->vertex->data)->i),
				   adjacency->weight);
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}

void print_unweighted_bool(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %s\n", get_bool_str(((union data_type *) vertex->data)->i));
		printf("adjacencies: ");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf("%s ",
				   get_bool_str(((union data_type *) adjacency->vertex->data)->i));
			adjacency = adjacency->next;
		}
		printf("\n");
		vertex = vertex->next;
	}
	printf("\n");
}



//////////////////////// TESTING ////////////////////////

/*void print_graph(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %p\n", vertex);
		printf("data: %p\n", vertex->data);
		printf("adjacencies:");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf(" %p", adjacency->vertex);
			adjacency = adjacency->next;
		}
		printf("\n\n");
		vertex = vertex->next;
	}
	printf("\n");
}

void print_queue(struct queue_list_node *queue) {
	fprintf(stderr, "printing queue: ");
	while (queue && queue->v) {
		fprintf(stderr, "%d ", *(int *) queue->v->data);
		queue = queue->next;
	}
	printf("\n");
}

void print_visited(struct vertex_list_node **visited) {
	int size = *(int *) visited[0]->data;
	printf("printing visited (excluding dummy size node): [");
	for (int i = 1; i <= size; i++) {
		if (visited[i]) {
			printf("%d, ", *(int *) visited[i]->data);
		}
		else {
			printf("0x0, ");
		}
	}
	printf("]\n");
}

void print_edges(struct edge_list_node *e) {
	while (e) {
		printf("from: %d  to: %d weight: %d\n", *(int *) e->from->data, *(int *) e->to->data, e->weight);
		e = e->next;
	}
}

void add_bidirectional_edge(void *a, void *b) {
	add_edge(a, b);
	add_edge(b, a);
}

int main() {
	struct graph *g = (struct graph *) new_graph();
	struct graph *g2 = (struct graph *) new_graph();

	int *new_data = malloc(sizeof(int));
	add_vertex(g, new_data);
	struct vertex_list_node *head = (struct vertex_list_node *) get_head_vertex(g);
	*(int *) head->data = 0;

	int vertices = 6;
	int save_vertex_num = 2;
	struct vertex_list_node *save;
	struct vertex_list_node *savedarray[vertices];
	savedarray[0] = head;

	for (int i = 1; i < vertices; i++) {
		int *new_data = malloc(sizeof(int));
		struct vertex_list_node *vertex = (struct vertex_list_node *) add_vertex(g, new_data);
		*(int *) vertex->data = i;

		if (i == save_vertex_num) {
			save = vertex;
		}
		savedarray[i] = vertex;

	}
	add_bidirectional_edge(savedarray[0], savedarray[1]);
	add_bidirectional_edge(savedarray[1], savedarray[2]);
	add_bidirectional_edge(savedarray[2], savedarray[3]);
	add_bidirectional_edge(savedarray[3], savedarray[4]);
	add_bidirectional_edge(savedarray[0], savedarray[5]);
	add_bidirectional_edge(savedarray[1], savedarray[5]);

	//printf("num vertices: %d\n", num_vertices(g));

	//printf("before entering bfs land *save->data: %d \n", *(int *) save->data);

	struct vertex_list_node **visited = get_bfs_visited_array(g2);
	void *queue = get_dfs_stack(savedarray[0], visited);
	struct vertex_list_node *curr = get_next_dfs_vertex(visited, queue);
	while (curr) {
		printf("asdfsdf %d\n", *curr->data);
		curr = get_next_dfs_vertex(visited, queue);
	}

	struct graph *g_nei = (struct graph *) graph_neighbors(g, savedarray[0]->data);

	graph_set_undirected_edge_weight(g, savedarray[3]->data, savedarray[4]->data, 50);

	graph_set_edge_weight(g, savedarray[4]->data, savedarray[0]->data, 80);

	graph_set_edge_weight(g, savedarray[1]->data, savedarray[2]->data, 70);

	graph_set_undirected_edge_weight(g, savedarray[0]->data, savedarray[3]->data, 70);

	fprintf(stderr, "%d \n", graph_get_edge_weight(g, savedarray[0]->data, savedarray[1]->data));

	fprintf(stderr, "%d \n", graph_get_edge_weight(g, savedarray[3]->data, savedarray[4]->data));


	//print_edges(edge_list);

	print_data((void *) g);

	//printf("\n%d\n", num_edges(edge_list));

}*/

////////////////////// END TESTING //////////////////////
