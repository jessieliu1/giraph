#include <stdio.h>
#include <stdlib.h>

/* Terminology-wise, we've painted ourselves into a corner here. 
   Elsewhere in in this project, "node" refers to a single node in a graph.
   That is NOT true in this file. In this file, "vertex" refers to a node in
   a graph, and "node" refers to a single node of a linked list. */

/* A single node of the adjacency list for a single vertex. */
struct edge_list_node {
	struct vertex_list_node *vertex;
	struct edge_list_node *next;
};

/* A single vertex in a graph. */
struct vertex_list_node {
	int *data;
	struct edge_list_node *adjacencies;
	struct vertex_list_node *next;
};

/* A graph. */
struct graph {
	struct vertex_list_node *head;
};

/* Returns a pointer to a new graph. */
void *new_graph() {
	struct graph *g = malloc(sizeof(struct graph));
	g->head = NULL;

	return (void *) g;
}

/* Add a new vertex to the end of the vertex list in a graph, and return a
   pointer to the new vertex. */
void *add_vertex(void *graph_ptr, int *data_ptr) {
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

/* Add a (directed) edge between two vertices. */
void add_edge(void *from_ptr, void *to_ptr) {
	/* HOLY SHIT THIS SHIT NEEDS COMMENTING
	   I WROTE IT LIKE 15 MINUTES AGO AND ALREADY DON'T UNDERSTAND */
	struct vertex_list_node *from = (struct vertex_list_node *) from_ptr;
	struct vertex_list_node *to = (struct vertex_list_node *) to_ptr;

	if (from->adjacencies == NULL) {
		from->adjacencies = malloc(sizeof(struct edge_list_node));
		from->adjacencies->vertex = to;
		from->adjacencies->next = NULL;
	} else {
		struct edge_list_node *last_node = from->adjacencies;
		while (last_node->next) {
			last_node = last_node->next;
		}
		last_node->next = malloc(sizeof(struct edge_list_node));
		last_node->next->vertex = to;
		last_node->next->next = NULL;
	}
}

/* Allocate a new unique data pointer. */
int *new_data() {
	return malloc(sizeof(int));
}

/* Change the data stored in a data pointer. */
void set_data(int *data_ptr, int data_val) {
	*data_ptr = data_val;
}

/* Get data stored in a data pointer. */
int get_data(int *data_ptr) {
	return *data_ptr;
}

/* Find and return the vertex_list_node associated with a data pointer.
   Returns null if there is none.
   If this ever needs to be called LLVM-side, change struct *'s in header to
   void *'s. */
struct vertex_list_node *find_vertex(struct graph *g, int *data_ptr) {
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		if (vertex->data == data_ptr) {
			return vertex;
		}
		vertex = vertex->next;
	}
	return NULL;
}

/* Given a graph and a data pointer, checks if the graph has a vertex associated
   with the data pointer, and if not, creates one and adds it to the graph.
   Corresponds to add_node method in giraph. */
void add_vertex_if_not_present(void *g_in, int *data_ptr) {
	struct graph *g = (struct graph *) g_in;
	if (find_vertex(g, data_ptr) == NULL) {
		add_vertex(g_in, data_ptr);
	}
}

/* Given a graph and a data pointer, finds the vertex in the graph associated
   with the data pointer and removes it from the vertex list and all adjacency
   lists. If no such vertex exists, does nothing.
   Corresponds to remove_node method in giraph. */
void remove_vertex(void *g_in, int *data_ptr) {
	struct graph *g = (struct graph *) g_in;
	struct vertex_list_node *remove = find_vertex(g, data_ptr);
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
			struct edge_list_node *curr_e = vertex->adjacencies;
			if (curr_e->vertex == remove) {
				vertex->adjacencies = curr_e->next;
				free(curr_e); /* woooaaahh */
			} else {
				/* else, just remove appropriate edge_list_node from list
				   by reconnecting surrounding nodes */
				struct edge_list_node *prev_e = vertex->adjacencies;
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

/* Given a graph and two data pointers, adds a (directed) edge between the
   vertices corresponding to each data pointer. If either of such vertices
   does not exist, they are created.
   Corresponds to add_edge method in giraph. */
void add_edge_method(void *g_in, int *from_data_ptr, int *to_data_ptr) {
	struct graph *g = (struct graph *) g_in;
	void *from = (void *) find_vertex(g, from_data_ptr);
	if (from == NULL) {
		from = add_vertex(g_in, from_data_ptr);
	}
	void *to = (void *) find_vertex(g, to_data_ptr);
	if (to == NULL) {
		to = add_vertex(g_in, to_data_ptr);
	}
	add_edge(from, to);
}

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
int *get_data_from_vertex(void *v_in) {
	struct vertex_list_node *v = (struct vertex_list_node *) v_in;

	return v->data;
}

void print_graph(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %p\n", vertex);
		printf("data: %p\n", vertex->data);
		printf("adjacencies:");
		struct edge_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf(" %p", adjacency->vertex);
			adjacency = adjacency->next;
		}
		printf("\n\n");
		vertex = vertex->next;
	}
	printf("\n");
}


void print_data(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %d\n", *vertex->data);
		printf("adjacencies:");
		struct edge_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf(" %d", *adjacency->vertex->data);
			adjacency = adjacency->next;
		}
		printf("\n\n");
		vertex = vertex->next;
	}
	printf("\n\n");
}
/*
int main() {
	struct graph *g = (struct graph *) new_graph();

	int *new_data = malloc(sizeof(int));
	add_vertex(g, new_data);
	struct vertex_list_node *head = (struct vertex_list_node *) get_head_vertex(g);
	*head->data = 0;

	int save_vertex_num = 2;
	struct vertex_list_node *save;

	for (int i = 1; i < 6; i++) {
		int *new_data = malloc(sizeof(int));
		struct vertex_list_node *vertex = (struct vertex_list_node *) add_vertex(g, new_data);
		*vertex->data = i;

		if (i == save_vertex_num) {
			save = vertex;
		}

		add_edge(head, vertex);
		add_edge(vertex, head);
	}

	printf("num vertices: %d\n", num_vertices(g));

	printf("vertex with data: %d ... next vertex in list has data: %d\n\n", *get_data_from_vertex(save),
		*get_data_from_vertex(get_next_vertex(save)));

	print_data((void *) g);
}*/
