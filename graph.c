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

void *add_vertex(void *graph_ptr) {
	struct vertex_list_node *vertex = malloc(sizeof(struct vertex_list_node));
	vertex->data = NULL;
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

void print_graph(void *graph_ptr) {
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
}

/*
int main() {
	struct graph *g = (struct graph *) new_graph();

	struct vertex_list_node *head = (struct vertex_list_node *) add_vertex(g);
	head->data = malloc(sizeof(int));
	*head->data = 0;
	head->adjacencies = malloc(sizeof(struct edge_list_node));

	struct vertex_list_node *last_node = head;
	struct edge_list_node *head_adj = head->adjacencies;

	for (int i = 1; i < 4; i++) {
		struct vertex_list_node *vertex = (struct vertex_list_node *) add_vertex(g);
		vertex->data = malloc(sizeof(int));
		*vertex->data = i;
		last_node = last_node->next;

		vertex->adjacencies = malloc(sizeof(struct edge_list_node));
		vertex->adjacencies->vertex = head;
		vertex->adjacencies->next = NULL;
		head_adj->vertex = vertex;
		if (i < 3) {
			head_adj->next = malloc(sizeof(struct edge_list_node));
			head_adj = head_adj->next;
		} else {
			head_adj->next = NULL;
		}
	}


	print_graph((void *) g);
}
*/
