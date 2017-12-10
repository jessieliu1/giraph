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

	struct vertex_list_node *head = (struct vertex_list_node *) add_vertex(g);
	head->data = malloc(sizeof(int));
	*head->data = 0;

	for (int i = 1; i < 4; i++) {
		struct vertex_list_node *vertex = (struct vertex_list_node *) add_vertex(g);
		vertex->data = malloc(sizeof(int));
		*vertex->data = i;

		add_edge(head, vertex);
		add_edge(vertex, head);
	}

	print_data((void *) g);
}
*/
