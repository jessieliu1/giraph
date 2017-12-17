#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Terminology-wise, we've painted ourselves into a corner here. 
   Elsewhere in in this project, "node" refers to a single node in a graph.
   That is NOT true in this file. In this file, "vertex" refers to a node in
   a graph, and "node" refers to a single node of a linked list. */

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
	int *data;
	struct adj_list_node *adjacencies;
	struct vertex_list_node *next;
};

/* A graph. */
struct graph {
	struct vertex_list_node *head;
};

struct queue_list_node {
	struct vertex_list_node *v;
	struct queue_list_node *next;
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

/* Allocate a new unique data pointer. */
int *new_data() {
	return malloc(sizeof(int));
}

/////// edge //////
/* Allocate a new unique data pointer. */
int *edge_from(void *e) {
	return ((struct edge_list_node *) e)->from->data;
}

int *edge_to(void *e) {
	return ((struct edge_list_node *) e)->to->data;
}

int edge_weight(void *e) {
	return ((struct edge_list_node *) e)->weight;
}

void edge_set_weight(void *e, int new_weight) {
	((struct edge_list_node *) e)->weight = new_weight;
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
   Returns null if there is none. */
void *find_vertex(void *g_in, int *data_ptr) {
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

/* Given a graph and a data pointer, checks if the graph has a vertex associated
   with the data pointer, and if not, creates one and adds it to the graph.
   Corresponds to add_node method in giraph. */
void add_vertex_if_not_present(void *g_in, int *data_ptr) {
	struct graph *g = (struct graph *) g_in;
	if (find_vertex(g_in, data_ptr) == NULL) {
		add_vertex(g_in, data_ptr);
	}
}

/* Given a graph and a data pointer, finds the vertex in the graph associated
   with the data pointer and removes it from the vertex list and all adjacency
   lists. If no such vertex exists, does nothing.
   Corresponds to remove_node method in giraph. */
void remove_vertex(void *g_in, int *data_ptr) {
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
   does not exist, they are created.
   Corresponds to add_edge method in giraph. */
void add_wedge_method(void *g_in, int *from_data_ptr, int *to_data_ptr, int w) {
	struct graph *g = (struct graph *) g_in;
	void *from = find_vertex(g_in, from_data_ptr);
	if (from == NULL) {
		from = add_vertex(g_in, from_data_ptr);
	}
	void *to = find_vertex(g_in, to_data_ptr);
	if (to == NULL) {
		to = add_vertex(g_in, to_data_ptr);
	}
	add_wedge(from, to, w);
}

/* calls add_wedge_method with default weight of 0 */
void add_edge_method(void *g_in, int *from_data_ptr, int *to_data_ptr) {
	add_wedge_method(g_in, from_data_ptr, to_data_ptr, 0);
}

/* Given a graph and two data pointers, removes the directed edge between the
   vertices corresponding to each data pointer. If either of such vertices
   does not exist, or if the edge does not exist, does nothing.
   Corresponds to remove_edge method in giraph. */
void remove_edge(void *g_in, int *from_data_ptr, int *to_data_ptr) {
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


void print_data(void *graph_ptr) {
	struct graph *g = (struct graph *) graph_ptr;
	struct vertex_list_node *vertex = g->head;
	while (vertex) {
		printf("vertex: %d\n", *vertex->data);
		printf("adjacencies:");
		struct adj_list_node *adjacency = vertex->adjacencies;
		while (adjacency) {
			printf(" %d", *adjacency->vertex->data);
			adjacency = adjacency->next;
		}
		printf("\n\n");
		vertex = vertex->next;
	}
	printf("\n\n");
}

//// for_edge ////
void print_edges(struct edge_list_node *e) {
	while (e) {
		printf("from: %d  to: %d weight: %d\n", *e->from->data, *e->to->data, e->weight);
		e = e->next;
	}
}

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


//////////////////

/////// BFS //////
void print_queue(struct queue_list_node *queue) {
	fprintf(stderr, "printing queue: ");
	while (queue && queue->v) {
		fprintf(stderr, "%d ", *queue->v->data);
		queue = queue->next;
	}
	printf("\n");
}

void print_visited(struct vertex_list_node **visited) {
	int size = *visited[0]->data;
	printf("printing visited (excluding dummy size node): [");
	for (int i = 1; i <= size; i++) {
		if (visited[i]) {
			printf("%d, ", *visited[i]->data);
		}
		else {
			printf("0x0, ");
		}
	}
	printf("]\n");
}

/* allocate an array of vertex pointers of size (num_vertices + 1), and 
store num_vertices in a dummy vertex at the first index */
void *get_bfs_visited_array(void *g_in) {
	int *size = malloc(sizeof(int));
	*size = num_vertices(g_in);
	struct vertex_list_node **visited = 
		(struct vertex_list_node **) malloc(sizeof(struct vertex_list_node *) * (*size + 1));
	memset(visited, 0, sizeof(struct vertex_list_node *) * (*size + 1));
	/* store num nodes in the graph in the first entry in the array */
	struct vertex_list_node *dummy_size_node = 
		(struct vertex_list_node *) malloc(sizeof(struct vertex_list_node));
	dummy_size_node->data = size;
	visited[0] = dummy_size_node;
	return visited;
}

/* check if a vertex pointer is already in the visited array */
int unvisited(struct vertex_list_node *v, struct vertex_list_node **visited) {
	int size = *visited[0]->data;
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
	int size = *visited[0]->data;
	for (int i = 1; i <= size; i++) {
		if (!visited[i]) {
			visited[i] = v;
			return;
		}
	}
}

/* create a bfs_queue, and push the first vertex pointer onto it */
void *get_bfs_queue(void *first_v, void *visited) {
	struct vertex_list_node *v = (struct vertex_list_node *) first_v;
	struct queue_list_node *q = malloc(sizeof(struct queue_list_node));
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

/* get the next graph vertex in bfs order, updating visited array and bfs queue */
void *get_next_bfs_vertex(void *visited_in, void *queue) {
	struct vertex_list_node **visited = (struct vertex_list_node **) visited_in;
	struct vertex_list_node *v = pop_queue(queue);
	/* if queue empty we are done */
	if (!v) {
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
//////////////////

void add_bidirectional_edge(void *a, void *b) {
	add_edge(a, b);
	add_edge(b, a);
}

/*int main() {
	struct graph *g = (struct graph *) new_graph();

	int *new_data = malloc(sizeof(int));
	add_vertex(g, new_data);
	struct vertex_list_node *head = (struct vertex_list_node *) get_head_vertex(g);
	*head->data = 1;

	int vertices = 5;
	int save_vertex_num = 2;
	struct vertex_list_node *save;
	struct vertex_list_node *savedarray[vertices];
	savedarray[0] = head;

	for (int i = 1; i < vertices; i++) {
		int *new_data = malloc(sizeof(int));
		struct vertex_list_node *vertex = (struct vertex_list_node *) add_vertex(g, new_data);
		*vertex->data = i+1;

		if (i == save_vertex_num) {
			save = vertex;
		}
		savedarray[i] = vertex;

	}
	add_bidirectional_edge(savedarray[0], savedarray[1]);
	add_bidirectional_edge(savedarray[1], savedarray[2]);
	add_bidirectional_edge(savedarray[2], savedarray[3]);
	add_bidirectional_edge(savedarray[2], savedarray[4]);
	add_bidirectional_edge(savedarray[3], savedarray[4]);
	add_bidirectional_edge(savedarray[4], savedarray[0]);

	printf("num vertices: %d\n", num_vertices(g));

	printf("vertex with data: %d ... next vertex in list has data: %d\n\n", *get_data_from_vertex(save),
		*get_data_from_vertex(get_next_vertex(save)));

	print_data((void *) g);

	void *h = construct_edge_list((void *) g);

	printf("\n%d\n", num_edges(h));

	printf("before entering bfs land *save->data: %d \n", *save->data);

	struct vertex_list_node **visited = get_bfs_visited_array(g);
	void *queue = get_bfs_queue(save, visited);
	while (get_next_bfs_vertex(visited, queue)) {

	}

	cleanup_bfs(visited, queue);
}*/