#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define MAX_NODES 50000  // Adjust as needed

/**
 * Node structure for adjacency list
 */
typedef struct Node {
  int vertex;
  struct Node* next;
} Node;

/**
 * Graph structure
 */
typedef struct Graph {
  int numVertices;
  Node** adjLists;
  int* visited;
} Graph;

/**
 * Queue structure for BFS
 */
typedef struct Queue {
  int items[MAX_NODES];
  int front;
  int rear;
} Queue;

/**
 * Function to create a node
 */
Node* createNode(int v) {
  Node* newNode = (Node*)malloc(sizeof(Node));
  newNode->vertex = v;
  newNode->next = NULL;
  return newNode;
}

/**
 * Function to create a graph
 */
Graph* createGraph(int vertices) {
  Graph* graph = (Graph*)malloc(sizeof(Graph));
  graph->numVertices = vertices;

  graph->adjLists = (Node**)malloc(vertices * sizeof(Node*));
  graph->visited = (int*)malloc(vertices * sizeof(int));

  for (int i = 0; i < vertices; i++) {
    graph->adjLists[i] = NULL;
    graph->visited[i] = 0;
  }

  return graph;
}

/**
 * Function to add edge to an undirected graph
 */
void addEdge(Graph* graph, int src, int dest) {
  // Add edge from src to dest
  Node* newNode = createNode(dest);
  newNode->next = graph->adjLists[src];
  graph->adjLists[src] = newNode;

  // Since the graph is undirected, add edge from dest to src also
  newNode = createNode(src);
  newNode->next = graph->adjLists[dest];
  graph->adjLists[dest] = newNode;
}

/**
 * Function to create a queue
 */
Queue* createQueue() {
  Queue* q = (Queue*)malloc(sizeof(Queue));
  q->front = -1;
  q->rear = -1;
  return q;
}

/**
 * Check if the queue is empty
 */
int isEmpty(Queue* q) { return q->rear == -1; }

/**
 * Enqueue function
 */
void enqueue(Queue* q, int value) {
  if (q->rear == MAX_NODES - 1)
    printf("\nQueue is Full!!");
  else {
    if (q->front == -1) q->front = 0;
    q->rear++;
    q->items[q->rear] = value;
  }
}

/**
 * Dequeue function
 */
int dequeue(Queue* q) {
  int item;
  if (isEmpty(q)) {
    printf("Queue is empty");
    item = -1;
  } else {
    item = q->items[q->front];
    q->front++;
    if (q->front > q->rear) {
      // Reset the queue
      q->front = q->rear = -1;
    }
  }
  return item;
}

/**
 * BFS algorithm
 */
void bfs(Graph* graph, int startVertex) {
  Queue* q = createQueue();

  graph->visited[startVertex] = 1;
  enqueue(q, startVertex);

  while (!isEmpty(q)) {
    int currentVertex = dequeue(q);

    Node* temp = graph->adjLists[currentVertex];

    while (temp) {
      int adjVertex = temp->vertex;

      if (graph->visited[adjVertex] == 0) {
        graph->visited[adjVertex] = 1;
        enqueue(q, adjVertex);
      }
      temp = temp->next;
    }
  }
  free(q);
}

/**
 * Main function to demonstrate BFS and measure execution time.
 */
int main() {
  int N_values[] = {1000, 5000, 10000, 20000, 50000};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);

  for (int t = 0; t < num_tests; t++) {
    int N = N_values[t];
    if (N > MAX_NODES) {
      printf("N exceeds MAX_NODES limit.\n");
      continue;
    }

    Graph* graph = createGraph(N);

    // Build a connected graph (e.g., a chain)
    for (int i = 0; i < N - 1; i++) {
      addEdge(graph, i, i + 1);
    }

    // Measure start time
    clock_t start_time = clock();

    // Perform BFS starting from vertex 0
    bfs(graph, 0);

    // Measure end time
    clock_t end_time = clock();

    // Calculate time taken
    double time_spent = (double)(end_time - start_time) / CLOCKS_PER_SEC;

    // Print the result
    printf("BFS with N = %d\n", N);
    printf("Time taken: %f seconds\n\n", time_spent);

    // Free memory
    for (int i = 0; i < N; i++) {
      Node* temp = graph->adjLists[i];
      while (temp) {
        Node* toFree = temp;
        temp = temp->next;
        free(toFree);
      }
    }
    free(graph->adjLists);
    free(graph->visited);
    free(graph);
  }

  return 0;
}
