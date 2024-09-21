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
 * DFS algorithm
 */
void dfs(Graph* graph, int vertex) {
  graph->visited[vertex] = 1;

  Node* temp = graph->adjLists[vertex];

  while (temp) {
    int adjVertex = temp->vertex;

    if (graph->visited[adjVertex] == 0) {
      dfs(graph, adjVertex);
    }
    temp = temp->next;
  }
}

/**
 * Main function to demonstrate DFS and measure execution time.
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

    // Perform DFS starting from vertex 0
    dfs(graph, 0);

    // Measure end time
    clock_t end_time = clock();

    // Calculate time taken
    double time_spent = (double)(end_time - start_time) / CLOCKS_PER_SEC;

    // Print the result
    printf("DFS with N = %d\n", N);
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
