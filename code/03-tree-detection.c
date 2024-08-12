#include <stdio.h>

/** Number of vertices in the graph */
#define N 5

/** Search for a cycle in a graph
 * @param `graph`: adjacency matrix of the graph
 * @param `v`: current vertex
 * @param `visited`: array of visited vertices
 * @param `parent`: parent of the current vertex
 * @return `1` if a cycle is found, `0` otherwise
 */
int search_cycle(int graph[N][N], int v, int *visited, int parent) {
  visited[v] = 1;
  for (int i = 0; i < N; ++i) {
    // FIXME: Implement the function
  }
  return 0;
}

/** Check if all vertices have been visited
 * @param `visited`: array of visited vertices
 * @return `1` if all vertices have been visited, `0` otherwise
 */
int all_visited(int *visited) {
  int i;
  // Find first unvisited vertex
  for (i = 0; i < N && visited[i]; ++i)
    ;
  return i == N;  // Return true if no unvisited vertices found, false otherwise
}

/** Check if a graph is a tree
 * @param `graph`: adjacency matrix of the graph
 * @return `1` if the graph is a tree, `0` otherwise
 */
int is_tree(int graph[N][N]) {
  int visited[N] = {0};
  int has_cycle;

  // Perform depth-first search from vertex 0 to check if all vertices
  // are reachable (connected)
  // FIXME: Implement the function

  // Check if all vertices have been visited
  // FIXME: Implement the function

  // Return true if no cycle is found and all
  return 1;
}

/** Main function */
int main() {
  // Tree example
  /*
     (0)--(1)--(2)
      |
      |
      |
     (3)-------(4)
  */
  int graph1[N][N] = {{0, 1, 0, 1, 0},
                      {1, 0, 1, 0, 0},
                      {0, 1, 0, 0, 0},
                      {1, 0, 0, 0, 1},
                      {0, 0, 0, 1, 0}};
  printf("Test Case 1: Is Tree? %d\n", is_tree(graph1));

  /*
  // Non-tree example (has at least one cycle)
     (0)--(1)--(2)
      |   / \   |
      |  /   \  |
      | /     \ |
     (3)-------(4)
  */
  int graph2[N][N] = {{0, 1, 0, 1, 0},
                      {1, 0, 1, 1, 1},
                      {0, 1, 0, 0, 1},
                      {1, 1, 0, 0, 1},
                      {0, 1, 1, 1, 0}};
  printf("Test Case 2: Is Tree? %d\n", is_tree(graph2));

  return 0;
}
