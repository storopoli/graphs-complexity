#include <stdio.h>

/** Number of vertices in the graph */
#define N 5

/** A utility function to print solution */
void print_solution(int path[]) {
  printf("Solution Exists: ");

  for (int i = 0; i < N; i++) printf("%d ", path[i]);

  // Let us print the first vertex again to show the complete cycle
  printf("%d ", path[0]);
  printf("\n");
  printf("\n");
}

/** A utility function to check if the vertex v can be added at index
 * 'pos' in the Hamiltonian cycle constructed so far
 * (stored in 'path[]')
 */
int is_safe(int v, int graph[N][N], int path[], int pos) {
  // Check if this vertex is an adjacent vertex of the previously added vertex.
  if (graph[path[pos - 1]][v] == 0) return 0;

  // Check if the vertex has already been included.
  // This step can be optimized by creating an array of size N
  for (int i = 0; i < pos; i++)
    if (path[i] == v) return 0;

  return 1;
}

/** A recursive utility function to solve Hamiltonian cycle problem */
int hamiltonian_cycle_util(int graph[N][N], int path[], int pos) {
  // base case: If all vertices are included in Hamiltonian cycle
  if (pos == N) {
    // And if there is an edge from the last included vertex to the
    // first vertex
    if (graph[path[pos - 1]][path[0]] == 1)
      return 1;
    else
      return 0;
  }

  // Try different vertices as a next candidate in Hamiltonian cycle.
  // We don't try for 0 as we included 0 as starting point in
  // hamiltonian_cycle()
  for (int v = 1; v < N; v++) {
    // Check if this vertex can be added to Hamiltonian cycle
    if (is_safe(v, graph, path, pos)) {
      path[pos] = v;

      // recur to construct rest of the path
      if (hamiltonian_cycle_util(graph, path, pos + 1) == 1) return 1;

      // If adding vertex v doesn't lead to a solution, then remove it
      path[pos] = -1;
    }
  }

  // If no vertex can be added to Hamiltonian Cycle constructed so far,
  // then return false
  return 0;
}

/** This function solves the Hamiltonian cycle problem using Backtracking.
 * It mainly uses hamiltonian_cycle_util() to solve the problem.
 * It returns false if there is no Hamiltonian Cycle possible,
 * otherwise return true and prints the path.
 */
int hamiltonian_cycle(int graph[N][N]) {
  int path[N];
  for (int i = 0; i < N; i++) path[i] = -1;

  // Let us put vertex 0 as the first vertex in the path.
  // If there is a Hamiltonian cycle,
  // then the path can be started from any point
  // of the cycle as the graph is undirected
  path[0] = 0;
  if (hamiltonian_cycle_util(graph, path, 1) == 0) {
    printf("Solution does not exist\n");
    printf("\n");
    return 0;
  }

  print_solution(path);
  return 1;
}

/** Main function */
int main() {
  /* Let us create the following graph
     (0)--(1)--(2)
      |   / \   |
      |  /   \  |
      | /     \ |
     (3)-------(4)    */
  int graph1[N][N] = {
      {0, 1, 0, 1, 0}, {1, 0, 1, 1, 1}, {0, 1, 0, 0, 1},
      {1, 1, 0, 0, 1}, {0, 1, 1, 1, 0},
  };

  // Print the solution
  printf("Test Case 1:\n");
  hamiltonian_cycle(graph1);

  /* Let us create the following graph
     (0)--(1)--(2)
      |   / \   |
      |  /   \  |
      | /     \ |
     (3)       (4)    */
  int graph2[N][N] = {
      {0, 1, 0, 1, 0}, {1, 0, 1, 1, 1}, {0, 1, 0, 0, 1},
      {1, 1, 0, 0, 0}, {0, 1, 1, 0, 0},
  };

  // Print the solution
  printf("Test Case 2:\n");
  hamiltonian_cycle(graph2);

  return 0;
}
