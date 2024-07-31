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

/** This function solves the Hamiltonian cycle problem using Backtracking.
 * It returns false if there is no Hamiltonian Cycle possible,
 * otherwise return true and prints the path.
 */
int hamiltonian_cycle(int graph[N][N]) {
  // FIXME: Implement the function
  // print_solution(path);
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
