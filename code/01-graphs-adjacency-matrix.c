#include <stdio.h>

/** N vertices and M edges */
int N, M;

/** Function to create an adjacency matrix */
void create_adj_matrix(int adj[][N + 1], int arr[][2]) {
  // Initialize all value to zero
  // FIXME: add the necessary code here

  // Traverse the array of edges and update the values
  // FIXME: add the necessary code here
}

/** Function to print an adjacency matrix */
void print_adj_matrix(int adj[][N + 1]) {
  // Traverse the adj[][]
  for (int i = 1; i < N + 1; i++) {
    for (int j = 1; j < N + 1; j++) {
      // Print the value at adj[i][j]
      printf("%d ", adj[i][j]);
    }
    printf("\n");
  }
}

/** Main function */
int main() {
  // Number of vertices
  N = 5;

  // Given edges
  int arr[][2] = {{1, 2}, {2, 3}, {4, 5}, {1, 5}};

  // Number of edges
  M = sizeof(arr) / sizeof(arr[0]);

  int adj[N + 1][N + 1];

  create_adj_matrix(adj, arr);

  print_adj_matrix(adj);

  return 0;
}
