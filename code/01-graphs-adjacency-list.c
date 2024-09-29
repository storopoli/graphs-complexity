#include <stdio.h>

/** Function to create an adjacency list */
void print_adj_list(int arr[5][5]) {
  // Initialize to all zeros
  int adj_list[5][5] = {{0}};

  for (int i = 0; i < 5; ++i) {
    printf("%d: ", i + 1);
    for (int j = 0; j < 5; ++j) {
      if (arr[i][j] == 1) {
        adj_list[i][adj_list[i][0]] = j;  // Store the neighbor in the list
        printf("%d ", j + 1);
        adj_list[i][0]++;  // Increment the count of neighbors for this vertex
      }
    }
    printf("\n");
  }
}

/** Main Function */
int main() {
  // Given matrix
  int arr[5][5] = {
      {1, 0, 1, 0, 0}, {0, 1, 0, 0, 0}, {0, 1, 0, 0, 1},
      {0, 0, 0, 0, 1}, {1, 0, 0, 1, 0},
  };

  print_adj_list(arr);

  return 0;
}
