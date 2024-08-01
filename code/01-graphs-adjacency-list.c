#include <stdio.h>

/** Function to create an adjacency list */
void print_adj_list(int arr[5][5]) {
  // Initialize to all zeros
  int adj_list[5][5] = {{0}};

  // Traverse the given matrix
  // FIXME: add the necessary code here
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
