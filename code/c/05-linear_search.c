#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Function to perform linear search on an array.
 *
 * @param arr The array to search in.
 * @param n The size of the array.
 * @param x The target value to search for.
 * @return The index of the target if found, otherwise -1.
 */
int linear_search(int arr[], int n, int x) {
  for (int i = 0; i < n; i++) {
    if (arr[i] == x) return i;  // Element found at position i
  }
  return -1;  // Element not found
}

/**
 * Main function to demonstrate linear search and measure execution time.
 */
int main() {
  int N_values[] = {1000000, 2000000, 4000000, 8000000, 16000000};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);

  for (int t = 0; t < num_tests; t++) {
    int N = N_values[t];
    int *arr = (int *)malloc(N * sizeof(int));

    // Fill the array with sequential elements
    for (int i = 0; i < N; i++) {
      arr[i] = i;
    }

    // Set the target value to be at the end of the array
    // (worst case for linear search)
    int target = N - 1;

    // Measure the start time
    clock_t start_time = clock();

    // Perform linear search
    int result = linear_search(arr, N, target);

    // Measure the end time
    clock_t end_time = clock();

    // Calculate the elapsed time in seconds
    double time_spent = (double)(end_time - start_time) / CLOCKS_PER_SEC;

    // Print the result and time
    printf("Linear Search with N = %d\n", N);
    if (result != -1)
      printf("Target found at index %d\n", result);
    else
      printf("Target not found\n");
    printf("Time taken: %f seconds\n\n", time_spent);

    // Free the allocated memory
    free(arr);
  }

  return 0;
}
