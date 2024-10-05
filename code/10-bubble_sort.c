#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Function to perform Bubble Sort on an array.
 *
 * @param arr The array to sort.
 * @param n The size of the array.
 */
void bubble_sort(int arr[], int n) {
  for (int i = 0; i < n - 1; i++) {
    for (int j = 0; j < n - i - 1; j++) {
      if (arr[j] > arr[j + 1]) {
        // Swap arr[j] and arr[j + 1]
        int temp = arr[j];
        arr[j] = arr[j + 1];
        arr[j + 1] = temp;
      }
    }
  }
}

/**
 * Main function to demonstrate Bubble Sort and measure execution time.
 */
int main() {
  int N_values[] = {10000, 20000, 30000, 40000};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);

  for (int t = 0; t < num_tests; t++) {
    int N = N_values[t];
    int *arr = (int *)malloc(N * sizeof(int));

    // Seed the random number generator
    srand(time(NULL));

    // Fill the array with random integers
    for (int i = 0; i < N; i++) {
      arr[i] = rand() % N;
    }

    // Measure the start time
    clock_t start_time = clock();

    // Perform Bubble Sort
    bubble_sort(arr, N);

    // Measure the end time
    clock_t end_time = clock();

    // Calculate the elapsed time in seconds
    double time_spent = (double)(end_time - start_time) / CLOCKS_PER_SEC;

    // Verify that the array is sorted
    int is_sorted = 1;
    for (int i = 1; i < N; i++) {
      if (arr[i - 1] > arr[i]) {
        is_sorted = 0;
        break;
      }
    }

    // Print the result and time
    printf("Bubble Sort with N = %d\n", N);
    if (is_sorted)
      printf("Array is sorted.\n");
    else
      printf("Array is NOT sorted.\n");
    printf("Time taken: %f seconds\n\n", time_spent);

    // Free the allocated memory
    free(arr);
  }

  return 0;
}