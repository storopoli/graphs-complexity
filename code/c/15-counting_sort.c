#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/**
 * Function to perform Counting Sort on an array.
 * Assumes that all elements are non-negative integers.
 *
 * @param arr The array to sort.
 * @param n The size of the array.
 * @param max_value The maximum possible value in the array.
 */
void counting_sort(int arr[], int n, int max_value) {
  int *count = (int *)calloc(max_value + 1, sizeof(int));
  int *output = (int *)malloc(n * sizeof(int));

  // Store the count of each element
  for (int i = 0; i < n; i++) count[arr[i]]++;

  // Change count[i] so that count[i] now contains actual position of this
  // element in output array
  for (int i = 1; i <= max_value; i++) count[i] += count[i - 1];

  // Build the output array
  for (int i = n - 1; i >= 0; i--) {
    output[count[arr[i]] - 1] = arr[i];
    count[arr[i]]--;
  }

  // Copy the output array to arr[]
  memcpy(arr, output, n * sizeof(int));

  // Free allocated memory
  free(count);
  free(output);
}

/**
 * Main function to demonstrate Counting Sort and measure execution time.
 */
int main() {
  int N_values[] = {100000, 200000, 400000, 800000};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);
  int max_value = 1000000;  // Define a maximum value for elements in the array

  for (int t = 0; t < num_tests; t++) {
    int N = N_values[t];
    int *arr = (int *)malloc(N * sizeof(int));

    // Seed the random number generator
    srand(123);

    // Fill the array with random integers between 0 and max_value
    for (int i = 0; i < N; i++) {
      arr[i] = rand() % (max_value + 1);
    }

    // Measure the start time
    clock_t start_time = clock();

    // Perform Counting Sort
    counting_sort(arr, N, max_value);

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
    printf("Counting Sort with N = %d\n", N);
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
