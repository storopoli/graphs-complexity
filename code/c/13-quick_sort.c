#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Function to partition the array on the basis of pivot element.
 *
 * @param arr The array to partition.
 * @param low The starting index.
 * @param high The ending index.
 * @return The partition index.
 */
int partition(int arr[], int low, int high) {
  int pivot = arr[high];  // Choose the last element as pivot
  int i = low - 1;        // Index of smaller element

  for (int j = low; j <= high - 1; j++) {
    // If current element is smaller than or equal to pivot
    if (arr[j] <= pivot) {
      i++;  // Increment index of smaller element
      // Swap arr[i] and arr[j]
      int temp = arr[i];
      arr[i] = arr[j];
      arr[j] = temp;
    }
  }
  // Swap arr[i + 1] and arr[high] (or pivot)
  int temp = arr[i + 1];
  arr[i + 1] = arr[high];
  arr[high] = temp;
  return i + 1;
}

/**
 * Function to perform Quick Sort on an array.
 *
 * @param arr The array to sort.
 * @param low The starting index.
 * @param high The ending index.
 */
void quick_sort(int arr[], int low, int high) {
  if (low < high) {
    // pi is partitioning index, arr[p] is now at right place
    int pi = partition(arr, low, high);

    // Separately sort elements before partition and after partition
    quick_sort(arr, low, pi - 1);
    quick_sort(arr, pi + 1, high);
  }
}

/**
 * Main function to demonstrate Quick Sort and measure execution time.
 */
int main() {
  int N_values[] = {100000, 200000, 400000, 800000};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);

  for (int t = 0; t < num_tests; t++) {
    int N = N_values[t];
    int *arr = (int *)malloc(N * sizeof(int));

    // Seed the random number generator
    srand(123);

    // Fill the array with random integers
    for (int i = 0; i < N; i++) {
      arr[i] = rand();
    }

    // Measure the start time
    clock_t start_time = clock();

    // Perform Quick Sort
    quick_sort(arr, 0, N - 1);

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
    printf("Quick Sort with N = %d\n", N);
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
