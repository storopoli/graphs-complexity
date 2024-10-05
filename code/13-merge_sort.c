#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Function to merge two subarrays of arr[].
 * First subarray is arr[left..mid]
 * Second subarray is arr[mid+1..right]
 *
 * @param arr The main array containing both subarrays.
 * @param left The starting index of the first subarray.
 * @param mid The ending index of the first subarray.
 * @param right The ending index of the second subarray.
 */
void merge(int arr[], int left, int mid, int right) {
  int n1 = mid - left + 1;
  int n2 = right - mid;

  // Create temp arrays
  int *L = (int *)malloc(n1 * sizeof(int));
  int *R = (int *)malloc(n2 * sizeof(int));

  // Copy data to temp arrays L[] and R[]
  for (int i = 0; i < n1; i++) L[i] = arr[left + i];
  for (int j = 0; j < n2; j++) R[j] = arr[mid + 1 + j];

  // Merge the temp arrays back into arr[left..right]
  int i = 0;     // Initial index of first subarray
  int j = 0;     // Initial index of second subarray
  int k = left;  // Initial index of merged subarray

  while (i < n1 && j < n2) {
    if (L[i] <= R[j]) {
      arr[k++] = L[i++];
    } else {
      arr[k++] = R[j++];
    }
  }

  // Copy the remaining elements of L[], if any
  while (i < n1) arr[k++] = L[i++];

  // Copy the remaining elements of R[], if any
  while (j < n2) arr[k++] = R[j++];

  // Free the temporary arrays
  free(L);
  free(R);
}

/**
 * Function to perform Merge Sort on an array.
 *
 * @param arr The array to sort.
 * @param left The starting index of the array.
 * @param right The ending index of the array.
 */
void merge_sort(int arr[], int left, int right) {
  if (left < right) {
    // Same as (left + right)/2 but avoids overflow
    int mid = left + (right - left) / 2;

    // Sort first and second halves
    merge_sort(arr, left, mid);
    merge_sort(arr, mid + 1, right);

    merge(arr, left, mid, right);
  }
}

/**
 * Main function to demonstrate Merge Sort and measure execution time.
 */
int main() {
  int N_values[] = {100000, 200000, 400000, 800000};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);

  for (int t = 0; t < num_tests; t++) {
    int N = N_values[t];
    int *arr = (int *)malloc(N * sizeof(int));

    // Seed the random number generator
    srand(time(NULL));

    // Fill the array with random integers
    for (int i = 0; i < N; i++) {
      arr[i] = rand();
    }

    // Measure the start time
    clock_t start_time = clock();

    // Perform Merge Sort
    merge_sort(arr, 0, N - 1);

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
    printf("Merge Sort with N = %d\n", N);
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
