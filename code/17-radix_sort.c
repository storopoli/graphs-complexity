#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define BASE 10  // Base of numeral system (decimal)

/**
 * Function to get the maximum value in an array.
 *
 * @param arr The array to search.
 * @param n The size of the array.
 * @return The maximum value in the array.
 */
int get_max(int arr[], int n) {
  int max = arr[0];
  for (int i = 1; i < n; i++)
    if (arr[i] > max) max = arr[i];
  return max;
}

/**
 * Function to perform Counting Sort based on a specific digit represented by
 * exp.
 *
 * @param arr The array to sort.
 * @param n The size of the array.
 * @param exp The exponent representing the digit position.
 */
void counting_sort_by_digit(int arr[], int n, int exp) {
  int *output = (int *)malloc(n * sizeof(int));
  int count[BASE] = {0};

  // Store count of occurrences in count[]
  for (int i = 0; i < n; i++) count[(arr[i] / exp) % BASE]++;

  // Change count[i] so that count[i] now contains actual position of this digit
  // in output[]
  for (int i = 1; i < BASE; i++) count[i] += count[i - 1];

  // Build the output array
  for (int i = n - 1; i >= 0; i--) {
    int digit = (arr[i] / exp) % BASE;
    output[count[digit] - 1] = arr[i];
    count[digit]--;
  }

  // Copy the output array to arr[]
  memcpy(arr, output, n * sizeof(int));

  // Free allocated memory
  free(output);
}

/**
 * Function to perform Radix Sort on an array.
 *
 * @param arr The array to sort.
 * @param n The size of the array.
 */
void radix_sort(int arr[], int n) {
  // Find the maximum number to know the number of digits
  int max = get_max(arr, n);

  // Do counting sort for every digit
  for (int exp = 1; max / exp > 0; exp *= BASE)
    counting_sort_by_digit(arr, n, exp);
}

/**
 * Main function to demonstrate Radix Sort and measure execution time.
 */
int main() {
  int N_values[] = {100000, 200000, 400000, 800000};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);
  int max_value = 1000000;  // Define a maximum value for elements in the array

  for (int t = 0; t < num_tests; t++) {
    int N = N_values[t];
    int *arr = (int *)malloc(N * sizeof(int));

    // Seed the random number generator
    srand(time(NULL));

    // Fill the array with random integers between 0 and max_value
    for (int i = 0; i < N; i++) {
      arr[i] = rand() % (max_value + 1);
    }

    // Measure the start time
    clock_t start_time = clock();

    // Perform Radix Sort
    radix_sort(arr, N);

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
    printf("Radix Sort with N = %d\n", N);
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
