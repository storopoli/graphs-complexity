#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Function to heapify a subtree rooted with node i which is an index in arr[].
 *
 * @param arr The array to heapify.
 * @param n The size of the heap.
 * @param i The index of the root node of the subtree.
 */
void heapify(int arr[], int n, int i) {
  int largest = i;        // Initialize largest as root
  int left = 2 * i + 1;   // left = 2*i + 1
  int right = 2 * i + 2;  // right = 2*i + 2

  // If left child exists and is greater than root
  if (left < n && arr[left] > arr[largest]) largest = left;

  // If right child exists and is greater than largest so far
  if (right < n && arr[right] > arr[largest]) largest = right;

  // If largest is not root
  if (largest != i) {
    // Swap arr[i] with arr[largest]
    int temp = arr[i];
    arr[i] = arr[largest];
    arr[largest] = temp;

    // Recursively heapify the affected sub-tree
    heapify(arr, n, largest);
  }
}

/**
 * Function to perform Heap Sort on an array.
 *
 * @param arr The array to sort.
 * @param n The size of the array.
 */
void heap_sort(int arr[], int n) {
  // Build a maxheap (rearrange array)
  for (int i = n / 2 - 1; i >= 0; i--) heapify(arr, n, i);

  // One by one extract elements from heap
  for (int i = n - 1; i >= 0; i--) {
    // Move current root to end
    int temp = arr[0];
    arr[0] = arr[i];
    arr[i] = temp;

    // Call max heapify on the reduced heap
    heapify(arr, i, 0);
  }
}

/**
 * Main function to demonstrate Heap Sort and measure execution time.
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

    // Perform Heap Sort
    heap_sort(arr, N);

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
    printf("Heap Sort with N = %d\n", N);
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
