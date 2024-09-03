#include <stdio.h>

/**
 * Function to find the maximum of two integers.
 *
 * @param a The first integer.
 * @param b The second integer.
 * @return The maximum of the two integers.
 */
int max(int a, int b) { return (a > b) ? a : b; }

/**
 * Recursive function to solve the 0/1 Knapsack problem using brute force.
 *
 * @param W The remaining capacity of the knapsack.
 * @param weights[] Array containing the weights of the items.
 * @param values[] Array containing the values of the items.
 * @param n The number of items remaining to consider.
 * @return The maximum value that can be obtained with the given capacity.
 */
int knapsack(int W, int weights[], int values[], int n) {
  // Base case: no items left or no capacity left
  if (n == 0 || W == 0) {
    return 0;
  }

  // If the weight of the nth item is more than the remaining capacity, it
  // cannot be included
  if (weights[n - 1] > W) {
    return knapsack(W, weights, values, n - 1);
  } else {
    // Return the maximum of two cases:
    // 1. nth item included
    // 2. nth item not included
    return max(
        values[n - 1] + knapsack(W - weights[n - 1], weights, values, n - 1),
        knapsack(W, weights, values, n - 1));
  }
}

/**
 * Main function to demonstrate the brute force solution for the knapsack
 * problem.
 */
int main() {
  int values[] = {60, 100, 120};  // Values of the items
  int weights[] = {10, 20, 30};   // Weights of the items
  int W = 50;                     // Maximum capacity of the knapsack
  int n = sizeof(values) / sizeof(values[0]);  // Number of items

  // Calculate the maximum value that can be carried in the knapsack
  int max_value = knapsack(W, weights, values, n);

  // Print the result
  printf("Maximum value that can be carried: %d\n", max_value);

  return 0;
}
