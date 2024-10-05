#include <stdio.h>
#include <time.h>

/**
 * Function to calculate x raised to the power n using divide and conquer.
 *
 * @param x The base value.
 * @param n The exponent value (can be negative).
 * @return The result of x raised to the power n.
 */
double power(double x, int n) {
  if (n == 0) return 1.0;

  if (n < 0) {
    x = 1.0 / x;
    n = -n;
  }

  double half = power(x, n / 2);

  if (n % 2 == 0)
    return half * half;
  else
    return x * half * half;
}

/**
 * Main function to demonstrate the power function and measure execution time.
 */
int main() {
  double x = 2.0;
  int N_values[] = {10, 20, 40, 80, 160, 320, 640};
  int num_tests = sizeof(N_values) / sizeof(N_values[0]);

  for (int t = 0; t < num_tests; t++) {
    int n = N_values[t];

    // Measure start time
    clock_t start_time = clock();

    // Calculate x^n
    double result = power(x, n);

    // Measure end time
    clock_t end_time = clock();

    // Calculate elapsed time in seconds
    double time_spent = (double)(end_time - start_time) / CLOCKS_PER_SEC;

    // Print the result and time
    printf("Calculating %.1f^%d\n", x, n);
    printf("Result: %e\n", result);
    printf("Time taken: %f seconds\n\n", time_spent);
  }

  return 0;
}
