#include <stdio.h>

/** Function that verifies if there is a subset that sums to the target
 * @param set[] The set of numbers
 * @param n The size of the set
 * @param target The target sum
 * @return 1 if there is a subset that sums to the target, 0 otherwise
 */
int subset_sum(int set[], int n, int target) {
  // FIXME: Implement the subset sum algorithm
  return 0;
}

/** Main function */
int main() {
  int set[] = {1, 2, 3, 4, 5};
  int n = sizeof(set) / sizeof(set[0]);
  int target = 9;

  if (subset_sum(set, n, target)) {
    printf("Yes, there is a subset that sums to %d\n", target);
  } else {
    printf("No, there isn't a subset that sums to %d\n", target);
  }

  return 0;
}
