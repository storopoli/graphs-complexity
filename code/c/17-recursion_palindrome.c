#include <stdio.h>
#include <string.h>

/**
 * Function to check if a string is a palindrome recursively.
 *
 * @param str The string to check.
 * @param left The left index for comparison.
 * @param right The right index for comparison.
 * @return 1 if the string is a palindrome, 0 otherwise.
 */
int is_palindrome_recursive(const char *str, int left, int right) {
  // Base case: If left index is greater or equal to right, all characters have
  // been checked
  if (left >= right) return 1;

  // If characters at current indices do not match, it's not a palindrome
  if (str[left] != str[right]) return 0;

  // Move towards the center
  return is_palindrome_recursive(str, left + 1, right - 1);
}

/**
 * Helper function to check if a string is a palindrome.
 *
 * @param str The string to check.
 * @return 1 if the string is a palindrome, 0 otherwise.
 */
int is_palindrome(const char *str) {
  int length = strlen(str);
  return is_palindrome_recursive(str, 0, length - 1);
}

/**
 * Main function to demonstrate the palindrome checker.
 */
int main() {
  const char *test_strings[] = {"radar",   "level", "hello",
                                "racecar", "madam", "step on no pets",
                                "",        "abcba", "not a palindrome"};
  int num_tests = sizeof(test_strings) / sizeof(test_strings[0]);

  for (int i = 0; i < num_tests; i++) {
    const char *str = test_strings[i];
    printf("Testing: \"%s\"\n", str);
    if (is_palindrome(str))
      printf("Result: It's a palindrome.\n\n");
    else
      printf("Result: It's not a palindrome.\n\n");
  }

  return 0;
}
