const std = @import("std");

/// Function to check if a string is a palindrome recursively.
///
/// # Parameters
///
/// - `str`: The string to check.
/// - `left`: The left index for comparison.
/// - `right`: The right index for comparison.
///
/// # Returns
///
/// `true` if the string is a palindrome, `false` otherwise.
fn isPalindromeRecursive(str: []const u8, left: usize, right: usize) bool {
    // Base case: If left index is greater or equal to right, all characters have been checked
    if (left >= right) {
        return true;
    }

    // If characters at current indices do not match, it's not a palindrome
    if (str[left] != str[right]) {
        return false;
    }

    // Move towards the center
    return isPalindromeRecursive(str, left + 1, right - 1);
}

/// Helper function to check if a string is a palindrome.
///
/// # Parameters
///
/// - `str`: The string to check.
///
/// # Returns
///
/// `true` if the string is a palindrome, `false` otherwise.
fn isPalindrome(str: []const u8) bool {
    if (str.len == 0) {
        return true;
    }
    return isPalindromeRecursive(str, 0, str.len - 1);
}

/// Main function to demonstrate the palindrome checker.
pub fn main() !void {
    const test_strings = [_][]const u8{
        "radar",
        "level",
        "hello",
        "racecar",
        "madam",
        "step on no pets",
        "",
        "abcba",
        "not a palindrome",
    };

    for (test_strings) |str| {
        std.debug.print("Testing: \"{s}\"\n", .{str});
        if (isPalindrome(str)) {
            std.debug.print("Result: It's a palindrome.\n\n", .{});
        } else {
            std.debug.print("Result: It's not a palindrome.\n\n", .{});
        }
    }
}
