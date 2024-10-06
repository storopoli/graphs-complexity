const std = @import("std");
const print = std.debug.print;

/// Function to find the maximum of two integers.
///
/// # Parameters
///
/// - `a`: The first integer.
/// - `b`: The second integer.
///
/// # Returns
///
/// The maximum of the two integers.
fn max(a: i32, b: i32) i32 {
    return if (a > b) a else b;
}

/// Recursive function to solve the 0/1 Knapsack problem using brute force.
///
/// # Parameters
///
/// - `W`: The remaining capacity of the knapsack.
/// - `weights`: Array containing the weights of the items.
/// - `values`: Array containing the values of the items.
/// - `n`: The number of items remaining to consider.
///
/// # Returns
///
/// The maximum value that can be obtained with the given capacity.
fn knapsack(W: i32, weights: []const i32, values: []const i32, n: usize) i32 {
    // Base case: no items left or no capacity left
    if (n == 0 or W == 0) {
        return 0;
    }

    // If the weight of the nth item is more than the remaining capacity, it cannot be included
    if (weights[n - 1] > W) {
        return knapsack(W, weights, values, n - 1);
    } else {
        // Return the maximum of two cases:
        // 1. nth item included
        // 2. nth item not included
        return max(
            values[n - 1] + knapsack(W - weights[n - 1], weights, values, n - 1),
            knapsack(W, weights, values, n - 1),
        );
    }
}

/// Main function to demonstrate the brute force solution for the knapsack problem.
pub fn main() !void {
    const values = [_]i32{ 60, 100, 120 }; // Values of the items
    const weights = [_]i32{ 10, 20, 30 }; // Weights of the items
    const W: i32 = 50; // Maximum capacity of the knapsack
    const n: usize = values.len; // Number of items

    // Calculate the maximum value that can be carried in the knapsack
    const max_value = knapsack(W, weights[0..], values[0..], n);

    // Print the result
    print("Maximum value that can be carried: {d}\n", .{max_value});
}
