const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const allocator = std.heap.page_allocator;

/// Function to perform linear search on an array.
///
/// # Parameters
///
/// - `arr`: The array to search in.
/// - `x`: The target value to search for.
///
/// # Returns
///
/// The index of the target if found, otherwise `null`.
fn linearSearch(arr: []const usize, x: usize) ?usize {
    for (0.., arr) |index, item| {
        if (item == x) return index; // Element found at position index
    }
    return null; // Element not found
}

/// Main function to demonstrate linear search and measure execution time.
pub fn main() !void {
    const N_values = [_]usize{ 1_000_000, 2_000_000, 4_000_000, 8_000_000, 16_000_000 };

    for (N_values) |N| {
        // Allocate memory for the array
        var arr = try allocator.alloc(usize, N);
        defer allocator.free(arr);

        // Fill the array with sequential elements
        for (0..N) |i| {
            arr[i] = i;
        }

        // Set the target value to be at the end of the array
        // (worst case for linear search)
        const target: usize = N - 1;

        // Measure the start time
        const start = try Instant.now();

        // Perform linear search
        const result = linearSearch(arr[0..], target);

        // Measure the end time
        const end = try Instant.now();

        // Calculate the elapsed time in seconds
        const elapsed: f64 = @floatFromInt(end.since(start));
        const time_spent = elapsed / time.ns_per_s;

        // Print the result and time
        print("Linear Search with N = {d}\n", .{N});
        if (result) |found| {
            print("Target found at index {d}\n", .{found});
        } else {
            print("Target not found\n", .{});
        }
        print("Time taken: {d} seconds\n\n", .{time_spent});
    }
}
