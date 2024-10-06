const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const allocator = std.heap.page_allocator;

/// Function to perform binary search on a sorted array.
///
/// # Parameters
///
/// - `arr`: The sorted array to search in.
/// - `x`: The target value to search for.
///
/// # Returns
///
/// The index of the target if found, otherwise `null`.
fn binarySearch(arr: []const usize, x: usize) ?usize {
    var left: usize = 0;
    var right: usize = arr.len - 1;

    while (left <= right) {
        const mid: usize = left + @divFloor(right - left, 2);
        if (arr[mid] == x) {
            // Element found
            return mid;
        }
        if (arr[mid] < x) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    return null; // Element not found
}

/// Main function to demonstrate binary search and measure execution time.
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
        // (worst case for binary search)
        const target: usize = N - 1;

        // Measure the start time
        const start = try Instant.now();

        // Perform binary search
        const result = binarySearch(arr[0..], target);

        // Measure the end time
        const end = try Instant.now();

        // Calculate the elapsed time in seconds
        const elapsed: f64 = @floatFromInt(end.since(start));
        const time_spent = elapsed / time.ns_per_s;

        // Print the result and time
        print("Binary Search with N = {d}\n", .{N});
        if (result) |found| {
            print("Target found at index {d}\n", .{found});
        } else {
            print("Target not found\n", .{});
        }
        print("Time taken: {d} seconds\n\n", .{time_spent});
    }
}
