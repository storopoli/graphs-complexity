const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const allocator = std.heap.page_allocator;
const rand = std.rand;
const mem = std.mem;

/// Function to perform Selection Sort on an array.
///
/// # Parameters
///
/// - `arr`: The array to sort.
fn selectionSort(arr: []usize) void {
    const n = arr.len;
    for (0..n - 1) |i| {
        var min_index = i;

        // Find the minimum element in the unsorted portion
        for (i + 1..n) |j| {
            if (arr[j] < arr[min_index]) {
                min_index = j;
            }
        }

        // Swap the found minimum element with the first unsorted element
        mem.swap(usize, &arr[min_index], &arr[i]);
    }
}

/// Function to verify if an array is sorted.
///
/// # Parameters
///
/// - `arr`: The array to check.
fn isSorted(arr: []usize) bool {
    const n = arr.len;
    for (0..n - 1) |i| {
        if (arr[i] > arr[i + 1]) {
            return false;
        }
    }
    return true;
}

/// Main function to demonstrate Selection Sort and measure execution time.
pub fn main() !void {
    const N_values = [_]usize{ 10_000, 20_000, 30_000, 40_000 };

    // Seed the random number generator
    var rng = rand.DefaultPrng.init(123); // Xoshiro256 is good enough

    for (N_values) |N| {
        // Allocate memory for the array
        var arr = try allocator.alloc(usize, N);
        defer allocator.free(arr);

        // Fill the array with random integers
        for (0..N) |i| {
            const random_value: usize = rng.next() % N;
            arr[i] = random_value;
        }

        // Measure the start time
        const start = try Instant.now();

        // Perform Selection Sort
        selectionSort(arr);

        // Measure the end time
        const end = try Instant.now();

        // Calculate the elapsed time in seconds
        const elapsed: f64 = @floatFromInt(end.since(start));
        const time_spent = elapsed / time.ns_per_s;

        // Verify that the array is sorted
        const is_sorted = isSorted(arr);

        // Print the result and time
        print("Selection Sort with N = {d}\n", .{N});
        if (is_sorted) {
            print("Array is sorted.\n", .{});
        } else {
            print("Array is NOT sorted.\n", .{});
        }
        print("Time taken: {d} seconds\n\n", .{time_spent});
    }
}
