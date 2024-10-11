const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const allocator = std.heap.page_allocator;
const rand = std.rand;
const mem = std.mem;

/// Function to perform Quick Sort on an array.
///
/// # Parameters
///
/// - `arr`: The array to sort.
/// - `low`: The starting index.
/// - `high`: The ending index.
fn quickSort(arr: []usize, low: usize, high: usize) void {
    if (low < high) {
        const pi = partition(arr, low, high);

        if (pi > 0) {
            quickSort(arr, low, pi - 1);
        }
        quickSort(arr, pi + 1, high);
    }
}

/// Function to partition the array on the basis of pivot element.
///
/// # Parameters
///
/// - `arr`: The array to partition.
/// - `low`: The starting index.
/// - `high`: The ending index.
///
/// # Returns
///
/// The partition index.
fn partition(arr: []usize, low: usize, high: usize) usize {
    const pivot = arr[high]; // Choose the last element as pivot
    var i = low;
    var j = low;

    while (j < high) : (j += 1) {
        // If current element is smaller than or equal to pivot
        if (arr[j] <= pivot) {
            // Swap arr[i] and arr[j]
            mem.swap(usize, &arr[i], &arr[j]);
            i += 1;
        }
    }
    // Swap arr[i] and arr[high] (or pivot)
    mem.swap(usize, &arr[i], &arr[high]);

    return i;
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

/// Main function to demonstrate Quick Sort and measure execution time.
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

        // Perform Quick Sort
        const low: usize = 0;
        const high = arr.len - 1;
        quickSort(arr, low, high);

        // Measure the end time
        const end = try Instant.now();

        // Calculate the elapsed time in seconds
        const elapsed: f64 = @floatFromInt(end.since(start));
        const time_spent = elapsed / time.ns_per_s;

        // Verify that the array is sorted
        const is_sorted = isSorted(arr);

        // Print the result and time
        print("Insertion Sort with N = {d}\n", .{N});
        if (is_sorted) {
            print("Array is sorted.\n", .{});
        } else {
            print("Array is NOT sorted.\n", .{});
        }
        print("Time taken: {d} seconds\n\n", .{time_spent});
    }
}
