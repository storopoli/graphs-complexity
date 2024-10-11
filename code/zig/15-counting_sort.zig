const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const Allocator = std.mem.Allocator;
const allocator = std.heap.page_allocator;
const rand = std.rand;

/// Function to perform Counting Sort on an array.
///
/// # Parameters
///
/// - `allocator`: The allocator to use for temporary arrays.
/// - `arr`: The array to sort.
/// - `max_value`: The maximum possible value in the array.
fn countingSort(alloc: Allocator, arr: []usize, max_value: usize) !void {
    const n = arr.len;

    // Create count array with size max_value + 1, initialized to zero
    var count = try alloc.alloc(usize, max_value + 1);
    defer alloc.free(count);
    @memset(count, 0);

    // Create output array
    var output = try alloc.alloc(usize, n);
    defer alloc.free(output);

    // Store the count of each element
    for (arr) |value| {
        count[value] += 1;
    }

    // Change count[i] so that count[i] now contains actual position
    // of this element in output array
    var total: usize = 0;
    for (0..max_value + 1) |i| {
        const old_count = count[i];
        count[i] = total;
        total += old_count;
    }

    // Build the output array
    for (arr) |value| {
        const index = count[value];
        output[index] = value;
        count[value] += 1;
    }

    // Copy the output array back to arr[]
    @memcpy(arr, output);
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

/// Main function to demonstrate Counting Sort and measure execution time.
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

        // Perform Counting Sort
        const max_value: usize = 1_000_000;
        try countingSort(allocator, arr, max_value);

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
