const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const Allocator = std.mem.Allocator;
const allocator = std.heap.page_allocator;
const rand = std.rand;

/// Base of numeral system (decimal)
const BASE: u32 = 10;

/// Function to perform Radix Sort on an array.
///
/// # Parameters
///
/// - `allocator`: The allocator to use for temporary arrays.
/// - `arr`: The array to sort.
fn radixSort(alloc: Allocator, arr: []usize) !void {
    // Find the maximum number to know the number of digits
    const max = getMax(arr);

    // Do counting sort for every digit
    var exp: usize = 1;
    while (max / exp > 0) : (exp *= BASE) {
        try countingSortByDigit(alloc, arr, exp);
    }
}

/// Function to get the maximum value in an array.
///
/// # Parameters
///
/// - `arr`: The array to search.
///
/// # Returns
///
/// The maximum value in the array.
fn getMax(arr: []usize) usize {
    var max: usize = arr[0];
    for (arr[1..]) |value| {
        if (value > max) {
            max = value;
        }
    }
    return max;
}

/// Function to perform Counting Sort based on a specific digit represented by `exp`.
///
/// # Parameters
///
/// - `allocator`: The allocator to use for temporary arrays.
/// - `arr`: The array to sort.
/// - `exp`: The exponent representing the digit position.
fn countingSortByDigit(alloc: Allocator, arr: []usize, exp: usize) !void {
    const n = arr.len;

    // Create output array
    var output = try alloc.alloc(usize, n);
    defer alloc.free(output);

    // Initialize count array
    var count: [BASE]usize = [_]usize{0} ** BASE;

    // Store count of occurrences in count[]
    for (arr) |value| {
        const digit = (value / exp) % BASE;
        count[digit] += 1;
    }

    // Change count[i] so that count[i] now contains actual position of this digit in output[]
    for (1..BASE) |i| {
        count[i] += count[i - 1];
    }

    // Build the output array
    var i = n - 1;
    while (i >= 0) : (i -= 1) {
        const value = arr[i];
        const digit = (value / exp) % BASE;
        count[digit] -= 1;
        output[count[digit]] = value;
        if (i == 0) break;
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

/// Main function to demonstrate Radix Sort and measure execution time.
pub fn main() !void {
    const N_values = [_]usize{ 10_000, 20_000, 30_000, 40_000 };

    // Seed the random number generator
    const seed: u64 = @intCast(time.nanoTimestamp());
    var rng = rand.DefaultPrng.init(seed); // Xoshiro256 is good enough

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

        // Perform Radix Sort
        try radixSort(allocator, arr);

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
