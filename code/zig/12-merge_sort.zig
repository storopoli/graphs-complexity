const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const allocator = std.heap.page_allocator;
const Allocator = std.mem.Allocator;
const rand = std.rand;

/// Function to perform Merge Sort on an array.
///
/// # Parameters
///
/// - `allocator`: The allocator to use for temporary arrays.
/// - `arr`: The array to sort.
/// - `left`: The starting index of the array.
/// - `right`: The ending index of the array.
fn mergeSort(alloc: Allocator, arr: []usize, left: usize, right: usize) !void {
    if (left < right) {
        // Same as (left + right)/2 but avoids overflow
        const mid: usize = left + @divFloor(right - left, 2);

        // Sort first and second halves
        try mergeSort(alloc, arr, left, mid);
        try mergeSort(alloc, arr, mid + 1, right);

        try merge(alloc, arr, left, mid, right);
    }
}

/// Function to merge two subarrays of `arr`.
/// First subarray is `arr[left..mid+1]`
/// Second subarray is `arr[mid+1..right+1]`
///
/// # Parameters
///
/// - `allocator`: The allocator to use for temporary arrays.
/// - `arr`: The main array containing both subarrays.
/// - `left`: The starting index of the first subarray.
/// - `mid`: The ending index of the first subarray.
/// - `right`: The ending index of the second subarray.
fn merge(alloc: Allocator, arr: []usize, left: usize, mid: usize, right: usize) !void {
    const n1 = mid - left + 1;
    const n2 = right - mid;

    // Create temp arrays
    var L = try alloc.alloc(usize, n1);
    defer alloc.free(L);
    var R = try alloc.alloc(usize, n2);
    defer alloc.free(R);

    // Copy data to temp arrays L[] and R[]
    for (0..n1) |i| L[i] = arr[left + i];
    for (0..n2) |j| R[j] = arr[mid + 1 + j];

    // Merge the temp arrays back into arr[left..right]
    var i: usize = 0; // Initial index of first subarray
    var j: usize = 0; // Initial index of second subarray
    var k: usize = left; // Initial index of merged subarray

    while (i < n1 and j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i += 1;
        } else {
            arr[k] = R[j];
            j += 1;
        }
        k += 1;
    }

    // Copy the remaining elements of L[], if any
    while (i < n1) : ({
        i += 1;
        k += 1;
    }) {
        arr[k] = L[i];
    }

    // Copy the remaining elements of R[], if any
    while (j < n2) : ({
        j += 1;
        k += 1;
    }) {
        arr[k] = R[j];
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

/// Main function to demonstrate Merge Sort and measure execution time.
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

        // Perform Merge Sort
        const left: usize = 0;
        const right = arr.len - 1;
        try mergeSort(allocator, arr, left, right);

        // Measure the end time
        const end = try Instant.now();

        // Calculate the elapsed time in seconds
        const elapsed: f64 = @floatFromInt(end.since(start));
        const time_spent = elapsed / time.ns_per_s;

        // Verify that the array is sorted
        const is_sorted = isSorted(arr);

        // Print the result and time
        print("Merge Sort with N = {d}\n", .{N});
        if (is_sorted) {
            print("Array is sorted.\n", .{});
        } else {
            print("Array is NOT sorted.\n", .{});
        }
        print("Time taken: {d} seconds\n\n", .{time_spent});
    }
}
