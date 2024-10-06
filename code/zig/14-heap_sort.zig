const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const allocator = std.heap.page_allocator;
const rand = std.rand;
const mem = std.mem;

/// Function to perform Heap Sort on an array.
///
/// # Parameters
///
/// - `arr`: The array to sort.
fn heapSort(arr: []usize) void {
    const n = arr.len;

    if (n <= 1) {
        return;
    }

    // Build a maxheap (rearrange array)
    var i = @divFloor(n, 2) - 1;
    while (i >= 0) : (i -= 1) {
        heapify(arr, n, i);
        if (i == 0) break;
    }

    // One by one extract elements from heap
    i = n - 1;
    while (i > 0) : (i -= 1) {
        // Move current root to end
        mem.swap(usize, &arr[0], &arr[i]);

        // Call max heapify on the reduced heap
        heapify(arr, i, 0);
    }
}

/// Function to heapify a subtree rooted with node `i`, which is an index in `arr`.
///
/// # Parameters
///
/// - `arr`: The array to heapify.
/// - `n`: The size of the heap.
/// - `i`: The index of the root node of the subtree.
fn heapify(arr: []usize, n: usize, i: usize) void {
    var largest: usize = i; // Initialize largest as root
    const left: usize = 2 * i + 1; // left child index
    const right: usize = 2 * i + 2; // right child index

    // If left child exists and is greater than root
    if (left < n and arr[left] > arr[largest]) {
        largest = left;
    }

    // If right child exists and is greater than largest so far
    if (right < n and arr[right] > arr[largest]) {
        largest = right;
    }

    // If largest is not root
    if (largest != i) {
        // Swap arr[i] with arr[largest]
        mem.swap(usize, &arr[i], &arr[largest]);

        // Recursively heapify the affected sub-tree
        heapify(arr, n, largest);
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

/// Main function to demonstrate Insertion Sort and measure execution time.
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

        // Perform Heap Sort
        heapSort(arr);

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
