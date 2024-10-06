const std = @import("std");
const print = std.debug.print;

/// Function that verifies if there is a subset that sums to the target
///
/// # Parameters
///
/// - `set`: The set of numbers
/// - `target`: The target sum
///
/// # Returns
///
/// `true` if there is a subset that sums to the target, `false` otherwise
fn subsetSum(set: []const i32, target: i32) bool {
    const n: usize = set.len;
    const num_subsets = @as(u8, 1) <<| n;
    // Iterate through all possible subsets
    for (0..num_subsets) |i| {
        var sum: i32 = 0;
        for (0..n) |j| {
            // Verify if the j-th element is in the subset
            if ((i & (@as(u8, 1) <<| j)) != 0) {
                sum += set[j];
            }
        }
        // If the subset sums to the target, return true
        if (sum == target) {
            return true;
        }
    }
    return false;
}

/// Main function
pub fn main() !void {
    const set = [_]i32{ 1, 2, 3, 4, 5 };
    const target: i32 = 9;

    if (subsetSum(set[0..], target)) {
        print("Yes, there is a subset that sums to {d}\n", .{target});
    } else {
        print("No, there isn't a subset that sums to {d}\n", .{target});
    }
}
