const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;

/// Function to calculate `x` raised to the power n using divide and conquer.
///
/// # Parameters
///
/// - `x`: The base value (can be negative or fractional).
/// - `n`: The exponent value (can be negative).
///
/// # Returns
///
/// The result of `x` raised to the power n as a `f64` (double precision).
fn power(x: f64, n: i32) f64 {
    if (n == 0) {
        return 1.0;
    }

    var base = x;
    var exponent = n;

    if (exponent < 0) {
        base = 1.0 / base;
        exponent = -exponent;
    }

    const half = power(base, @divFloor(exponent, 2));

    if (@mod(exponent, 2) == 0) {
        return half * half;
    } else {
        return base * half * half;
    }
}

/// Main function to demonstrate the power function and measure execution time.
pub fn main() !void {
    const x: f64 = 2.0;
    const N_values = [_]i32{ 10, 20, 40, 80, 160, 320, 640 };

    for (N_values) |n| {
        // Measure start time
        const start = try Instant.now();

        // Calculate x^n
        const result = power(x, n);

        // Measure the end time
        const end = try Instant.now();

        // Calculate the elapsed time in seconds
        const elapsed: f64 = @floatFromInt(end.since(start));
        const time_spent = elapsed / time.ns_per_s;

        // Print the result and time
        print("Calculating {d} ^ {d}\n", .{ x, n });
        print("Result: {e}\n", .{result});
        print("Time taken: {d} seconds\n\n", .{time_spent});
    }
}
