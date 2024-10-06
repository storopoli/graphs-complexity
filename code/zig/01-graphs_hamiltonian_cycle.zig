const std = @import("std");
const print = std.debug.print;

/// Number of vertices in the graph
const N = 5;

/// A utility function to print the solution
fn printSolution(path: [N]i32) void {
    print("Solution Exists: ", .{});
    for (0..N) |i| {
        print("{d} ", .{path[i]});
    }
    // Print the first vertex again to show the complete cycle
    print("{d}\n\n", .{path[0]});
}

/// A utility function to check if the vertex `v` can be added at index `pos`
/// in the Hamiltonian cycle constructed so far (stored in `path`)
fn isSafe(v: i32, graph: *[N][N]i32, path: [N]i32, pos: usize) bool {
    // Check if this vertex is an adjacent vertex of the previously added vertex.
    if (graph.*[@intCast(path[pos - 1])][@intCast(v)] == 0) return false;

    // Check if the vertex has already been included.
    for (0..pos) |i| {
        if (path[i] == v) return false;
    }

    return true;
}

/// A recursive utility function to solve the Hamiltonian cycle problem
fn hamiltonianCycleUtil(graph: *[N][N]i32, path: *[N]i32, pos: usize) bool {
    // Base case: If all vertices are included in the Hamiltonian cycle
    if (pos == N) {
        // And if there is an edge from the last included vertex to the first vertex
        if (graph.*[@intCast(path[pos - 1])][@intCast(path[0])] == 1) {
            return true;
        } else {
            return false;
        }
    }

    // Try different vertices as the next candidate in the Hamiltonian cycle.
    // We don't try for 0 as we included 0 as the starting point in hamiltonianCycle()
    for (1..N) |v| {
        const v_int: i32 = @intCast(v);
        // Check if this vertex can be added to the Hamiltonian cycle
        if (isSafe(v_int, graph, path.*, pos)) {
            path.*[pos] = v_int;

            // Recur to construct the rest of the path
            if (hamiltonianCycleUtil(graph, path, pos + 1))
                return true;

            // If adding vertex `v` doesn't lead to a solution, then remove it
            path.*[pos] = -1;
        }
    }

    // If no vertex can be added to the Hamiltonian cycle constructed so far,
    // then return false
    return false;
}

/// This function solves the Hamiltonian cycle problem using backtracking.
/// It mainly uses `hamiltonianCycleUtil()` to solve the problem.
/// It returns false if there is no Hamiltonian cycle possible,
/// otherwise returns true and prints the path.
fn hamiltonianCycle(graph: *[N][N]i32) bool {
    var path: [N]i32 = [_]i32{-1} ** N;

    // Let us put vertex 0 as the first vertex in the path.
    // If there is a Hamiltonian cycle,
    // then the path can be started from any point
    // of the cycle as the graph is undirected
    path[0] = 0;
    if (!hamiltonianCycleUtil(graph, &path, 1)) {
        print("Solution does not exist\n\n", .{});
        return false;
    }

    printSolution(path);
    return true;
}

/// Main function
pub fn main() !void {
    // Test case 1:
    // (0)--(1)--(2)
    //  |   / \   |
    //  |  /   \  |
    //  | /     \ |
    // (3)-------(4)
    var graph1: [N][N]i32 = .{
        .{ 0, 1, 0, 1, 0 },
        .{ 1, 0, 1, 1, 1 },
        .{ 0, 1, 0, 0, 1 },
        .{ 1, 1, 0, 0, 1 },
        .{ 0, 1, 1, 1, 0 },
    };

    _ = hamiltonianCycle(&graph1);

    // Test case 2:
    // (0)--(1)--(2)
    //  |   / \   |
    //  |  /   \  |
    //  | /     \ |
    // (3)       (4)
    var graph2: [N][N]i32 = .{
        .{ 0, 1, 0, 1, 0 },
        .{ 1, 0, 1, 1, 1 },
        .{ 0, 1, 0, 0, 1 },
        .{ 1, 1, 0, 0, 0 },
        .{ 0, 1, 1, 0, 0 },
    };

    print("Test Case 2:\n", .{});
    _ = hamiltonianCycle(&graph2);
}
