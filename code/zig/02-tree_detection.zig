const std = @import("std");
const print = @import("std").debug.print;

/// Number of vertices in the graph
const N = 5;

/// Search for a cycle in a graph
///
/// # Parameters
///
/// - `graph`: adjacency matrix of the graph
/// - `v`: current vertex
/// - `visited`: array of visited vertices
/// - `parent`: parent of the current vertex
///
/// # Returns
///
/// `true` if a cycle is found, `false` otherwise
fn searchCycle(graph: *[N][N]i32, v: usize, visited: *[N]bool, parent: isize) bool {
    visited[v] = true;
    for (0..N) |i| {
        if (graph.*[v][i] != 0) {
            if (!visited[i]) {
                if (searchCycle(graph, i, visited, @intCast(v))) return true;
            } else if (@as(isize, @intCast(i)) != parent) {
                // Cycle detected
                return true;
            }
        }
    }
    return false;
}

/// Check if all vertices have been visited
///
/// # Parameters
///
/// - `visited`: array of visited vertices
///
/// # Returns
///
/// `true` if all vertices have been visited, `false` otherwise
fn allVisited(visited: [N]bool) bool {
    var i: usize = 0;
    // Find first unvisited vertex
    while (i < N and visited[i]) : (i += 1) {}
    // Return true if no unvisited vertices found, false otherwise
    return i == N;
}

/// Check if a graph is a tree
///
/// # Parameters
///
/// - `graph`: adjacency matrix of the graph
///
/// # Returns
///
/// `true` if the graph is a tree, `false` otherwise
fn isTree(graph: *[N][N]i32) bool {
    var visited: [N]bool = [_]bool{false} ** N;

    // Perform depth-first search from vertex 0 to check if all vertices
    // are reachable (connected)
    const has_cycle = searchCycle(graph, 0, &visited, -1);
    if (has_cycle) return false;

    // Check if all vertices have been visited
    if (!allVisited(visited)) return false;

    // Return true if no cycle is found and all vertices are connected
    return true;
}

/// Main function
pub fn main() !void {
    // Tree example
    // (0)--(1)--(2)
    //  |
    //  |
    //  |
    // (3)-------(4)
    var graph1: [N][N]i32 = .{
        .{ 0, 1, 0, 1, 0 },
        .{ 1, 0, 1, 0, 0 },
        .{ 0, 1, 0, 0, 0 },
        .{ 1, 0, 0, 0, 1 },
        .{ 0, 0, 0, 1, 0 },
    };
    print("Test Case 1: Is Tree? {}\n", .{isTree(&graph1)});

    // Non-tree example (has at least one cycle)
    // (0)--(1)--(2)
    //  |   / \   |
    //  |  /   \  |
    //  | /     \ |
    // (3)-------(4)
    var graph2: [N][N]i32 = .{
        .{ 0, 1, 0, 1, 0 },
        .{ 1, 0, 1, 1, 1 },
        .{ 0, 1, 0, 0, 1 },
        .{ 1, 1, 0, 0, 1 },
        .{ 0, 1, 1, 1, 0 },
    };
    print("Test Case 1: Is Tree? {}\n", .{isTree(&graph2)});
}
