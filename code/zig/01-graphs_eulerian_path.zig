const std = @import("std");
const print = std.debug.print;

/// Number of vertices in the graph
const N = 5;
/// Maximum length of the path
const MAX_PATH_LENGTH = 20;

/// Find Eulerian Path in a graph
///
/// # Parameters
///
/// - `graph`: Adjacency matrix of the graph
fn findPath(graph: *[N][N]i32) void {
    var numofadj: [N]i32 = [_]i32{0} ** N;
    var stack: [N]i32 = [_]i32{0} ** N;
    var path: [MAX_PATH_LENGTH]i32 = [_]i32{0} ** MAX_PATH_LENGTH;
    var top: usize = 0;
    var path_length: usize = 0;

    // Find out number of edges each vertex has
    for (0..N) |i| {
        for (0..N) |j| {
            numofadj[i] += graph.*[i][j];
        }
    }

    // Find out how many vertices have an odd number of edges
    var startpoint: usize = 0;
    var numofodd: i32 = 0;
    var idx_i: usize = N;
    while (idx_i > 0) : (idx_i -= 1) {
        const idx = idx_i - 1;
        if (@mod(numofadj[idx], 2) == 1) {
            numofodd += 1;
            startpoint = idx;
        }
    }

    // If the number of vertices with odd number of edges is greater than two, return "No Solution".
    if (numofodd > 2) {
        print("No Solution\n", .{});
        return;
    }

    // Initialize empty stack and path; take the starting current as discussed
    var cur: usize = startpoint;

    // Loop will run until there is an element in the stack or current vertex has some neighbor.
    while (top > 0 or numofadj[cur] > 0) {
        // If current node has no neighbors, add it to path and pop stack; set new current to the popped element
        if (numofadj[cur] == 0) {
            path[path_length] = @intCast(cur);
            path_length += 1;
            if (top > 0) {
                top -= 1;
                cur = @intCast(stack[top]);
            } else {
                break;
            }
        }
        // If the current vertex has at least one neighbor, add the current vertex to stack,
        // remove the edge between them, and set the current to its neighbor.
        else {
            for (0..N) |j| {
                if (graph.*[cur][j] == 1) {
                    stack[top] = @intCast(cur);
                    top += 1;
                    graph.*[cur][j] = 0;
                    graph.*[j][cur] = 0;
                    numofadj[cur] -= 1;
                    numofadj[j] -= 1;
                    cur = j;
                    break;
                }
            }
        }
    }

    // Add the last vertex to the path
    path[path_length] = @intCast(cur);
    path_length += 1;

    // Print the path
    var idx: usize = path_length;
    while (idx > 0) : (idx -= 1) {
        print("{d} -> ", .{path[idx - 1]});
    }
    print("\n", .{});
}

/// Main function
pub fn main() !void {
    // Test case 1:
    // 0 --- 1
    // |    | \
    // |    2--3
    // 4
    var graph1: [N][N]i32 = .{
        .{ 0, 1, 0, 0, 1 },
        .{ 1, 0, 1, 1, 0 },
        .{ 0, 1, 0, 1, 0 },
        .{ 0, 1, 1, 0, 0 },
        .{ 1, 0, 0, 0, 0 },
    };

    print("Test Case 1:\n", .{});
    findPath(&graph1);

    // Test case 2:
    //   0 -- 1 -- 2
    //  /|  / \  | \
    // 3--4      5
    var graph2: [N][N]i32 = .{
        .{ 0, 1, 0, 1, 1 },
        .{ 1, 0, 1, 0, 1 },
        .{ 0, 1, 0, 1, 1 },
        .{ 1, 0, 1, 0, 0 },
        .{ 1, 1, 1, 0, 0 },
    };

    print("Test Case 2:\n", .{});
    findPath(&graph2);

    // Test case 3:
    //   0 --- 1
    //  /|\     |\
    // 2  4---5  3
    var graph3: [N][N]i32 = .{
        .{ 0, 1, 1, 0, 1 },
        .{ 1, 0, 0, 1, 1 },
        .{ 1, 0, 0, 1, 0 },
        .{ 0, 1, 1, 0, 1 },
        .{ 1, 1, 0, 1, 0 },
    };

    print("Test Case 3:\n", .{});
    findPath(&graph3);
}
