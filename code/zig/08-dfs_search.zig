const std = @import("std");
const print = std.debug.print;
const time = std.time;
const Instant = time.Instant;
const allocator = std.heap.page_allocator;
const Allocator = std.mem.Allocator;

const MAX_NODES: usize = 50_000; // Adjust as needed

/// Node structure for adjacency list
const Node = struct {
    vertex: usize,
    next: ?*Node,
};

/// Graph structure
const Graph = struct {
    numVertices: usize,
    adjLists: []?*Node, // Array of optional pointers to Nodes
    visited: []bool, // Array of visited flags

    /// Deinitialize the graph by freeing allocated memory
    fn deinit(self: *Graph, alloc: Allocator) void {
        // Free all nodes in adjacency lists
        for (self.adjLists) |nodePtr| {
            var node = nodePtr;
            while (node) |currentNode| {
                const nextNode = currentNode.next;
                alloc.destroy(currentNode);
                node = nextNode;
            }
        }
        alloc.free(self.adjLists);
        alloc.free(self.visited);
    }
};

/// Function to create a node
fn createNode(alloc: Allocator, v: usize) !*Node {
    const newNode = try alloc.create(Node);
    newNode.* = Node{
        .vertex = v,
        .next = null,
    };
    return newNode;
}

/// Function to create a graph
fn createGraph(alloc: Allocator, vertices: usize) !*Graph {
    const graph = try alloc.create(Graph);
    graph.numVertices = vertices;

    graph.adjLists = try alloc.alloc(?*Node, vertices);
    graph.visited = try alloc.alloc(bool, vertices);

    for (0..vertices) |i| {
        graph.adjLists[i] = null;
        graph.visited[i] = false;
    }

    return graph;
}

/// Function to add edge to an undirected graph
fn addEdge(alloc: Allocator, graph: *Graph, src: usize, dest: usize) !void {
    // Add edge from src to dest
    var newNode = try createNode(alloc, dest);
    newNode.next = graph.adjLists[src];
    graph.adjLists[src] = newNode;

    // Since the graph is undirected, add edge from dest to src also
    newNode = try createNode(alloc, src);
    newNode.next = graph.adjLists[dest];
    graph.adjLists[dest] = newNode;
}

/// DFS algorithm
fn dfs(graph: *Graph, vertex: usize) void {
    graph.visited[vertex] = true;

    var temp = graph.adjLists[vertex];
    while (temp) |node| {
        const adjVertex = node.vertex;

        if (!graph.visited[adjVertex]) {
            dfs(graph, adjVertex);
        }
        temp = node.next;
    }
}

/// Main function to demonstrate DFS and measure execution time.
pub fn main() !void {
    const N_values = [_]usize{ 1_000, 5_000, 10_000, 20_000, 50_000 };

    for (N_values) |N| {
        if (N > MAX_NODES) {
            print("N exceeds MAX_NODES limit.\n", .{});
            continue;
        }

        const graph = try createGraph(allocator, N);
        defer allocator.destroy(graph);
        defer graph.deinit(allocator);

        // Build a connected graph (e.g., a chain)
        for (0..N - 1) |i| {
            try addEdge(allocator, graph, i, i + 1);
        }

        // Measure start time
        const start = try Instant.now();

        // Perform DFS starting from vertex 0
        dfs(graph, 0);

        // Measure end time
        const end = try Instant.now();

        // Calculate the elapsed time in seconds
        const elapsed: f64 = @floatFromInt(end.since(start));
        const time_spent = elapsed / time.ns_per_s;

        // Print the result
        print("DFS with N = {d}\n", .{N});
        print("Time taken: {d} seconds\n\n", .{time_spent});
    }
}
