#include <stdio.h>

/** Number of vertices in the graph */
#define N 5
/** Maximum length of the path */
#define MAX_PATH_LENGTH 20

/** Find Eulerian Path in a graph
 * @param graph Adjacency matrix of the graph
 */
void find_path(int graph[N][N]) {
  int numofadj[N];
  int stack[N];
  int path[MAX_PATH_LENGTH];
  int top = 0;
  int path_length = 0;

  // Find out number of edges each vertex has
  // FIXME: Add the code here

  // Find out how many vertex has odd number edges
  // FIXME: Add the code here

  // If number of vertex with odd number of edges is greater than two return "No
  // Solution".
  // FIXME: Add the code here

  // If there is a path find the path
  // Initialize empty stack and path take the starting current as discussed
  // FIXME: Add the code here

  // Loop will run until there is element in the stack or current edge has some
  // neighbour.
  // FIXME: Add the code here

  // If the current vertex has at least one neighbour add the current vertex
  // to stack, remove the edge between them and set the current to its
  // neighbour.
  // FIXME: Add the code here

  // Add the last vertex to the path
  // FIXME: Add the code here

  // Print the path
  // FIXME: Add the code here
}

/** Main function */
int main() {
  /* Test case 1

       0 --- 1
       |    | \
       |    2--3
       4

  */
  int graph1[N][N] = {{0, 1, 0, 0, 1},
                      {1, 0, 1, 1, 0},
                      {0, 1, 0, 1, 0},
                      {0, 1, 1, 0, 0},
                      {1, 0, 0, 0, 0}};

  printf("Test Case 1:\n");
  find_path(graph1);

  /* Test case 2

     0 -- 1 -- 2
    /|  / \  | \
   3--4      5

  */
  int graph2[N][N] = {{0, 1, 0, 1, 1},
                      {1, 0, 1, 0, 1},
                      {0, 1, 0, 1, 1},
                      {1, 1, 1, 0, 0},
                      {1, 0, 1, 0, 0}};

  printf("Test Case 2:\n");
  find_path(graph2);

  /* Test case 3

     0 --- 1
    /|\     |\
   2  4---5  3

  */
  int graph3[N][N] = {{0, 1, 0, 0, 1},
                      {1, 0, 1, 1, 1},
                      {0, 1, 0, 1, 0},
                      {0, 1, 1, 0, 1},
                      {1, 1, 0, 1, 0}};

  printf("Test Case 3:\n");
  find_path(graph3);

  return 0;
}
