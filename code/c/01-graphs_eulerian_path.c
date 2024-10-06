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
  for (int i = 0; i < N; i++) {
    numofadj[i] = 0;
    for (int j = 0; j < N; j++) {
      numofadj[i] += graph[i][j];
    }
  }

  // Find out how many vertex has odd number edges
  int startpoint = 0, numofodd = 0;
  for (int i = N - 1; i >= 0; i--) {
    if (numofadj[i] % 2 == 1) {
      numofodd++;
      startpoint = i;
    }
  }

  // If number of vertex with odd number of edges is greater than two return "No
  // Solution".
  if (numofodd > 2) {
    printf("No Solution\n");
    return;
  }

  // If there is a path find the path
  // Initialize empty stack and path take the starting current as discussed
  int cur = startpoint;

  // Loop will run until there is element in the stack or current edge has some
  // neighbour.
  while (top > 0 || numofadj[cur] > 0) {
    // If current node has not any neighbour add it to path and pop stack set
    // new current to the popped element
    if (numofadj[cur] == 0) {
      path[path_length++] = cur;
      if (top > 0) {
        cur = stack[--top];
      } else {
        break;
      }
    }

    // If the current vertex has at least one neighbour add the current vertex
    // to stack, remove the edge between them and set the current to its
    // neighbour.
    else {
      for (int i = 0; i < N; i++) {
        if (graph[cur][i] == 1) {
          stack[top++] = cur;
          graph[cur][i] = 0;
          graph[i][cur] = 0;
          numofadj[cur]--;
          numofadj[i]--;
          cur = i;
          break;
        }
      }
    }
  }

  // Add the last vertex to the path
  path[path_length++] = cur;

  // Print the path
  for (int i = path_length - 1; i >= 0; i--) {
    printf("%d -> ", path[i]);
  }
  printf("\n");
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
