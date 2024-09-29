#import "@preview/slydst:0.1.0": *
#import "@preview/diagraph:0.2.5": *
#import "@preview/lovelace:0.3.0": *

#set text(lang: "en")

#show: slides.with(
  title: "Graph Theory and Computational Complexity",
  subtitle: none,
  date: none,
  authors: ("Jose Storopoli, PhD",),
  layout: "medium",
  ratio: 4 / 3,
  title-color: orange,
)

#set text(size: 16pt)
#show link: set text(blue)

/*
Level-one headings correspond to new sections.
Level-two headings correspond to new slides.
Blank space can be filled with vertical spaces like #v(1fr).
*/

== License

#align(horizon + center)[#image("images/cc-zero.svg", width: 80%)]

== Links
#align(horizon + center)[
  All links are in #text(blue)[blue].

  Feel free to click on them.
]

== Content

#outline()

= Why study Graph Theory and Computational Complexity?

#align(horizon + center)[#image(
    "images/algorithm_analysis_meme.jpg",
    width: 50%,
  )]

== Computational Theory

#align(horizon)[
  *Computational theory* is a subfield of computer science and mathematics that
  seeks to determine which problems can be computed within a given computational
  model.

  *Computation* can be defined as the calculation of a function through an
  algorithm.
]

== #link("https://en.wikipedia.org/wiki/Alan_Turing")[Turing] vs. #link("https://en.wikipedia.org/wiki/Alonzo_Church")[Church]

#align(horizon + center)[
  #figure(
    grid(
      columns: 2,
      gutter: 2mm,
      image("images/turing.jpg", width: 60%),
      image("images/church.jpg", width: 60%),
    ),
    caption: "Alan Turing and Alonzo Church",
  )<turing-church>
]

#pagebreak()

#align(horizon)[
  - Turing proposed the *Turing machine* as a computational model.
  - Alonzo Church proposed *lambda calculus* as a computational model.
  - Both models are mathematically *equivalent*.
]

== Algorithm

#align(horizon)[
  An *algorithm* is a finite sequence of executable actions aimed at obtaining a
  solution to a particular type of problem.
]

== Graph Theory

#align(horizon)[
  Why study Graphs?

  #v(1em)

  #align(center)[
    _Almost_ everything you do in computing can be modeled as a *graph problem*.
  ]
]

== Computational Complexity

#align(horizon)[
  *Computational complexity* is a field in computer science that studies the
  amount of resources required to solve a computational problem#footnote[
    a decidable problem.
  ].
]

#pagebreak()

#align(horizon)[
  We use the $O$ notation to describe the complexity of an algorithm.
  - $O(1)$ (*constant* complexity):
    - Accessing an array
    - Inserting a node in a linked list
    - Insertion and removal in a queue
  - $O(log n)$ (*logarithmic* complexity):
    - Binary search
    - Insertion and removal in a binary search tree

  #pagebreak()

  - $O(n)$ (*linear* complexity):
    - Traversing an array
    - Traversing a linked list
    - Comparing two strings
  - $O(n log n)$ (*log-linear* complexity):
    - _Quick Sort_ algorithm
    - _Merge Sort_ algorithm

  #pagebreak()

  - $O(n^2)$ (*quadratic* complexity):
    - Traversing a matrix
    - _Bubble Sort_ algorithm
    - _Insertion Sort_ algorithm
  - $O(n^3)$ (*cubic* complexity):
    - Matrix multiplication (naive approach)
  - $O(n!)$ (*factorial* complexity):
    - Traveling salesman problem solution
    - Generating all permutations of a list
]

= Graphs

#align(horizon + center)[#image(
    "images/graph_isomorphism_meme.jpg",
    width: 50%,
  )]

== What are Graphs?

Graphs are mathematical structures that model *relationships between objects*.

#align(horizon + center)[
  #figure(
    raw-render(```dot
    graph G {
      rankdir=LR;
      layout=dot;
      a -- {b, c};
      b -- {c, d};
      c -- e;
      d -- e;
      e -- f;
      {rank=same; a;};
      {rank=same; b; c;};
      {rank=same; d; e;};
      {rank=same; f;};
    }
    ```),
    caption: "Graph",
  ) <graph>
]

== Formally

Graphs are *ordered pairs* $G = (V, E)$ where:

- $V$ is a finite set of *vertices* (also called nodes)
- $E$ is a finite set of *edges* (also called arcs) represented by a pair of
  vertices $(u, v)$

The @graph, for example:

#text(size: 14pt)[
  $ V = \{a, b, c, d, e, f\} $
  $ E = \{(a, b), (a, c), (b, c), (b, d), (c, e), (d, e), (e, f)\} $
]

== Directed Graphs

Graphs can be *directed* or *_non_-directed*.

#align(horizon + center)[
  #figure(
    raw-render(```dot
    digraph G {
      rankdir=LR;
      layout=dot;
      a -> {b, c};
      b -> c;
      c -> e;
      d -> {b, e};
      e -> f;
      {rank=same; a;};
      {rank=same; b; c;};
      {rank=same; d; e;};
      {rank=same; f;};
    }
    ```),
    caption: "Directed Graph",
  ) <directed-graph>
]

== Weighted Graphs

Most graphs are *weighted*, meaning they have values associated with the edges.

#align(horizon + center)[
  #figure(
    raw-render(```dot
    graph G {
      rankdir=LR;
      layout=dot;
      a -- b [label=2];
      a -- c [label=3];
      b -- c [label=1];
      b -- d [label=4];
      c -- e [label=1];
      d -- e [label=2];
      e -- f [label=1];
      {rank=same; a;};
      {rank=same; b; c;};
      {rank=same; d; e;};
      {rank=same; f;};
    }
    ```),
    caption: "Weighted Graph",
  ) <weighted-graph>
]

== Graph Examples

#align(horizon)[
  - Computer networks
  - Social networks
  - City maps
  - Molecular structures
]

== #link("https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg")[The 7 Bridges of Königsberg]

The first practical application of graph theory, solved by Euler in 1736.

#align(center)[
  *Is it possible to cross all bridges without repeating any?*
]

#align(horizon + center)[
  #figure(
    image("images/konigsberg_briges.png", width: 35%),
    caption: "The 7 Bridges of Königsberg",
  ) <konigsberg-brigdes>
]

== #link("https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg")[The 7 Bridges of Königsberg]

#align(horizon + center)[
  #figure(
    raw-render(```dot
    graph G {
      rankdir=LR;
      splines=curved;
      layout=neato;
      a[pos="-1,0!"];
      b[pos="0,1!"];
      c[pos="0,-1!"];
      d[pos="1,0!"];
      a -- {b, c, d};
      b:w -- a;
      c:w -- a;
      b -- d;
      c -- d;
    }
    ```),
    caption: "Graph of the 7 Bridges of Königsberg",
  ) <graph-konigsberg-brigdes>
]

== Solution to the 7 Bridges

#align(horizon)[
  The solution to the Königsberg problem was provided by Euler. The graph requires
  *two conditions* to be solved:

  - The graph must be *fully connected*
  - The graph must have exactly *0 or 2 vertices of odd degree*
]

== #link("https://en.wikipedia.org/wiki/Four_color_theorem")[The Four Color Theorem]

#align(horizon)[
  *No more than four colors are needed to color the regions of any map, so that two
  adjacent regions do not share the same color.*
]

#pagebreak()

#align(horizon + center)[
  #figure(
    image("images/four_color_graph.svg", width: 50%),
    caption: "Abstracting a map with four colors using graphs",
  ) <four-color-map>
]

#text(size: 14pt)[
  The theorem was proven in 1976 by Kenneth Appel and Wolfgang Haken#footnote[
    one of the first theorems proved with the help of computers.
  ].
]

== Graph Applications

- Airline itineraries: Calculate the maximum flow in a directed graph.
- Routing software (GPS): Calculate the shortest path between two points.
- Solving a sudoku: Solve a graph coloring problem.
- Online search algorithms: Determine vertex centralities based on themes.
- Social networks: find the largest friend community.

== Subgraphs

A *subgraph* of a graph $G$ is another graph formed from a *subset of the
vertices and edges of $G$*. The vertex subset must include all the edges'
vertices, but may include additional vertices.

#align(horizon + center)[
  #figure(
    image("images/subgraph.svg", width: 40%),
    caption: "Subgraph",
  ) <subgraph>
]

== Induced Subgraph

An *induced subgraph* is a subgraph that *includes all vertices and edges* whose
endpoints belong to the vertex subset.

#align(horizon + center)[
  #figure(
    image("images/induced_subgraph.svg", width: 50%),
    caption: "Induced Subgraph",
  ) <induced-subgraph>
]

== Isomorphism

An isomorphism of the graphs $G$ and $H$ is a bijection#footnote[
  a function that establishes a one-to-one correspondence between the elements of
  two sets.
]
between the sets of vertices of $G$ and $H$:

$ f: V(G) -> V(H) $

#align(horizon + center)[
  #figure(
    image("images/graph_isomorphism.png", width: 66%),
    caption: "Isomorphic Graphs",
  ) <isomorphic-graphs>
]

== Graph Representation

#align(horizon)[
  There are several ways to represent graphs, the most common are:

  - *Adjacency matrix*
  - *Adjacency list*
]

== Adjacency Matrix

#align(horizon)[
  An *adjacency matrix* is a square matrix $bold(A)$ of size $n times n$:

  $ bold(A)^(n times n) = a_(i j) $

  where $a_(i j)$ is the number of edges between vertices $i$ and $j$.
]

#pagebreak()

#align(horizon + center)[
  #figure(
    grid(
      columns: 2,
      gutter: 2mm,
      text[$
          bold(A) = mat(
        1, 1, 0, 0, 1, 0;1, 0, 1, 0, 1, 0;0, 1, 0, 1, 0, 0;0, 0, 1, 0, 1, 1;1, 1, 0, 1, 0, 0;0, 0, 0, 1, 0, 0;
      )
        $],
      raw-render(
        ```dot
        graph G {
          layout=neato;
          rankdir=LR;
          1 -- {1, 2, 5};
          2 -- {3, 5};
          3 -- {4};
          4 -- {5, 6};
        }
        ```,
        width: 80%,
      ),
    ),
    caption: "Adjacency matrix and Graph",
  ) <adjacency-matrix>
]

#pagebreak()

#align(horizon)[
  Properties of an adjacency matrix#footnote[
    $n$ is the number of vertices in the graph.
  ]:

  - Symmetric for undirected graphs
  - Asymmetric for directed graphs
  - Space cost $O(n^2)$
  - Construction cost $O(n^2)$
  - Edge search cost $O(1)$
]

== Adjacency List

#align(horizon)[
  An *adjacency list* is a list of lists, where each list $L_i$ contains the
  vertices adjacent to vertex $i$.
]

#pagebreak()

#align(horizon + center)[
  #figure(
    grid(
      columns: 2,
      gutter: 2mm,
      table(
        columns: 2,
        table.header([*Vertex*], [*Neighbors*]),
        [1], [1, 2, 5],
        [2], [1, 3, 5],
        [3], [2, 4],
        [4], [3, 5, 6],
        [5], [1, 2, 4],
        [6], [4],
      ),
      raw-render(
        ```dot
        graph G {
          layout=neato;
          rankdir=LR;
          1 -- {1, 2, 5};
          2 -- {3, 5};
          3 -- {4};
          4 -- {5, 6};
        }
        ```,
        width: 80%,
      ),
    ),
    caption: "Adjacency list and Graph",
  ) <adjacency-list>
]

#pagebreak()

#align(horizon)[
  Properties of an adjacency list#footnote[
    $n$ is the number of vertices in the graph and $m$ is the number of edges.
  ]:

  - Space cost $O(n + m)$#footnote[for undirected graphs.]<adjacency-list-cost>
  - Construction cost $O(m)$#footnote(<adjacency-list-cost>)
  - Edge search cost $O(n)$
]

== Practical Section (C or pseudocode)

#align(horizon)[
  - Represent a directed and undirected graph
  - Parse a graph from an adjacency matrix
  - Parse a graph from an adjacency list
]

== Paths and Cycles

#align(horizon)[
  #text(size: 14pt)[
    A *path* is a sequence of vertices such that from each vertex there is an edge
    to the next vertex.

    A path is called *simple* if none of the vertices in the path are repeated. The
    *length* of the path is the number of edges the path uses, counting multiple
    edges more than once.

    The *cost* of a path in a balanced graph is the sum of the costs of the edges
    traversed.

    Two paths are *independent* if they share no vertices, except the first and
    last.
  ]
]

#pagebreak()

#align(horizon + center)[
  #figure(
    raw-render(```dot
    graph G {
      rankdir=LR;
      layout=dot;
      a -- b[color=red];
      a -- c;
      b -- c[color=red];
      a -- d;
      c -- e[color=red];
      d -- e;
      e -- f[color=red];
      {rank=same; a;};
      {rank=same; b; c;};
      {rank=same; d; e;};
      {rank=same; f;};
    }
    ```),
    caption: "Path of Length 4",
  ) <path>
]

#pagebreak()

#align(horizon)[
  A *cycle* is a path in which the *first and last vertices coincide*, but no
  other vertices are *repeated*.
]

#pagebreak()

#align(horizon + center)[
  #figure(
    raw-render(```dot
    graph G {
      rankdir=LR;
      layout=dot;
      a -- b;
      a -- c[color=red];
      b -- c;
      a -- d[color=red];
      c -- e[color=red];
      d -- e[color=red];
      e -- f;
      {rank=same; a;};
      {rank=same; b; c;};
      {rank=same; d; e;};
      {rank=same; f;};
    }
    ```),
    caption: "Cycle of Length 4",
  ) <cycle>
]

== Eulerian Path

#align(horizon)[
  An *Eulerian path* is a path that uses each edge exactly once. If such a path
  exists, the graph is called traversable.

  An *Eulerian cycle* is a cycle that uses each edge exactly once.
]

== Hamiltonian Path

#align(horizon)[
  A *Hamiltonian path* is a path that visits each vertex exactly once.

  A *Hamiltonian cycle*#footnote[
    fun fact: one of the first zero-knowledge proof schemes was based on finding a
    Hamiltonian cycle in a giant graph. For more details, see #link(
      "https://en.wikipedia.org/wiki/Zero-knowledge_proof#Hamiltonian_cycle_for_a_large_graph",
    )[Wikipedia]
    and the #link(
      "https://web.archive.org/web/20230103032937/http://euler.nmt.edu/~brian/students/pope.pdf",
    )[original paper].
  ] is a cycle that visits each vertex exactly once.
]

== #link("https://en.wikipedia.org/wiki/Travelling_salesman_problem")[Traveling Salesman Problem]

The *traveling salesman problem* (TSP) is a problem that tries to determine the
shortest route to visit a series of cities (visiting each city only once),
returning to the starting city.

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      graph G {
        layout=neato;
        1[pos="0,0!"]
        2[pos="2,1!"]
        3[pos="4,0.5!"]
        4[pos="1,-1!"]
        5[pos="3,-1!"]
        1 -- 2[label=2];
        1 -- 4[label=3];
        1 -- 5[label=6];
        2 -- 3[label=4];
        2 -- 4[label=3];
        3 -- 4[label=7];
        3 -- 5[label=3];
        4 -- 5[label=3];
      }
      ```,
      width: 50%,
    ),
    caption: "Traveling Salesman Problem",
  ) <travelling-salesman-problem>
]

#pagebreak()

#align(horizon)[
  Formulating it in graph terms, the TSP is a problem of finding a Hamiltonian
  cycle such that the cost of the cycle is the smallest possible.

  $ C = min_("cycle") sum_(i=1)^n c_(i, i+1) $
]

== Practical Section (C or pseudocode)

#align(horizon)[
  - Find an Eulerian path in C
  - Find a Hamiltonian cycle in C
]

= Trees

#align(horizon + center)[#image("images/trees_meme.jpg", width: 50%)]

== What are Trees?

Trees are *acyclic* and *connected* graphs.

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph G {
        a -> {b, c};
        b -> d;
        c -> {e, f};
      }
      ```,
      width: 50%,
    ),
    caption: "Tree",
  ) <tree>
]

#pagebreak()

#align(horizon)[
  - *Root*: the vertex with no incoming edges. All trees have (only) one root
    vertex.
  - *Leaf*: vertex with no outgoing edges.
  - *Level*: distance from the root.
  - *Height*: maximum level.
  - *Parent*: vertex/vertices with a lower level (closer to the root).
  - *Child*: vertex/vertices with a higher level (further from the root).
  - *Ancestor*: vertex/vertices with a lower level.
  - *Descendant*: vertex/vertices with a higher level.
]

== Subtrees

Subtrees are trees that are subsets of a tree.

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph G {
        a
        b
        c[color=red,fontcolor=red]
        e[color=red,fontcolor=red];
        f[color=red,fontcolor=red];
        a -> {b, c};
        b -> d;
        c -> {e, f}[color=red];
      }
      ```,
      width: 45%,
    ),
    caption: "Subtree",
  ) <subtree>
]

== Tree Types

#align(horizon + center)[
  #figure(
    raw-render(```dot
    digraph G {
      a -> b
      b -> c;
    }
    ```),
    caption: "Path Tree",
  ) <tree-path>
]

#pagebreak()

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      graph G {
        layout=circo;
        a -- {b, c, d, e , f, g};
      }
      ```,
      width: 66%,
    ),
    caption: "Star Tree",
  ) <tree-path>
]

#pagebreak()

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      // taken from https://stackoverflow.com/a/23430742
      digraph G {
        nodesep=0.2;
        ranksep=0.5;

        {node[style=invis,label=""]; cx_d;}

        {node[style=invis, label="", width=.1]; ocx_f; ocx_b;}

        {rank=same; b; f; cx_d}
        {rank=same; a; c; ocx_b}
        {rank=same; e; g; ocx_f}

        d -> b;
        d -> f;
        b -> a;
        b -> c;

        f -> e;
        f -> g;

        {
          edge[style=invis];

          // Distantiate nodes
          d -> cx_d;
          b -> cx_d -> f;

          // Force ordering between children
          f -> ocx_f;
          e -> ocx_f -> g;
          b -> ocx_b;
          a -> ocx_b -> c;
        }
      }
      ```,
      width: 50%,
    ),
    caption: "Binary Tree",
  ) <tree-binary>
]

== Balanced Trees

A tree is *balanced* if the height difference between the left and right
subtrees is at most 1.

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph G {
        a -> {b, c};
        b -> d;
        c -> {e, f};
      }
      ```,
      width: 45%,
    ),
    caption: "Balanced Tree",
  ) <balanced-tree>
]

#pagebreak()

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph G {
        a -> {b, c};
        c -> {d, e};
        d -> f;
      }
      ```,
      width: 40%,
    ),
    caption: "Unbalanced Tree",
  ) <unbalanced-tree>
]

== Practical Section (C or pseudocode)

#align(horizon)[
  - Detect if a graph is a tree (i.e., if it is acyclic and connected)
  - Detect which vertex is the root of a tree
]

= Interlude: Polynomial and Exponential Functions

#align(horizon + center)[#image("images/polynomials_meme.jpg", width: 50%)]

== Polynomial Functions

#align(horizon)[
  A function is polynomial if it can be expressed in the form

  $ O(a_n dot n^k + a_(n-1) dot n^(k-1) + ... + a_1 dot n + a_0) $

  where:

  - $n$ is the input size
  - $k$ is a constant
  - $a_n, a_{n-1}, ..., a_1, a_0$ are constant coefficients
]

== Examples

#align(horizon)[
  - $n$
  - $n^2 + 3n + 2$
  - $4n^4 + n^3 + 2n^2 + 7$
  - $n^(100) + 500n^(50) + 3000$
]

== Big-O Notation

#text(size: 15pt)[
  #align(horizon)[
    Big-O notation is used to describe the complexity of an algorithm.

    For example, in the function $n^3 + n^2 + 5n + 100$, the largest constant $k = 3$ will
    asymptotically#footnote[
      as something approaches infinity, i.e., $lim -> oo$.
    ]
    dominate the computation time, so the complexity is $O(n^3)$.

    Also, in Big-O notation, we disregard constant coefficients. For example, $O(3n^2)$ simplifies
    to $O(n^2)$ and
    $50 O(1)$ simplifies to $O(1)$.
  ]
]

== Types of Polynomial Complexity

#align(horizon)[
  - Constant: $O(1)$
  - Logarithmic: $O(log n)$
  - Linear: $O(n)$
  - Log-linear#footnote[also called linearithmic.]: $O(n log n)$
  - Quadratic: $O(n^2)$
  - Cubic: $O(n^3)$
  - Polynomial: $O(n^k)$
]

== Exponential Functions

#align(horizon)[
  A function is exponential if it can be reduced using Big-O notation to

  $ O(n^m) $

  where $m$ *_is not_* a positive constant.

  For example, $O(2^n)$ is an exponential complexity#footnote[
    note that $n$ is not constant.
  ].
]

== Types of Exponential Complexity

#align(horizon)[
  - Exponential: $2^n$
  - Factorial: $n!$
  - Superexponential: $n^n$
  - Doubly Exponential: $2^(2^n)$
]

= Computational Complexity

#align(horizon + center)[#image("images/big_o_meme.jpg", width: 45%)]

== Definition

#align(horizon)[
  The computational complexity of an algorithm is the *number of computational
  operations (such as arithmetic operations, comparisons,
  and memory accesses) required for its execution*.

  #v(1em)

  This number clearly depends on the size and nature of the inputs.
]

== Bounded Complexity

#align(horizon)[
  If the complexity of an algorithm is bounded by a function $f(n)$, where $f$ is
  a polynomial function of $n$ (input size), then the algorithm is said to have
  *polynomial* complexity.

  #v(1em)

  Algorithms with polynomial complexity belong to the class $cal(P)$
]

== Class $cal(P)$

#align(horizon)[
  A *decision problem* is a problem that has a *yes* or *no* answer. Such a
  problem belongs to the class $cal(P)$ if there exists an algorithm that solves
  any instance of the problem in *polynomial complexity*.
]

== Class $cal(N P)$

#align(horizon)[
  A decision problem belongs to the class $cal(N P)$ if there exists a
  *polynomial-time algorithm that _verifies_ the solution to a problem*.

  #v(1em)

  It is trivial to establish that $cal(P) subset.eq cal(N P)$.
]

== Examples of $cal(P)$ and $cal(N P)$ Problems

#align(horizon)[
  - Class $cal(P)$:
    - Sorting algorithms
    - Search algorithms

  - Class $cal(N P)$:
    - Problem of prime factorization of integers
    - Traveling salesman problem
]

== $cal(P)$ vs $cal(N P)$

#text(size: 9.8pt)[
  #table(
    columns: 3,
    align: left + horizon,
    table.header([], [*$cal(P)$*], [*$cal(N P)$*]),
    [*Solvability*],
    [Solvable efficiently in polynomial time.],
    [Efficient verification, but the solution may not be found efficiently.],

    [*Time Complexity*],
    [Polynomial-time algorithms are known.],
    [Efficient verification algorithms are known, but efficient solution algorithms
      are not guaranteed.],

    [*Nature of Solutions*],
    [Solutions can be found efficiently.],
    [Solutions, once proposed, can be verified efficiently.],

    [*Known Relationship*],
    [$cal(P)$ is a subset of $cal(N P)$.],
    [It is unknown whether $cal(N P)$ is a proper subset of $cal(P)$ or if they are
      equal.],
  )
]

== $cal(P)$ vs $cal(N P)$

#align(horizon + center)[#image("images/p_vs_np.png")]

== $cal(N P)$-complete

#align(horizon)[
  A $cal(N P)$-complete problem is an $cal(N P)$ problem that is *as hard as any
  other problem in $cal(N P)$*. If a $cal(N P)$-complete problem can be solved in
  polynomial time, then all problems in $cal(N P)$-complete can also be solved in
  polynomial time.
]

== Boolean Satisfiability (SAT)

#align(horizon)[
  The Boolean satisfiability problem (SAT) seeks to determine whether a
  *propositional formula can be made true* by means of an appropriate assignment
  ("solution") of truth values to its variables.

  $ (a and b and c) or (d and e and f) or (g and h and i) or (j and k and l) $

  where $a, b, c, d, e, f, g, h, i, j, k, l$ are boolean variables, and $and$ (`AND`)
  and $or$ (`OR`) are boolean operators.

  #pagebreak()

  #v(1em)

  Although it is easy to verify whether a given assignment makes the formula true,
  there is no known faster method for finding a satisfying assignment other than
  testing all possible assignments.

  #v(1em)

  #link("https://en.wikipedia.org/wiki/Cook%E2%80%93Levin_theorem")[Cook and Levin proved]
  that every problem that can be easily verified can be solved as quickly as SAT,
  which is why it is NP-complete.
]

== $cal(N P)$-hard

#align(horizon)[
  An $cal(N P)$-hard problem is one for which *no efficient algorithm is known to
  solve it*. However, if an efficient algorithm for an $cal(N P)$-hard problem is
  found, then all problems in $cal(N P)$ can be solved efficiently.
]

== $cal(P)$ vs $cal(N P)$-complete and $cal(N P)$-hard

#align(horizon + center)[#image("images/P_np_np-complete_np-hard.svg")]

== $cal(P)$ vs $cal(N P)$-complete and $cal(N P)$-hard

#align(horizon)[
  - $cal(N P)$-complete:
    - Traveling Salesman Problem in decision form: "Is there a cycle with a cost less
      than or equal to X?"

  - $cal(N P)$-hard:
    - Traveling Salesman Problem in optimization form: "What is the minimum-cost
      cycle?"
]

== Practical Section (C)

#text(size: 14pt)[
  #align(horizon)[
    #link("https://en.wikipedia.org/wiki/Knapsack_problem")[*Knapsack Problem*]

    You are an adventurer and have found a cave full of treasures. However, your
    backpack has limited capacity and you need to decide which items to take to
    maximize the total value, without exceeding the capacity of the backpack.

    You have a list of `n` items, where each item `i` has:

    - *Value*: $v[i]$ (in gold)
    - *Weight*: $w[i]$ (in kilograms)

    Your backpack's capacity is $W$ (in kilograms).

    #pagebreak()

    - Write an algorithm that determines the subset of items that maximizes the total
      value in the backpack without exceeding the total weight $W$.

    - Write an algorithm that, given a certain input of items and capacity, determines
      whether it is possible to fit all items in the backpack.
  ]
]

= Identifying Algorithm Complexity

#align(horizon + center)[#image(
    "images/recursion_joker_debugging_meme.jpg",
    width: 80%,
  )]

== Introduction

#align(horizon)[
  Complexity analysis is essential for evaluating the *efficiency of algorithms*.
  It helps us predict the behavior of an algorithm as the input size increases,
  which is crucial for *optimization* and choosing the *right algorithm* for a
  specific application.
]

== Big-O Notation

#align(horizon)[
  Big-O notation ($O$) is used to describe the *worst-case runtime of an
  algorithm* in terms of the *input size $n$*.
]

== Steps to Determine Complexity

#align(horizon)[
  1. *Identify the dominant operations*: Focus on operations that are executed
    repeatedly, such as loops, recursions, and function calls.

  #pagebreak()

  2. *Estimate how many times these operations are executed*: Analyze the depth and
    number of iterations of loops and recursions.

  #pagebreak()

  3. *Ignore non-dominant terms and constants*: In Big-O notation, we ignore constant
    multiplicative factors and lower-order terms.

  #pagebreak()

  4. *Choose the appropriate Big-O notation*: Use the result from the previous steps
    to identify the correct Big-O complexity.
]

#pagebreak()

#align(horizon + center)[#image(
    "images/recursion_world_burn_meme.jpeg",
    width: 80%,
  )]

== Control Structures

- *Sequential Structures*: constant complexity $O(1)$
- *Conditional Structures*: constant complexity $O(1)$
- *Loops*: linear complexity $O(n)$
- *Nested Loop*: quadratic complexity $O(n^2)$
- *Recursion*:
  - *Linear*: linear complexity $O(n)$
  - *Divide-and-Conquer*: logarithmic complexity $O(log n)$
  - *Binary*: complexity $O(n log n)$
  - *Exponential*: exponential complexity $O(2^n)$

== Sequential Structures

#align(horizon)[
  Control structures that do not involve loops or recursion have constant
  complexity $O(1)$.

  ```c
  int x = 5;
  int y = 10;
  int z = x + y;  // O(1)
  ```
]

== Conditional Structures

#align(horizon)[
  Simple conditional structures, such as `if`-`else`, do not affect complexity,
  but the execution of internal blocks should be considered.

  ```c
  if (x > y) {
      z = x - y;  // O(1)
  } else {
      z = y - x;  // O(1)
  }
  // Total complexity: O(1)
  ```
]

== Loops

#align(horizon)[
  The complexity of a loop depends on the number of iterations:

  - *Simple Loop*:

    ```c
    for (int i = 0; i < n; i++) {
        // O(1) operation
    }
    // Total complexity: O(n)
    ```

  #pagebreak()

  - *Nested Loop*:

    ```c
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            // O(1) operation
        }
    }
    // Total complexity: O(n^2)
    ```

  #pagebreak()

  - *Loop with multiplicative increment*:

    ```c
    for (int i = 1; i < n; i *= 2) {
        // O(1) operation
    }
    // Total complexity: O(log n)
    ```
]

== Recursion

#align(horizon + center)[#image(
    "images/recursion_joker_stackoverflow_meme.jpeg",
    width: 80%,
  )]

#pagebreak()

#align(horizon)[
  The complexity of recursive algorithms depends on the number of recursive calls
  and the input size in each call.

  - *Linear Recursion*:

    ```c
    void linear_recursion(int n) {
        if (n == 0) return;
        // O(1) operation
        linear_recursion(n-1);
    }
    // Total complexity: O(n)

    ```

  #pagebreak()

  - *Divide-and-Conquer Recursion*:

    ```c
    void divide_and_conquer_recursion(int n) {
        if (n == 0) return;
        // O(1) operation
        divide_and_conquer_recursion(n/2);
    }
    // Total complexity: O(log n)

    ```

  #pagebreak()

  - *Binary Recursion (like Merge Sort):*:

    ```c
    void merge_sort(int arr[], int n) {
        if (n < 2) return;
        int mid = n / 2;
        merge_sort(arr, mid);
        merge_sort(arr + mid, n - mid);
        merge(arr, mid, n - mid);  // O(n)
    }
    // Total complexity: O(n log n)
    ```

  #pagebreak()

  - *Exponential Recursion*:
    ```c
    int fibonacci(int n) {
        if (n <= 1) return n;
        return fibonacci(n-1) + fibonacci(n-2);
    }
    // Total complexity: O(2^n)
    ```
]

== Practical Example - Linear Search

#align(horizon)[
  ```c
  int linear_search(int arr[], int n, int x) {
      for (int i = 0; i < n; i++) {
          if (arr[i] == x) return i;
      }
      return -1;
  }
  // Total complexity: O(n)
  ```
]

== Practical Example - Binary Search

#align(horizon)[
  ```c
  int binary_search(int arr[], int n, int x) {
      int start = 0, end = n - 1;
      while (start <= end) {
          int mid = (start + end) / 2;
          if (arr[mid] == x) return mid;
          if (arr[mid] < x) start = mid + 1;
          else end = mid - 1;
      }
      return -1;
  }
  // Total complexity: O(log n)
  ```
]

== Practical Example - Bubble Sort

#text(size: 14pt)[
  #align(horizon)[
    ```c
    void bubble_sort(int arr[], int n) {
        for (int i = 0; i < n-1; i++) {
            for (int j = 0; j < n-i-1; j++) {
                if (arr[j] > arr[j+1]) {
                    // swap elements
                    int temp = arr[j];
                    arr[j] = arr[j+1];
                    arr[j+1] = temp;
                }
            }
        }
    }
    // Total complexity: O(n^2)
    ```
  ]
]

== Practical Section (C or pseudocode)

#align(horizon)[
  - Implement and determine the complexity of an algorithm that counts the number of
    occurrences of an element in a matrix.

  - Find a way to reduce the complexity of calculating Fibonacci.
]

= Interlude: Analyzing Algorithm Complexity using C Code

#align(horizon + center)[#image("images/programming_meme.jpg", width: 50%)]

#pagebreak()

#align(horizon)[
  #text(size: 12pt)[

    Reversing an array:
    // Total complexity: O(n)
    ```c
    void reverse_array(int arr[], int n) {
        int start = 0, end = n - 1;
        while (start < end) {
            int temp = arr[start];
            arr[start] = arr[end];
            arr[end] = temp;
            start++;
            end--;
        }
    }
    ```

    #pagebreak()

    Checking if a string is a
    #link("https://en.wikipedia.org/wiki/Palindrome")[palindrome]:
    // Total complexity: O(n)
    ```c
    bool is_palindrome(char str[]) {
        int start = 0;
        // strings in C are null-terminated
        int end = strlen(str) - 1;
        while (start < end) {
            if (str[start] != str[end]) {
                return false;
            }
            start++;
            end--;
        }
        return true;
    }
    ```

    #pagebreak()

    Find the maximum difference between two elements in an array where the larger
    element comes after the smaller one:
    // Total complexity: O(n)
    #text(size: 11pt)[
      ```c
      int max_difference(int arr[], int n) {
          int min_element = arr[0];
          int max_diff = arr[1] - arr[0];

          for (int i = 1; i < n; i++) {
              if (arr[i] - min_element > max_diff) {
                  max_diff = arr[i] - min_element;
              }
              if (arr[i] < min_element) {
                  min_element = arr[i];
              }
          }

          return max_diff;
      }
      ```
    ]

    #pagebreak()

    Sorting an array using the
    #link("https://en.wikipedia.org/wiki/Insertion_sort")[insertion sort]
    algorithm:
    // Total complexity: O(n^2)
    ```c
    void insertion_sort(int arr[], int n) {
        for (int i = 1; i < n; i++) {
            int key = arr[i];
            int j = i - 1;
            while (j >= 0 && arr[j] > key) {
                arr[j + 1] = arr[j];
                j = j - 1;
            }
            arr[j + 1] = key;
        }
    }
    ```

    #pagebreak()

    Find the duplicates in an array:
    // Total complexity: O(n^2)
    ```c
    void find_duplicates(int arr[], int n) {
        for (int i = 0; i < n - 1; i++) {
            for (int j = i + 1; j < n; j++) {
                if (arr[i] == arr[j]) {
                    printf("Duplicate found: %d\n", arr[i]);
                }
            }
        }
    }
    ```

    #pagebreak()

    Compute the power of a number:
    // Total complexity: O(log n)
    ```c
    int power(int x, int n) {
        if (n == 0) {
            return 1;
        }
        int half = power(x, n / 2);
        if (n % 2 == 0) {
            return half * half;
        } else {
            return x * half * half;
        }
    }
    ```

    #pagebreak()

    Find the
    #link("https://en.wikipedia.org/wiki/Greatest_common_divisor")[greatest common divisor]
    of two numbers:
    // Total complexity: O(log(min(a, b)))
    ```c
    int gcd(int a, int b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }
    ```

    #pagebreak()

    #link("https://en.wikipedia.org/wiki/Primality_test")[Prime number check]
    (naive method):
    // Total complexity: O(sqrt(n))
    ```c
    bool is_prime(int n) {
        if (n <= 1) {
            return false;
        }
        for (int i = 2; i * i <= n; i++) {
            if (n % i == 0) {
                return false;
            }
        }
        return true;
    }
    ```

    #pagebreak()

    Find the majority element (an element that appears more than $n/2$ times) using
    #link(
  "https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm",
)[Boyer-Moore's Voting Algorithm].
    // Total complexity: O(n)
    #text(size: 6pt)[
      ```c
      int find_majority_element(int arr[], int n) {
          int count = 0, candidate = -1;
          // Find potential candidate
          for (int i = 0; i < n; i++) {
              if (count == 0) {
                  candidate = arr[i];
                  count = 1;
              } else if (arr[i] == candidate) {
                  count++;
              } else {
                  count--;
              }
          }
          // Verify if the candidate is the majority element
          count = 0;
          for (int i = 0; i < n; i++) {
              if (arr[i] == candidate) {
                  count++;
              }
          }
          if (count > n / 2) {
              return candidate;
          } else {
              return -1;  // No majority element
          }
      }
      ```
    ]

    #pagebreak()

    Generate
    #link("https://en.wikipedia.org/wiki/Pascal%27s_triangle")[Pascal's Triangle]:
    // Total complexity: O(n^2)
    #text(size: 11pt)[
      ```c
      void generate_pascals_triangle(int n) {
          int arr[n][n];

          for (int line = 0; line < n; line++) {
              for (int i = 0; i <= line; i++) {
                  if (i == 0 || i == line) {
                      arr[line][i] = 1;
                  } else {
                      arr[line][i] = arr[line - 1][i - 1] + arr[line - 1][i];
                  }
                  printf("%d ", arr[line][i]);
              }
              printf("\n");
          }
      }
      ```
    ]

    #pagebreak()

    #link("https://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane's_algorithm")[Kadane's algorithm]
    to find the maximum subarray sum:
    // Total complexity: O(n)
    #text(size: 11pt)[
      ```c
      int max_subarray_sum(int arr[], int n) {
          int max_ending_here = 0;
          int max_so_far = INT_MIN;

          for (int i = 0; i < n; i++) {
              max_ending_here = max_ending_here + arr[i];
              if (max_so_far < max_ending_here) {
                  max_so_far = max_ending_here;
              }
              if (max_ending_here < 0) {
                  max_ending_here = 0;
              }
          }

          return max_so_far;
      }
      ```
    ]

    #pagebreak()

    #link(
  "https://en.wikipedia.org/wiki/Longest_common_subsequence",
)[Longest Common Subsequence (LCS)]:
    // Total complexity: O(n * m)
    #text(size: 10pt)[
      ```c
      int lcs(char *X, char *Y, int m, int n) {
          int dp[m + 1][n + 1];

          for (int i = 0; i <= m; i++) {
              for (int j = 0; j <= n; j++) {
                  if (i == 0 || j == 0) {
                      dp[i][j] = 0;
                  } else if (X[i - 1] == Y[j - 1]) {
                      dp[i][j] = dp[i - 1][j - 1] + 1;
                  } else {
                      dp[i][j] = max(dp[i - 1][j], dp[i][j - 1]);
                  }
              }
          }

          return dp[m][n];
      }
      ```
    ]

    #pagebreak()

    Merge two sorted arrays:
    // Total complexity: O(N log N)
    #text(size: 8pt)[
      ```c
      void merge_sorted_arrays(int A[], int B[], int m, int n, int C[]) {
          int i = 0, j = 0, k = 0;
          int N = m + n;  // N is the combined length of A and B

          while (i < m && j < n) {
              if (A[i] <= B[j]) {
                  C[k++] = A[i++];
              } else {
                  C[k++] = B[j++];
              }
          }

          // Copy the remaining elements of A, if any
          while (i < m) {
              C[k++] = A[i++];
          }

          // Copy the remaining elements of B, if any
          while (j < n) {
              C[k++] = B[j++];
          }
      }
      ```
    ]

    #pagebreak()

    This algorithm is a
    #link(
  "https://en.wikipedia.org/wiki/Fast_inverse_square_root",
)[fast way to compute the inverse square root],
    $1 / sqrt(x)$, made famous by its use in the game Quake III Arena by
    #link("https://en.wikipedia.org/wiki/John_Carmack")[John Carmack]. The method
    uses a clever approximation and a single iteration of #link("https://en.wikipedia.org/wiki/Newton%27s_method")[Newton's method] to
    refine it.
    // Total complexity: O(1)
    #text(size: 8pt)[
      ```c
      float Q_rsqrt(float number) {
          long i;
          float x2, y;
          const float threehalfs = 1.5F;

          x2 = number * 0.5F;
          y = number;
          i = *(long*)&y;                       // Evil bit-level hacking
          i = 0x5f3759df - (i >> 1);            // What the fuck?
          y = *(float*)&i;
          y = y * (threehalfs - (x2 * y * y));  // 1st iteration of Newton's method
          // y  = y * ( threehalfs - ( x2 * y * y ) );   // 2nd iteration, this can be removed

          return y;
      }
      ```
    ]
  ]
]

= Search Algorithms

#align(horizon + center)[#image(
    "images/search_algorithms_meme.png",
    width: 100%,
  )]

== What is a Search Algorithm?

#align(horizon)[
  A *search algorithm* is a sequence of instructions that allows finding a specific element within a data structure. It is fundamental in computer science as it optimizes data access and manipulation.
]

== Why Study Search Algorithms?

#align(horizon)[
  - *Efficiency*: Efficient search algorithms save time and resources.
  - *Foundations*: They are the basis for more complex algorithms and data structures.
  - *Practical Applications*: Used in databases, operating systems, artificial intelligence, among others.
]

== Types of Search Algorithms

#align(horizon)[
  - *Linear Search*
  - *Binary Search*
  - *Graph Search Algorithms*:
    - *Breadth-First Search (BFS)*
    - *Depth-First Search (DFS)*
]

== Linear Search

=== Concept

#align(horizon)[
  *Linear search* is the simplest search algorithm. It sequentially checks each element of the data structure until it finds the desired element or reaches the end of the structure.
]

#pagebreak()

=== Characteristics of Linear Search

#align(horizon)[
  - *Simple to Implement*
  - *Does Not Require Ordered Structure*
  - *Time Complexity*: $O(n)$, where $n$ is the number of elements.
]

#pagebreak()

=== Pseudoalgorithm

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Linear Search],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[given a list $A$ of $n$ elements with values $A_0, dots A_(n-1)$, and target value $T$:],
      )[
        + *for* $i$ *in* $A$:
          + *if* $A_i = T$:
            + *return* $i$
        + *return* _null_
      ]
    ],
  ) <linear-search>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 14pt)[
    ```c
    int linear_search(int arr[], int n, int x) {
        for (int i = 0; i < n; i++) {
            if (arr[i] == x)
                // Element found at position i
                return i;
        }
        return -1;  // Element not found
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Best Case*: The element is at the first position; $O(1)$.
  - *Worst Case*: The element is at the last position or not present; $O(n)$.
  - *Average Case*: On average, it checks half of the elements; $1/2 O(n) = O(n)$.
]

== Binary Search

#align(horizon)[
  *Binary search* is an efficient algorithm to find an element in an ordered list, reducing the search space by half with each iteration.
]

#pagebreak()

=== Characteristics of Binary Search

#align(horizon)[
  - *Requires Ordered Structure*
  - *Time Complexity*: $O(log n)$
  - *More Efficient than Linear Search in Large Data Sets*
]

#pagebreak()

=== Pseudoalgorithm

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Binary Search],
    text(size: 9pt)[
      #pseudocode-list(
        title: smallcaps[given an _ordered_ list $A$ of $n$ elements with values $A_0, dots A_(n-1)$, and target value $T$:],
      )[
        + $L := 0$; $R := n-1$.
        + *while* $L <= R$:
          + $m := floor((L+R) / 2)$
          + *if* $A_m < T$:
            + $L := m+1$
          + *else if* $A_m > T$:
            + $R := m-1$
          + *else*:
            + *return* $m$
        + *return* _null_
      ]
    ],
  ) <binary-search>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 14pt)[
    ```c
    int binary_search(int arr[], int n, int x) {
        int start = 0, end = n - 1;
        while (start <= end) {
            int mid = start + (end - start) / 2;
            if (arr[mid] == x)
                return mid;  // Element found
            if (arr[mid] < x)
                start = mid + 1;
            else
                end = mid - 1;
        }
        return -1;  // Element not found
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - With each iteration, the algorithm halves the search space.
  - *Time Complexity*: $O(log n)$
  - *Efficiency*: Much faster than linear search in large data sets.
]

== Graph Search Algorithms

=== Types of Graph Search Algorithms

#align(horizon)[
  - *Breadth-First Search (BFS)*
  - *Depth-First Search (DFS)*
]

#pagebreak()

=== Applications

#align(horizon)[
  - *Finding Paths*: Between two vertices in a graph.
  - *Checking Connectivity*: Whether all vertices are reachable.
  - *Cycle Detection*: In directed and undirected graphs.
]

== Breadth-First Search (BFS)

=== Concept

#align(horizon)[
  *Breadth-first search* explores a graph by visiting all vertices at the same distance layer from the origin before moving to the next layer.
]

#pagebreak()

=== Characteristics of BFS

#align(horizon)[
  - *Uses a Queue*
  - *Guarantees the Shortest Path in Unweighted Graphs*
  - *Time Complexity*: $O(V + E)$, where $V$ is the number of vertices and $E$ is the number of edges.
]

#pagebreak()

=== Pseudoalgorithm

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Breadth-first Search],
    text(size: 8pt)[
      #pseudocode-list(
        title: smallcaps[given a graph $G$, a _root_ vertex, and target value $T$:],
      )[
        + $Q =: "queue"$
        + _root_.explored $= "true"$
        + $Q$.enqueue(_root_)
        + *while* $!Q$.empty():
          + $v := Q$.dequeue()
          + *if* $v = T$
            + *return* $v$
        + *for* all edges from $v$ to $w$ *in* $G$.adjacentEdges(v):
          + *if* $!w$.explored:
            + $w$.explored
            + $w$.parent $:= v$
            + $Q$.enqueue($w$)
        + *return* _null_
      ]
    ],
  ) <breadth-first-search>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 9pt)[
    ```c
    void bfs(int graph[][MAX], int start, int n) {
        int visited[MAX] = {0};
        int queue[MAX], front = 0, rear = 0;

        visited[start] = 1;
        queue[rear++] = start;

        while (front < rear) {
            int current = queue[front++];
            printf("%d ", current);

            for (int i = 0; i < n; i++) {
                if (graph[current][i] && !visited[i]) {
                    visited[i] = 1;
                    queue[rear++] = i;
                }
            }
        }
    }
    ```
  ]
]

#pagebreak()

=== Illustration of BFS

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph BFS {
        rankdir=TB;
        node [shape=circle, style=filled, color=lightgrey];

        // Nodes with numbered labels
        A [label="A\n(1)"];
        B [label="B\n(2)"];
        C [label="C\n(3)"];
        D [label="D\n(4)"];
        E [label="E\n(5)"];
        F [label="F\n(6)"];
        G [label="G\n(7)"];
        H [label="H\n(8)"];

        // Edges
        A -> B;
        A -> C;
        B -> D;
        B -> E;
        C -> F;
        C -> G;
        E -> H;
      }
      ```,
      width: 37%,
    ),
    caption: "Illustration of BFS with vertices numbered by visitation order",
  )
]

== Depth-First Search (DFS)

=== Concept

#align(horizon)[
  *Depth-first search* explores as far as possible along each branch before backtracking.
]

#pagebreak()

=== Characteristics of DFS

#align(horizon)[
  - *Uses a Stack* (can be implemented recursively)
  - *Does Not Guarantee the Shortest Path*
  - *Time Complexity*: $O(V + E)$
]

#pagebreak()

=== Pseudoalgorithm

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Depth-first Search],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[given a graph $G$, a vertex $v$, and target value $T$:],
      )[
        + $v$.discovered
        + *if* $v = T$
          + *return* $v$
        + *for* all edges from $v$ to $w$ *in* $G$.adjacentEdges(v):
          + *if* $!w$.discovered:
            + DFS($G$, $w$)
        + *return* _null_
      ]
    ],
  ) <depth-first-search>
]

#pagebreak()

=== Example in C (Recursive)

#align(horizon)[
  #text(size: 15pt)[
    ```c
    void dfs(int graph[][MAX], int current, int visited[], int n) {
        visited[current] = 1;
        printf("%d ", current);

        for (int i = 0; i < n; i++) {
            if (graph[current][i] && !visited[i]) {
                dfs(graph, i, visited, n);
            }
        }
    }
    ```
  ]
]

#pagebreak()

=== Illustration of DFS

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph DFS {
        rankdir=TB;
        node [shape=circle, style=filled, color=lightgrey];

        // Nodes with numbered labels
        A [label="A\n(1)"];
        B [label="B\n(2)"];
        D [label="D\n(3)"];
        E [label="E\n(4)"];
        H [label="H\n(5)"];
        C [label="C\n(6)"];
        F [label="F\n(7)"];
        G [label="G\n(8)"];

        // Edges
        A -> B;
        A -> C;
        B -> D;
        B -> E;
        E -> H;
        C -> F;
        C -> G;
      }
      ```,
      width: 37%,
    ),
    caption: "Illustration of DFS with vertices numbered by visitation order",
  )
]

== Comparison between BFS and DFS

#align(horizon)[
  #text(size: 12pt)[
    #table(
      columns: 3,
      align: left + horizon,
      table.header([*Characteristic*], [*BFS*], [*DFS*]),
      [*Data Structure*], [Queue], [Stack],
      [*Memory Usage*],
      [Higher (stores all neighbors)],
      [Lower (stores only current path)],

      [*Shortest Path*], [Yes (in unweighted graphs)], [Not necessarily],
      [*Completeness*], [Yes], [Yes],
      [*Applications*],
      [Shortest path, node levels],
      [Cycle detection, topological sorting],
    )
  ]
]
