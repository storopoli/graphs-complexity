#import "@preview/slydst:0.1.2": *
#import "@preview/diagraph:0.3.0": *
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

==

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
      image("images/turing.jpg", width: 60%), image("images/church.jpg", width: 60%),
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

==

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

==

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

==

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

==

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

==

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

==

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

==

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

#pagebreak()

=== What if we $k > 1$ pivots?

#align(horizon)[
  #text(size: 14pt)[
    At each step:

    - Divides the array into $k+1$ partitions
    - Search space reduces to $n / (k+1)$
    - Comparisons increase from $1$ to $k$

    Total comparisons:

    - Total steps: $log_(k+1) n$
    - Total comparisons: $k dot.c log_(k+1) n$

    Complexity:

    - $O (k dot.c (log n) / (log(k+1))) = O(log n)$
  ]
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
        + $Q := "queue"$
        + _root_.explored $:= "true"$
        + $Q$.enqueue(_root_)
        + *while* $!Q$.empty():
          + $v := Q$.dequeue()
          + *if* $v = T$
            + *return* $v$
        + *for* all edges from $v$ to $w$ *in* $G$.adjacentEdges(v):
          + *if* $!w$.explored:
            + $w$.explored $:= "true"$
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
  #text(size: 7pt)[
    ```c
    int bfs(int graph[][MAX], int start, int n, int target) {
        if (start->value == T)
          return start->id;
        int visited[MAX] = {0};
        int queue[MAX], front = 0, rear = 0;

        visited[start] = 1;
        queue[rear++] = start;

        while (front < rear) {
            int current = queue[front++];

            for (int i = 0; i < n; i++) {
                if (current->value == T)
                  return current->id;
                if (graph[current][i] && !visited[i]) {
                    visited[i] = 1;
                    queue[rear++] = i;
                }
            }
        }
        return -1;
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
        + $v$.discovered $:= "true"$
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
  #text(size: 14pt)[
    ```c
    int dfs(int graph[][MAX], int current, int visited[], int n) {
        if (current->value == T)
          return current->id;
        visited[current] = 1;

        for (int i = 0; i < n; i++) {
            if (graph[current][i] && !visited[i]) {
                dfs(graph, i, visited, n);
            }
        }
        return -1;
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
      [*Memory Usage*], [Higher (stores all neighbors)], [Lower (stores only current path)],
      [*Shortest Path*], [Yes (in unweighted graphs)], [Not necessarily],
      [*Completeness*], [Yes], [Yes],
      [*Applications*], [Shortest path, node levels], [Cycle detection, topological sorting],
    )
  ]
]

= Sorting Algorithms

==

#align(horizon + center)[
  #image(
    "images/sorting_algorithms_meme.jpg",
    width: 50%,
  )
]

== Introduction

#align(horizon)[
  *Sorting algorithms* are algorithms that put elements of a list in a certain order.
  The most frequently used orders are numerical order and lexicographical order.

  Sorting is important because it:

  - Organizes data to make it more usable.
  - Optimizes the efficiency of other algorithms that require sorted data.
  - Facilitates searching and data representation.
]

== Types of Sorting Algorithms

#align(horizon)[
  #text(size: 14pt)[
    Sorting algorithms can be classified based on several factors:

    - *Comparison vs Non-comparison based*: Whether elements are compared
      to determine their order.
    - *Stable vs Unstable*: Whether equivalent elements maintain their original
      relative order.
    - *Time Complexity*: How the runtime increases with the number of elements.
    - *Space Complexity*: The amount of memory required beyond the input data.
  ]
]

== Stable vs Non-Stable

#align(horizon + center)[
  #image(
    "images/sort_stable_vs_unstable.png",
    width: 100%,
  )
]

== Common Sorting Algorithms

#align(horizon)[
  - *Bubble Sort*
  - *Selection Sort*
  - *Insertion Sort*
  - *Merge Sort*
  - *Quick Sort*
  - *Heap Sort*
  - *Counting Sort*
  - *Radix Sort*
  - *Bucket Sort*
]

== Bubble Sort

#align(horizon)[
  *Bubble Sort* is a simple comparison-based algorithm where
  each pair of adjacent elements is compared,
  and the elements are swapped if they are in the wrong order.
  This process is repeated until no swaps are needed.

  It is called "bubble" sort because smaller elements "bubble" to
  the top of the list.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. Compare each pair of adjacent items.
  2. Swap them if they are in the wrong order.
  3. Repeat steps 1 and 2 for all elements.
  4. Continue the process until a pass completes with no swaps.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Bubble Sort],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Given an array $A$ of $n$ elements:],
      )[
        + *for* $i$ from $0$ to $n - 1$:
          + *for* $j$ from $0$ to $n - i - 1$:
            + *if* $A[j] > A[j + 1]$:
              + swap $A[j]$ and $A[j + 1]$
      ]
    ],
  ) <bubble-sort>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 14pt)[
    ```c
    void bubble_sort(int arr[], int n) {
        for (int i = 0; i < n - 1; i++) {
            // Last i elements are already in place
            for (int j = 0; j < n - i - 1; j++) {
                if (arr[j] > arr[j + 1]) {
                    // Swap arr[j] and arr[j + 1]
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                }
            }
        }
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*:
    - Best Case: $O(n)$ (when the array is already sorted)
    - Average Case: $O(n^2)$
    - Worst Case: $O(n^2)$
  - *Space Complexity*: $O(1)$ (in-place sorting)
  - *Stability*: Stable (equal elements maintain their relative order)
]

== Selection Sort

#align(horizon)[
  *Selection Sort* divides the input list into two parts:
  a sorted sublist of items built up from left to right,
  and a sublist of the remaining unsorted items.
  It repeatedly selects the smallest (or largest) element from the unsorted sublist,
  swapping it with the leftmost unsorted element.

  The process continues moving the sublist boundaries one element to the right.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. Set the first unsorted element as the minimum.
  2. Compare this minimum with the next element.
  3. If the next element is smaller, set it as the new minimum.
  4. Continue until the end of the array.
  5. Swap the minimum with the first unsorted position.
  6. Move the boundary one element to the right.
  7. Repeat until the array is sorted.
]

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Selection Sort],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Given an array $A$ of $n$ elements:],
      )[
        + *for* $i$ from $0$ to $n - 1$:
          + $"minIdx" := i$
          + *for* $j$ from $i + 1$ to $n$:
            + *if* $A[j] < A["minIdx"]$:
              + minIdx := $j$
          + swap $A[i]$ and $A["minIdx"]$
      ]
    ],
  ) <selection-sort>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 13pt)[
    ```c
    void selection_sort(int arr[], int n) {
        for (int i = 0; i < n - 1; i++) {
            int min_idx = i;
            for (int j = i + 1; j < n; j++) {
                if (arr[j] < arr[min_idx])
                    min_idx = j;
            }
            // Swap the found minimum element with
            // the first element
            int temp = arr[min_idx];
            arr[min_idx] = arr[i];
            arr[i] = temp;
        }
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*:
    - Best Case: $O(n^2)$
    - Average Case: $O(n^2)$
    - Worst Case: $O(n^2)$
  - *Space Complexity*: $O(1)$
  - *Stability*: Unstable (equal elements may not maintain their relative order)
]

== Insertion Sort

#align(horizon)[
  *Insertion Sort* builds the final sorted array one item at a time.
  It assumes that the first element is already sorted,
  then inserts each subsequent element into the correct position relative to
  the sorted portion.

  It's similar to how people arrange a hand of playing cards.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. Start from the second element (index $1$).
  2. Compare the current element with elements in the sorted portion.
  3. Shift all larger elements in the sorted portion one position to the right.
  4. Insert the current element into its correct position.
  5. Repeat until the array is sorted.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Insertion Sort],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Given an array $A$ of $n$ elements:],
      )[
        + *for* $i$ from $1$ to $n - 1$:
          + $"key" := A[i]$
          + $j := i - 1$
          + *while* $j >= 0$ *and* $A[j] > "key"$:
            + $A[j + 1] := A[j]$
            + $j := j - 1$
          + $A[j + 1] := "key"$
      ]
    ],
  ) <insertion-sort>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 11pt)[
    ```c
    void insertion_sort(int arr[], int n) {
        for (int i = 1; i < n; i++) {
            int key = arr[i];
            int j = i - 1;

            // Move elements of arr[0..i-1],
            // that are greater than key,
            // to one position ahead of
            // their current position
            while (j >= 0 && arr[j] > key) {
                arr[j + 1] = arr[j];
                j = j - 1;
            }
            arr[j + 1] = key;
        }
    }
    ```
  ]
]

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*:
    - Best Case: $O(n)$ (when the array is already sorted)
    - Average Case: $O(n^2)$
    - Worst Case: $O(n^2)$
  - *Space Complexity*: $O(1)$
  - *Stability*: Stable
]

== Merge Sort

#align(horizon)[
  *Merge Sort* is a divide and conquer algorithm that divides the array into halves,
  sorts each half, and then merges them back together.

  It is efficient and has a guaranteed runtime of $O(n log n)$.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. If the array is of length $0$ or $1$, it is already sorted.
  2. Divide the array into two halves.
  3. Recursively sort each half.
  4. Merge the two sorted halves into one sorted array.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Merge Sort],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Function merge_sort(array A, left, right):],
      )[
        + *if* $"left" < "right"$:
          + $"mid" := ("left" + "right") / 2$
          + $"merge_sort"(A, "left", "mid")$
          + $"merge_sort"(A, "mid" + 1, "right")$
          + $"merge"(A, "left", "mid", "right")$
      ]
    ],
  ) <merge-sort>
]

#pagebreak()

=== `merge` Function

#align(horizon)[
  The `merge` function combines two sorted subarrays into one sorted array.

  - Left subarray: $A["left".."mid"]$
  - Right subarray: $A["mid"+1.."right"]$
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 6pt)[
    ```c
    void merge(int arr[], int l, int m, int r) {
        int n1 = m - l + 1;
        int n2 = r - m;
        // Create temp arrays
        int L[n1], R[n2];
        // Copy data to temp arrays L[] and R[]
        for (int i = 0; i < n1; i++)
            L[i] = arr[l + i];
        for (int j = 0; j < n2; j++)
            R[j] = arr[m + 1 + j];
        // Merge the temp arrays back into arr[l..r]
        int i = 0, j = 0, k = l;
        while (i < n1 && j < n2) {
            if (L[i] <= R[j]) {
                arr[k++] = L[i++];
            } else {
                arr[k++] = R[j++];
            }
        }
        // Copy the remaining elements of L[], if any
        while (i < n1)
            arr[k++] = L[i++];
        // Copy the remaining elements of R[], if any
        while (j < n2)
            arr[k++] = R[j++];
    }
    ```
  ]
]

#pagebreak()

#align(horizon)[
  #text(size: 12pt)[
    ```c
    void merge_sort(int arr[], int l, int r) {
        if (l < r) {
            int m = l + (r - l) / 2;
            merge_sort(arr, l, m);
            merge_sort(arr, m + 1, r);
            merge(arr, l, m, r);
        }
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*:
    - Best Case: $O(n log n)$
    - Average Case: $O(n log n)$
    - Worst Case: $O(n log n)$
  - *Space Complexity*: $O(n)$ (due to auxiliary arrays)
  - *Stability*: Stable
]

== Quick Sort

#align(horizon)[
  *Quick Sort* is a divide and conquer algorithm that selects a *pivot* element
  and partitions the array around the pivot,
  such that elements less than the pivot are on the left,
  and elements greater than the pivot are on the right.

  It then recursively sorts the subarrays on either side of the pivot.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. Choose a pivot element.
  2. Partition the array into two subarrays:
    - Elements less than the pivot.
    - Elements greater than the pivot.
  3. Recursively apply the above steps to the subarrays.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Quick Sort],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Function quick_sort(array A, low, high):],
      )[
        + *if* $"low" < "high"$:
          + $pi := "partition"(A, "low", "high")$
          + $"quick_sort"(A, "low", pi - 1)$
          + $"quick_sort"(A, pi + 1, "high")$
      ]
    ],
  ) <quick-sort>
]

#pagebreak()

=== `partition` Function

#align(horizon)[
  The `partition` function rearranges the array such that:

  - All elements less than the pivot come before it.
  - All elements greater than the pivot come after it.
  - The pivot is in its final sorted position.

  It returns the index of the pivot.
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 9pt)[
    ```c
    int partition(int arr[], int low, int high) {
        int pivot = arr[high]; // pivot
        int i = low;          // Index of smaller element

        for (int j = low; j <= high - 1; j++) {
            // If current element is smaller than or equal to pivot
            if (arr[j] <= pivot) {
                i++;    // increment index of smaller element
                int temp = arr[i];
                arr[i] = arr[j];
                arr[j] = temp;
            }
        }
        // Swap arr[i] and arr[high] (or pivot)
        int temp = arr[i];
        arr[i[high];
        arr[high] = temp;

        return i;
    }
    ```
  ]
]

#pagebreak()

#align(horizon)[
  #text(size: 14pt)[
    ```c
    void quick_sort(int arr[], int low, int high) {
        if (low < high) {
            int pi = partition(arr, low, high);

            // Separately sort elements before
            // partition and after partition
            quick_sort(arr, low, pi - 1);
            quick_sort(arr, pi + 1, high);
        }
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*:
    - Best Case: $O(n log n)$
    - Average Case: $O(n log n)$
    - Worst Case: $O(n^2)$
      (when the smallest or largest element is always chosen as the pivot)
  - *Space Complexity*: $O(log n)$ (due to recursive calls)
  - *Stability*: Unstable
]

== Heap Sort

#align(horizon)[
  *Heap Sort* involves building a `max heap` from the input data,
  and then repeatedly extracting the maximum element from the heap
  and rebuilding the heap.

  It uses the properties of a heap data structure to sort elements.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. Build a `max heap` from the input data.
  2. Swap the root (maximum value) of the heap with the last element.
  3. Reduce the heap size by one.
  4. `heapify` the root element to get the highest element at root again.
  5. Repeat steps 2 to 4 until the heap size is greater than 1.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Heap Sort],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Function heap_sort(array A, n):],
      )[
        + *for* $i$ from $n / 2 - 1$ down to $0$:
          + heapify(A, n, i)
        + *for* $i$ from $n - 1$ down to $0$:
          + swap $A[0]$ and $A[i]$
          + heapify(A, i, 0)
      ]
    ],
  ) <heap-sort>
]

#pagebreak()

=== `heapify` Function

#align(horizon)[
  The `heapify` function ensures the subtree rooted at index $i$ satisfies
  the max heap property.

  - If the children of node $i$ are max heaps
    but node $i$ might be smaller than its children.
  - Swap node $i$ with its largest child.
  - Recursively `heapify` the affected subtree.
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 9pt)[
    ```c
    void heapify(int arr[], int n, int i) {
        int largest = i;   // Initialize largest as root
        int l = 2 * i + 1; // left = 2*i + 1
        int r = 2 * i + 2; // right = 2*i + 2
        // If left child is larger than root
        if (l < n && arr[l] > arr[largest])
            largest = l;
        // If right child is larger than largest so far
        if (r < n && arr[r] > arr[largest])
            largest = r;
        // If largest is not root
        if (largest != i) {
            int swap = arr[i];
            arr[i] = arr[largest];
            arr[largest] = swap;
            // Recursively heapify the affected sub-tree
            heapify(arr, n, largest);
        }
    }
    ```
  ]
]

#pagebreak()

#align(horizon)[
  #text(size: 12pt)[
    ```c
    void heap_sort(int arr[], int n) {
        // Build heap (rearrange array)
        for (int i = n / 2 - 1; i >= 0; i--)
            heapify(arr, n, i);

        // One by one extract an element from heap
        for (int i = n - 1; i >= 0; i--) {
            // Move current root to end
            int temp = arr[0];
            arr[0] = arr[i];
            arr[i] = temp;

            // call max heapify on the reduced heap
            heapify(arr, i, 0);
        }
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*:
    - Best Case: $O(n log n)$
    - Average Case: $O(n log n)$
    - Worst Case: $O(n log n)$
  - *Space Complexity*: $O(1)$
  - *Stability*: Unstable
]

== Counting Sort

#align(horizon)[
  *Counting Sort* is an integer sorting algorithm that operates by counting
  the number of objects that possess distinct key values (kind of hashing).
  It is not a comparison sort and has a running time of $O(n + k)$ where $k$
  is the range of the input data.

  It's efficient when the range of input data is not significantly greater
  than the number of objects to be sorted.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. Find the maximum element in the array.
  2. Initialize a count array of size ($max + 1$) with zeros.
  3. Store the count of each element at their respective index.
  4. Modify the count array by adding the previous counts.
  5. Build the output array by placing elements at their correct positions.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Counting Sort],
    text(size: 10pt)[
      #pseudocode-list(
        title: smallcaps[Given an array $A$ of $n$ elements:],
      )[
        + $max := "find_max"(A)$
        + initialize count array $C[0..max]$
        + *for* each element in $A$:
          + $C["element"] := C["element"] + 1$
        + *for* $i$ from $1$ to $max$:
          + $C[i] := C[i] + C[i - 1]$
        + *for* $i$ from $n - 1$ down to $0$:
          + $"output"[C[A[i]] - 1] := A[i]$
          + $C[A[i]] := C[A[i]] - 1$
        + copy output array to $A$
      ]
    ],
  ) <counting-sort>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 5.8pt)[
    ```c
    void counting_sort(int arr[], int n) {
        int output[n];
        int max = arr[0];
        // Find the largest element in the array
        for (int i = 1; i < n; i++) {
            if (arr[i] > max)
                max = arr[i];
        }
        int count[max + 1];
        // Initialize count array with all zeros
        for (int i = 0; i <= max; ++i)
            count[i] = 0;
        // Store the count of each element
        for (int i = 0; i < n; i++)
            count[arr[i]]++;
        // Store the cumulative count
        for (int i = 1; i <= max; i++)
            count[i] += count[i - 1];
        // Find the index of each element of the original array in count array, and
        // place the elements in output array
        for (int i = n - 1; i >= 0; i--) {
            output[count[arr[i]] - 1] = arr[i];
            count[arr[i]]--;
        }
        // Copy the sorted elements into original array
        for (int i = 0; i < n; i++)
            arr[i] = output[i];
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*: $O(n + k)$ where $k$ is the range of the input.
  - *Space Complexity*: $O(n + k)$
  - *Stability*: Stable
  - *Limitation*: Only works with integers and when $k$ is not significantly greater than $n$.
]

== Radix Sort

#align(horizon)[
  *Radix Sort* is a non-comparative sorting algorithm that sorts data
  with integer keys by grouping keys by individual digits that share
  the same significant position and value.

  It uses Counting Sort as a subroutine to sort elements.
]

#pagebreak()

=== Algorithm Steps

#align(horizon)[
  1. Find the maximum number to know the number of digits.
  2. Do `counting_sort` for every digit, starting from least significant digit
    to most significant digit.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Radix Sort],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Function radix_sort(array A, n):],
      )[
        + $max := "find_max"(A)$
        + *for* $"exp" := 1$; $floor("max" / "exp") > 0$; $"exp" *= 10$:
          + $"counting_sort_by_digit"(A, n, "exp")$
      ]
    ],
  ) <radix-sort>
]

#pagebreak()

=== `counting_sort_by_digit` Function

#align(horizon)[
  The `counting_sort_by_digit` function sorts the array according
  to the digit represented by `exp` (exponent).

  - For $"exp" = 1$, sort according to the least significant digit.
  - For $"exp" = 10$, sort according to the second least significant digit.
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 9pt)[
    ```c
    void counting_sort_by_digit(int arr[], int n, int exp) {
        int output[n];
        int count[10] = {0};
        // Store count of occurrences in count[]
        for (int i = 0; i < n; i++)
            count[(arr[i] / exp) % 10]++;
        // Change count[i] so that count[i] contains actual
        // position of this digit in output[]
        for (int i = 1; i < 10; i++)
            count[i] += count[i - 1];
        // Build the output array
        for (int i = n - 1; i >= 0; i--) {
            output[count[(arr[i] / exp) % 10] - 1] = arr[i];
            count[(arr[i] / exp) % 10]--;
        }
        // Copy the output array to arr[], so that arr[] now
        // contains sorted numbers according to current digit
        for (int i = 0; i < n; i++)
            arr[i] = output[i];
    }
    ```
  ]
]

#pagebreak()

#align(horizon)[
  #text(size: 14pt)[
    ```c
    void radix_sort(int arr[], int n) {
        // Find the maximum number to know the number of digits
        int max = arr[0];
        for (int i = 1; i < n; i++)
            if (arr[i] > max)
                max = arr[i];

        // Do counting sort for every digit
        for (int exp = 1; max / exp > 0; exp *= 10)
            counting_sort_by_digit(arr, n, exp);
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Time Complexity*: $O(d dot.c (n + b))$
    - $d$: Number of digits
    - $b$: Base of numbering system (10 for decimal)
  - *Space Complexity*: $O(n + b)$
  - *Stability*: Stable
  - *Limitation*: Works best when $d$ is not significantly large.
]

== Comparison of Sorting Algos

#align(horizon)[
  #text(size: 9pt)[
    #table(
      columns: 7,
      align: left + horizon,
      table.header(
        [*Algorithm*],
        [*Best Case*],
        [*Average Case*],
        [*Worst Case*],
        [*Space*],
        [*Stable*],
        [*Method*],
      ),

      [*Bubble Sort*], [$O(n)$], [$O(n^2)$], [$O(n^2)$], [$O(1)$], [Yes], [Exchange],
      [*Selection Sort*], [$O(n^2)$], [$O(n^2)$], [$O(n^2)$], [$O(1)$], [No], [Selection],
      [*Insertion Sort*], [$O(n)$], [$O(n^2)$], [$O(n^2)$], [$O(1)$], [Yes], [Insertion],
      [*Merge Sort*], [$O(n log n)$], [$O(n log n)$], [$O(n log n)$], [$O(n)$], [Yes], [Merge],
      [*Quick Sort*], [$O(n log n)$], [$O(n log n)$], [$O(n^2)$], [$O(log n)$], [No], [Partition],
      [*Heap Sort*], [$O(n log n)$], [$O(n log n)$], [$O(n log n)$], [$O(1)$], [No], [Selection],
      [*Counting Sort*], [$O(n + k)$], [$O(n + k)$], [$O(n + k)$], [$O(n + k)$], [Yes], [Counting],
      [*Radix Sort*], [$O(n k)$], [$O(n k)$], [$O(n k)$], [$O(n + k)$], [Yes], [Digit],
    )
  ]
]

== Practical Section (C or pseudocode)

#align(horizon)[
  - *Task*: Implement a sorting algorithm of your choice
    and analyze its time and space complexity.
  - *Task*: Modify the Quick Sort algorithm to use a random pivot
    to improve performance on already sorted arrays.
  - *Task*: Implement a stable version of Selection Sort.
]

= Recursion

==

#align(horizon + center)[
  #image(
    "images/recursion_meme.jpg",
    width: 50%,
  )
]

== What is Recursion?

#align(horizon)[
  *Recursion* is a programming technique where a function calls itself
  to solve a smaller problem of the same type _until_ it reaches a base case.
  It is a way to *divide a complex problem into simpler and more manageable subproblems*.
]

== How Does It Work?

#align(horizon)[
  1. *Base Case*: Defines when the recursive function should stop calling itself.
    It is the stopping condition.

  2. *Recursive Call*: The function calls itself with a modified input
    that brings it closer to the base case.

  3. *Resolution*: The recursive calls return values that
    are combined to solve the original problem.
]

== Classic Example: Factorial

#align(horizon)[
  - *Mathematical Definition*:

    $
      n! = cases(
        1 "if" n = 0,
        n times (n - 1)! "if" n > 0
      )
    $

  - *Recursive Implementation in C*:

    ```c
    int factorial(int n) {
      if (n == 0)
        return 1;
      else
        return n * factorial(n - 1);
    }
    ```

  #pagebreak()

  - *Bonus: Implementation in Haskell*:

    ```haskell
    fatorial :: Int -> Int
    fatorial 0 = 1
    fatorial n = n * fatorial (n - 1)
    ```
]

== Visualization of Factorial Recursion

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph Factorial {
        n4 [label="f(4)"];
        n3 [label="f(3)"];
        n2_1 [label="f(2)"];
        n2_2 [label="f(2)"];
        n1_1 [label="f(1)"];
        n1_2 [label="f(1)"];
        n1_3 [label="f(1)"];
        n0_1 [label="f(0)"];
        n0_2 [label="f(0)"];

        n4 -> {n3, n2_1};
        n3 -> {n2_2, n1_3};
        n2_1 -> {n1_1, n0_1};
        n2_2 -> {n1_2, n0_2};
      }
      ```,
      width: 70%,
    ),
    caption: "Recursion Tree for factorial(4)",
  )
]

== When to Use Recursion?

#align(horizon)[
  - *Problems that can be divided into similar subproblems*:
    Such as trees, graphs, and hierarchical structures.

  - *Algorithms that require backtracking*:
    Such as depth-first search (DFS), permutation and combination algorithms.

  - *Simplify Implementation*: Some algorithms are easier to
    implement recursively than iteratively.
]

== Advantages and Disadvantages

#align(horizon)[
  *Advantages*:

  - Cleaner and more readable code for certain problems.
  - Naturally suited for recursive data structures (trees, graphs).
  - Facilitates solving complex problems by dividing them into smaller parts.

  #pagebreak()

  *Disadvantages*:

  - Consumes more memory due to the call stack.
  - May be less efficient in terms of time compared to iterative solutions.
  - Risk of stack overflow if the recursion is too deep.
]

== Recursion vs Iteration

#align(horizon)[
  - *Recursion*:
    - Uses function calls to repeat code.
    - May be less efficient due to function call overhead.
    - More intuitive for problems that are naturally recursive.
  - *Iteration*:
    - Uses repetition structures like loops (`for`, `while`).
    - Generally more efficient in terms of memory and time usage.
    - May be less intuitive for certain problems.

  #pagebreak()

  *Example*: Calculating the factorial of `n`.

  - *Recursive*:

    ```c
    int factorial(int n) {
      if (n == 0)
        return 1;
      else
        return n * factorial(n - 1);
    }
    ```

  #pagebreak()

  - *Iterative*:

    ```c
    int factorial(int n) {
      int result = 1;
      for (int i = 2; i <= n; i++) {
        result *= i;
      }
      return result;
    }
    ```
]

== Beware of Excessive Recursion

#align(horizon)[
  - *Stack Overflows*:
    Each recursive call adds a frame to the call stack.
    Very deep recursion can lead to overflows.

  - *Redundant Calculations*:
    In some recursions, like the naive Fibonacci calculation,
    many calculations are repeated.

  - *Optimization*: Techniques like *_memoization_* or
    transforming recursion into iteration can improve efficiency.
]

== Memoization

#align(horizon)[
  Stores the results of subproblems already solved to avoid repeated calculations.

  *Example with Fibonacci*:

  #text(size: 11pt)[
    ```c
    int fibonacci(int n, int memo[]) {
      if (memo[n] != -1)
        return memo[n];
      if (n == 0)
        memo[n] = 0;
      else if (n == 1)
        memo[n] = 1;
      else
        memo[n] = fibonacci(n - 1, memo) + fibonacci(n - 2, memo);
      return memo[n];
    }
    ```
  ]
]

== Tail Recursion

#align(horizon)[
  - *Definition*:
    A recursion where the recursive call is the last operation of the function.

  - *Benefits*: Some compilers can optimize tail recursions
    to avoid stack growth (tail call elimination).

  - *Example*:

  #text(size: 11pt)[
    ```c
    int factorial_tail(int n, int accumulator) {
      if (n == 0)
        return accumulator;
      else
        return factorial_tail(n - 1, n * accumulator);
    }
    ```
  ]
]

== Practical Part (C or pseudocode)

#align(horizon)[
  - *Problem*: Implement a recursive function that determines
    if a word is a palindrome.

  - *Hint*: Compare the first and last characters of the string
    and call the function recursively on the inner substring.
]

= Divide and Conquer

==

#align(horizon + center)[
  #image(
    "images/divide_and_conquer_meme.png",
    width: 50%,
  )
]

== What is Divide and Conquer?

#align(horizon)[
  *Divide and Conquer* is an algorithm design paradigm that consists
  of dividing a problem into smaller subproblems,
  solving these subproblems recursively,
  and then combining the solutions to obtain the final solution.
]

== How Does It Work?

#align(horizon)[
  1. *Divide*: The problem is divided into smaller subproblems that
    are instances of the same type as the original problem.

  2. *Conquer*: The subproblems are solved recursively.
    If they are small enough, they are solved directly.

  3. *Combine*: The solutions of the subproblems are combined to
    solve the original problem.
]

== Classic Examples

#align(horizon)[
  - *_Merge Sort_*: A sorting algorithm that divides the _array_ in half,
    sorts each half, and then combines the two sorted halves.

  - *_Quick Sort_*: An algorithm that selects a pivot,
    divides the _array_ into _subarrays_ smaller and larger than the pivot,
    and then recursively sorts the _subarrays_.

  - *Binary Search*: A search method that
    divides the search space in half at each iteration.
]

== Master Theorem

#align(horizon)[
  In algorithm analysis,
  the #link("https://en.wikipedia.org/wiki/Master_theorem_(analysis_of_algorithms)")[*Master Theorem*]
  for divide and conquer recurrences provides
  an asymptotic analysis (using Big-O notation)
  for *recurrence relations* that occur in the analysis of
  many divide and conquer algorithms.

  #pagebreak()

  Consider a problem that can be solved using a recursive algorithm
  like the following:

  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Example of Recursion],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Procedure $p$(input $x$ of size $n$)],
      )[
        + *if* $n < "some constant" k$:
          + solve $x$ directly, without recursion
        + *else*:
          + create $a$ subproblems from $x$, each of size $n/b$
          + call procedure $p$ recursively on each subproblem
          + combine the results of the subproblems
      ]
    ],
  ) <master-theorem>

  #pagebreak()

  - The call tree has a node for each recursive call.
  - The leaf nodes are the base cases of the recursion:
    subproblems of size less than $k$ that are not solved recursively.
  - Each node performs an amount of work corresponding to the size of
    the subproblem $m$ given by $p(m)$.
  - The total amount of work performed by the complete algorithm is
    the sum of the work performed by all nodes in the tree.

  #pagebreak()

  #align(horizon + center)[
    #image(
      "images/master_theorem_intuition.png",
      width: 100%,
    )
  ]

]

== Complexity Analysis

#align(horizon)[
  The complexity of divide and conquer algorithms can be
  expressed by the *Master Recurrence*:

  $ T(n) = a T(n / b) + f(n) $

  #pagebreak()

  Where:

  - $T(n)$: Execution time of the algorithm on an input of size $n$.
  - $a$: Number of subproblems.
  - $b$: Factor by which the problem size is divided.
  - $f(n)$: Cost of dividing and combining the subproblems.

  #pagebreak()

  The solution to this recurrence depends on the relationship between $f(n)$ and $n^(log_b a)$.

  - *Case 1*: if $f(n) = O(n^(log_b a - epsilon))$ for some constant $epsilon > 0$,
    then $T(n) = O(n^(log_b a))$.

  - *Case 2*: if $f(n) = O(n^(log_b a))$, then $T(n) = O(n^(log_b a) log n)$.

  - *Case 3*: if $f(n) = O(n^(log_b a + epsilon))$ for some constant $epsilon > 0$
    and if $a f(n/b) <= c f(n)$ for some constant $c < 1$,
    then $T(n) = O(f(n))$.
]

== Example: _Merge Sort_

#align(horizon)[
  #text(size: 14pt)[
    - *_Merge Sort_* divides the problem into 2 subproblems of size $n/2$:

    $ T(n) = 2 T(n / 2) + O(n) $

    - Here, $a = 2$, $b = 2$, $f(n) = O(n)$.

    - We calculate $n^(log_b a) = n^(log_2 2) = n^1$.

    - Since $f(n) = O(n^(log_b a))$, we are in *Case 2* of the Master Theorem.

    - Therefore, $T(n) = O(n log n)$.
  ]
]

== Example: _Quick Sort_ (Worst Case)

#align(horizon)[
  - In the worst case, *_Quick Sort_* divides the problem into
    one subproblem of size $n - 1$ and another of size 0:

  $ T(n) = T(n - 1) + O(n) $

  - This recurrence resolves to $T(n) = O(n^2)$.

  - In the best case (balanced partitions), the complexity is $O(n log n)$.
]

== Applications of Divide and Conquer

#align(horizon)[
  - *Multiplication of Large Integers*
    (#link("https://en.wikipedia.org/wiki/Karatsuba_algorithm")[Karatsuba Algorithm])
  - *Fast Fourier Transform*
    (#link("https://en.wikipedia.org/wiki/Fast_Fourier_transform")[FFT])
  - *Matrix Multiplication*
    (#link("https://en.wikipedia.org/wiki/Strassen_algorithm")[Strassen's Algorithm])
  - *Computational Geometry Problems*
    (#link("https://en.wikipedia.org/wiki/Convex_hull")[Convex Hull], etc.)
]

== Advantages and Disadvantages

#align(horizon)[
  #text(size: 14pt)[
    *Advantages*:

    - Can reduce the complexity of complex problems.
    - Utilizes recursion, facilitating the implementation of complex algorithms.

    *Disadvantages*:

    - May have time and space overhead due to recursive calls.
    - Not all problems are naturally divisible into smaller subproblems.
  ]
]

== Practical Part (C or pseudocode)

#align(horizon)[
  - *Problem*: Implement an algorithm that raises a number $x$
    to a power $n$ using the divide and conquer paradigm,
    optimizing for $O(log n)$.

  - *Hint*: Use the property that $x^n = (x^(n/2))^2$ for even $n$.
]

= Greedy Algorithms

==

#align(horizon + center)[
  #image(
    "images/greedy_algo_meme.jpeg",
    width: 95%,
  )
]

== Introduction

#align(horizon)[
  *Greedy Algorithms* are an approach to solve optimization problems
  that follows the strategy of making the best local choice at each step,
  with the hope of finding a globally optimal solution.

  The idea is to build a solution piece by piece,
  always choosing the option that seems the best at the moment.
]

== Characteristics of Greedy Algorithms

#align(horizon)[
  - *Greedy Choice*: At each step, choose the option that seems the best at the moment.
  - *Optimal Substructure*: An optimal solution to the problem contains optimal solutions to subproblems.
  - *Irrevocability*: Decisions made are not reconsidered later.
]

== When to Use Greedy Algorithms

#align(horizon)[
  - When the problem exhibits the *greedy property*, that is, an optimal solution can be achieved by making locally optimal choices.
  - When the problem has the *optimal substructure property*, allowing optimal solutions to be constructed from optimal subsolutions.
]

== Example: Fractional Knapsack

#align(horizon)[
  - *Problem*: Given a set of items, each with a weight and a value, determine the fraction of each item to include in a knapsack with limited weight capacity, so as to maximize the total value.
  - *Note*: Items can be divided (unlike the 0-1 Knapsack).
]

#pagebreak()

=== Greedy Approach

#align(horizon)[
  1. Compute the value per unit weight for each item $(v_i / w_i)$.
  2. Sort the items in decreasing order of value per unit weight.
  3. Include as much as possible of the item with the highest value per unit weight.
  4. If the capacity allows, move to the next item in order and repeat step 3.
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [Greedy Fractional Knapsack],
    text(size: 8pt)[
      #pseudocode-list(
        title: smallcaps[Function fractional_knapsack(capacity, items):],
      )[
        + *for* each item in items:
          + compute $"value_per_weight" := "value" / "weight"$
        + sort items in decreasing order of $"value_per_weight"$
        + $"total_value" := 0$
        + *for* each item in sorted items:
          + *if* $"capacity" > 0$:
            + $"amount" := \min("item_weight", "capacity")$
            + $"total_value" := "total_value" + "amount" times "value_per_weight"$
            + $"capacity" := "capacity" - "amount"$
          + *else*:
            + *break* the loop
        + *return* $"total_value"$
      ]
    ],
  ) <fractional-knapsack>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 6.8pt)[
    ```c
    typedef struct {
        double value;
        double weight;
    } Item;

    int compare(const void *a, const void *b) {
        Item *itemA = (Item *)a;
        Item *itemB = (Item *)b;
        double ratioA = itemA->value / itemA->weight;
        double ratioB = itemB->value / itemB->weight;
        return (ratioB - ratioA) > 0 ? 1 : -1;
    }

    double fractional_knapsack(Item items[], int n, double capacity) {
        qsort(items, n, sizeof(Item), compare);
        double total_value = 0.0;
        double remaining_capacity = capacity;

        for (int i = 0; i < n && remaining_capacity > 0; i++) {
            double amount = fmin(items[i].weight, remaining_capacity);
            total_value += amount * (items[i].value / items[i].weight);
            remaining_capacity -= amount;
        }

        return total_value;
    }
    ```
  ]
]

#pagebreak()

=== Complexity Analysis

#align(horizon)[
  - *Sorting Time*: $O(n log n)$ due to Quicksort.
  - *Total Time*: $O(n log n)$
  - *Auxiliary Space*: $O(1)$ (if the sorting is in-place)
]

== Important Characteristics of Greedy Algorithms

#align(horizon)[
  - *Simple and Intuitive*: Easy to implement and understand.
  - *Efficiency*: Often have a lower time complexity than algorithms like dynamic programming.
  - *Limitations*: Do not always provide the globally optimal solution; depend on the problem's properties.
]

== Common Problems Solved with Greedy Algorithms

#align(horizon)[
  - Activity Selection Problem
  - Huffman Coding Problem
  - Coin Change Problem
  - Minimum Cost Path in Graphs (Dijkstra)
  - Traveling Salesman Problem (Heuristics)
]

#pagebreak()

== When Greedy Algorithms Do Not Work

#align(horizon)[
  - *0-1 Knapsack Problem*:
    It's not possible to divide items; the greedy approach does not guarantee the optimal solution.
  - *Traveling Salesman Problem*:
    Choosing the nearest next city does not lead to the globally optimal route.
]

== Comparison with Dynamic Programming

#align(horizon)[
  #text(size: 13pt)[
    - *Dynamic Programming:*
      - Solves smaller subproblems and stores their results to avoid recomputation.
      - Guarantees the globally optimal solution.
      - Can be more complex and consume more time and space.
    - *Greedy Algorithms:*
      - Make the best local choice without considering subproblems.
      - May not guarantee the globally optimal solution.
      - More efficient in terms of time and space.
  ]
]

#pagebreak()

== Conclusion

#align(horizon)[
  - *Advantages:*
    - Simplicity and efficiency.
    - Suitable for problems with greedy properties and optimal substructure.
  - *Disadvantages:*
    - Not applicable to all problems.
    - May not find the globally optimal solution.
  - *Important:*
    - Carefully analyze the problem to determine if a greedy approach is suitable.
]

== Practical Section

#align(horizon)[
  - *Task*: Solve the 0-1 Knapsack Problem using a greedy approach and compare with the optimal solution.
  - *Task*: Identify a problem that cannot be optimally solved with greedy algorithms and explain why.
]

= Dynamic Programming

==

#align(horizon + center)[
  #image(
    "images/dynamic_programming_meme.png",
    width: 95%,
  )
]

== Introduction

#align(horizon)[
  *Dynamic Programming* is an optimization technique that solves complex problems
  by breaking them down into smaller, simpler subproblems, storing the results
  of already solved subproblems to avoid redundant computations.

  It is particularly useful for problems that exhibit *optimal substructure*
  and *overlapping subproblems*.
]

== Characteristics of Dynamic Programming

#align(horizon)[
  - *Optimal Substructure*: The optimal solution to the problem can be constructed from the optimal solutions of its subproblems.
  - *Overlapping Subproblems*: Subproblems are solved multiple times; therefore, it is efficient to store their results.
  - *Memoization*: Storing results of already solved subproblems.
]

== Implementation Approaches

#align(horizon)[
  - *Top-Down with Memoization*:
    - Recursive implementation.
    - Stores results of subproblems in a data structure (such as a table or dictionary).
  - *Bottom-Up (Iterative)*:
    - Solves smaller subproblems first and uses their results to build solutions for larger subproblems.
    - Generally involves building a table.
]

== Example: Fibonacci

#align(horizon)[
  - *Recursive Definition*:
    - $F(0) = 0$
    - $F(1) = 1$
    - $F(n) = F(n-1) + F(n-2)$, for $n >= 2$
  - *Problem:* Calculate the $n$-th Fibonacci number.
]

#pagebreak()

=== Simple Recursive Implementation (Inefficient)

#align(horizon)[
  ```c
  int fibonacci(int n) {
      if (n <= 1)
          return n;
      else
          return fibonacci(n - 1) + fibonacci(n - 2);
  }
  ```
  - *Time Complexity:* Exponential, $O(2^n)$.
  - *Issue:* Many redundant computations.
]

#pagebreak()

=== Implementation with Memoization (Top-Down)

#align(horizon)[
  #text(size: 11pt)[
    ```c
    int fib_memo[MAX]; // Initialize with -1
    int fibonacci(int n) {
    if (fib_memo[n] != -1)
        return fib_memo[n];
    if (n <= 1)
        fib_memo[n] = n;
    else
        fib_memo[n] = fibonacci(n - 1) + fibonacci(n - 2);
    return fib_memo[n];
    }
    ```
    - *Time Complexity:* $O(n)$
    - *Advantage:* Avoids redundant computations by storing results.
  ]
]

#pagebreak()

=== Iterative Implementation (Bottom-Up)

#align(horizon)[
  #text(size: 14pt)[
    ```c
    int fibonacci(int n) {
        int fib[n + 1];
        fib[0] = 0;
        fib[1] = 1;
        for (int i = 2; i <= n; i++) {
            fib[i] = fib[i - 1] + fib[i - 2];
        }
        return fib[n];
    }
    ```
    - *Time Complexity:* $O(n)$
    - *Space Complexity:* $O(n)$
  ]
]

#pagebreak()

=== Space Optimization

#align(horizon)[
  #text(size: 11pt)[
    ```c
    int fibonacci(int n) {
        int a = 0, b = 1, c;
        if (n == 0) return a;
        for (int i = 2; i <= n; i++) {
            c = a + b;
            a = b;
            b = c;
        }
        return b;
    }
    ```
    - *Time Complexity:* $O(n)$
    - *Space Complexity:* $O(1)$
    - *Advantage:* Uses only scalar variables, saving memory.
  ]
]

== Example: 0-1 Knapsack

#align(horizon)[
  - *Problem*: Given a set of items, each with a weight and a value, determine the number of each item to include in a collection so that the total weight is less than or equal to a given capacity and the total value is maximized.
  - *Constraints*:
    - Each item can be included at most once.
    - Items cannot be divided.
]

#pagebreak()

=== Approach with Dynamic Programming

#align(horizon)[
  - *Function Definition*:
    - $K(n, W)$ is the maximum value that can be obtained considering the first $n$ items and maximum capacity $W$.
  - *Recurrence*:
    - If $w_n > W$ (weight of item $n$ is greater than current capacity):
      $K(n, W) = K(n-1, W)$
    - Else:
      $K(n, W) = max(K(n-1, W), v_n + K(n-1, W - w_n))$
]

#pagebreak()

=== Pseudocode

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algorithm],
    caption: [0-1 Knapsack with Dynamic Programming],
    text(size: 8pt)[
      #pseudocode-list(
        title: smallcaps[Function knapsack(values, weights, n, W):],
      )[
        + create table $K[0..n][0..W]$
        + for $i$ from $0$ to $n$:
          + for $w$ from $0$ to $W$:
            + $K[i][w] = 0$
        + for $i$ from $1$ to $n$:
          + for $w$ from $0$ to $W$:
            + if $"weights"[i-1] \leq w$:
              + $K[i][w] = max(K[i-1][w], "values"[i-1] + K[i-1][w - "weights"[i-1]])$
            + else:
              + $K[i][w] = K[i-1][w]$
        + return $K[n][W]$
      ]
    ],
  ) <knapsack>
]

#pagebreak()

=== Example in C

#align(horizon)[
  #text(size: 10pt)[
    ```c
    int knapsack(int W, int weights[], int values[], int n) {
      int i, w;
      int K[n + 1][W + 1];
      // Build table K[][] in bottom-up manner
      for (i = 0; i <= n; i++) {
        for (w = 0; w <= W; w++) {
          if (i == 0 || w == 0)
            K[i][w] = 0;
          else if (weights[i - 1] <= w)
            K[i][w] = max(values[i - 1] +
                K[i - 1][w - weights[i - 1]],  K[i - 1][w]);
          else
            K[i][w] = K[i - 1][w];
        }
      }

      return K[n][W];
    }
    ```
  ]
]

== Complexity Analysis

#align(horizon)[
  - *Time Complexity:* $O(n W)$, where $n$ is the number of items and $W$ is the knapsack capacity.
  - *Space Complexity:* $O(n W)$, due to the table used.
]

== Other Classic Problems

#align(horizon)[
  - *Longest Common Subsequence (LCS) Problem*
  - *Shortest Path Problem* (like the Floyd-Warshall algorithm)
  - *Rod Cutting Problem*
  - *Task Scheduling Problem*
]

== How to Identify Suitable Problems

#align(horizon)[
  #text(size: 14pt)[
    - *Optimal Substructure:*
      The optimal solution to the problem can be constructed from the optimal solutions of its subproblems.
    - *Overlapping Subproblems:*
      The problem can be divided into subproblems that are reused multiple times.
    - *Sample Questions:*
      - Can the problem be divided into smaller steps?
      - Do the smaller steps repeat?
  ]
]

== Comparison with Greedy Algorithms

#align(horizon)[
  #text(size: 14pt)[
    - *Greedy Algorithms:*
      - Make the best local choice at each step.
      - Do not revisit previous decisions.
      - May not find the globally optimal solution.
    - *Dynamic Programming:*
      - Considers all possibilities and chooses the best.
      - Stores results of subproblems to avoid recomputation.
      - Guarantees the globally optimal solution.
  ]
]

== Optimization Techniques

#align(horizon)[
  #text(size: 14pt)[
    - *Space Optimization:*
      Reduce space used by storing only what's necessary.
      - *Example:*
        Use one-dimensional arrays instead of two-dimensional matrices when possible.
    - *Subproblem Reuse:*
      Identify identical subproblems to avoid recomputation.
    - *Dynamic Programming with Memoization vs. Iterative:*
      Choose the approach that best fits the problem and resource constraints.
  ]
]

== Conclusion

#align(horizon)[
  #text(size: 14pt)[
    - *Advantages:*
      - Guarantees the globally optimal solution.
      - Avoids redundant computations.
    - *Challenges:*
      - May consume a lot of time and space for problems with large inputs.
      - Identifying the optimal substructure and overlapping subproblems is not always trivial.
    - *Important:*
      - Correctly formulate the recurrence and initial conditions.
      - Decide between Top-Down and Bottom-Up approaches.
  ]
]

== Practical Section

#align(horizon)[
  - *Task*: Implement the LCS (Longest Common Subsequence) algorithm and analyze its complexity.
  - *Task*: Solve the Rod Cutting Problem using dynamic programming and compare with the simple recursive approach.
  - *Task*: Identify a real-world problem that can be optimized using dynamic programming and develop the solution.
]