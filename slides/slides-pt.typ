#import "@preview/slydst:0.1.4": *
#import "@preview/diagraph:0.3.5": *
#import "@preview/lovelace:0.3.0": *

#set text(lang: "pt")

#show: slides.with(
  title: "Teoria dos Grafos e Complexidade Computacional",
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
Level-one headings corresponds to new sections.
Level-two headings corresponds to new slides.
Blank space can be filled with vertical spaces like #v(1fr).
*/

== Licença

#align(horizon + center)[#image("images/cc-zero.svg", width: 80%)]

== Links

#align(horizon + center)[
  Todos os links estão em #text(blue)[azul].

  Sinta-se à vontade para clicar neles.
]

== Conteúdo

#outline()

= Por que estudar Teoria dos Grafos e Complexidade Computacional?

==

#align(horizon + center)[#image(
  "images/algorithm_analysis_meme.jpg",
  width: 50%,
)]

== Teoria Computação

#align(horizon)[
  A *teoria da computação* é subcampo da ciência da computação e matemática que
  busca determinar quais problemas podem ser computados em um dado modelo de
  computação.

  A *computação* pode ser definida como o cálculo de uma função por meio de um
  algoritmo.
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
    caption: "Alan Turing e Alonzo Church",
  )<turing-church>
]

#pagebreak()

#align(horizon)[
  - Turing propôs a *máquina de Turing* como modelo de computação.
  - Alonzo Church propôs o *cálculo lambda* como modelo de computação.
  - Ambos os modelos são matematicamente *equivalentes*.
]

== Algoritmo

#align(horizon)[
  *Algoritmo* é uma sequência finita de ações executáveis que visam obter uma
  solução para um determinado tipo de problema.
]

== Teoria dos Grafos

#align(horizon)[
  Por que estudar Grafos?

  #v(1em)

  #align(center)[
    _Quase_ tudo que você faz em computação pode ser modelado como um *problema de
      grafos*.
  ]
]

== Complexidade Computacional

#align(horizon)[
  *Complexidade Computacional* é um campo da ciência da computação que estuda a
  quantidade de recursos necessários para resolver um problema computacional#footnote[
    um problema decidível.
  ].
]

#pagebreak()

#align(horizon)[
  Usamos a notação $O$ para descrever a complexidade de um algoritmo.
  - $O(1)$ (complexidade *constante*):
    - Acessar uma _array_
    - Inserir um nó em uma lista encadeada
    - Inserção e remoção em uma fila
  - $O(log n)$ (complexidade *logarítmica*):
    - Busca binária
    - Inserção e remoção em uma árvore binária de busca balanceada

  #pagebreak()

  - $O(n)$ (complexidade *linear*):
    - Percorrer um _array_
    - Percorrer uma lista encadeada
    - Comparar duas _strings_
  - $O(n log n)$ (complexidade *log-linear*):
    - Algoritmo de ordenação _Quick Sort_ (caso médio)
    - Algoritmo de ordenação _Merge Sort_

  #pagebreak()

  - $O(n^2)$ (complexidade *quadrática*):
    - Percorrer uma matriz
    - Algoritmo de ordenação _Bubble Sort_
    - Algoritmo de ordenação _Insertion Sort_
    - Algoritmo de ordenação _Quick Sort_ (pior caso)
  - $O(n^3)$ (complexidade *cúbica*):
    - Multiplicação de matrizes (abordagem ingênua)
  - $O(n!)$ (complexidade *fatorial*):
    - Solução do problema do caixeiro-viajante
    - Gerar todas as permutações de uma lista
]

= Grafos

==

#align(horizon + center)[#image(
  "images/graph_isomorphism_meme.jpg",
  width: 50%,
)]

== O que são Grafos?

Grafos são estruturas matemáticas que modelam *relações entre objetos*.

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
    caption: "Grafo",
  ) <graph>
]

== Formalmente

Grafos são *pares ordenados* $G = (V, E)$ onde:

- $V$ é um conjunto finito de *vértices* (também chamados de nós)
- $E$ é um conjunto finito de *arestas* (também chamadas de arcos) representado
  por um par de vértices $(u, v)$

A @graph, por exemplo:

#text(size: 14pt)[
  $ V = \{a, b, c, d, e, f\} $
  $ E = \{(a, b), (a, c), (b, c), (b, d), (c, e), (d, e), (e, f)\} $
]

== Grafos Direcionados

Grafos podem ser *direcionados* ou *_não_-direcionados*.

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
    caption: "Grafo Direcionado",
  ) <directed-graph>
]

== Grafos Ponderados

Grafos podem ser *ponderados*, isto é, possuem valores associados às
arestas.

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
    caption: "Grafo Ponderado",
  ) <weighted-graph>
]

== Exemplos de Grafos

#align(horizon)[
  - Redes de computadores
  - Redes sociais
  - Mapas de cidades
  - Estruturas moleculares
]

== #link(
  "https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg",
)[As 7 pontes de Königsberg]

Primeira aplicação prática da teoria dos grafos, resolvida por Euler em 1736.

#align(center)[
  *É possível atravessar todas as pontes sem repetir nenhuma?*
]

#align(horizon + center)[
  #figure(
    image("images/konigsberg_briges.png", width: 35%),
    caption: "As 7 pontes de Königsberg",
  ) <konigsberg-brigdes>
]

== #link(
  "https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg",
)[As 7 pontes de Königsberg]

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
    caption: "Grafo das 7 pontes de Königsberg",
  ) <graph-konigsberg-brigdes>
]

== Solução das 7 pontes

#align(horizon)[
  A solução do problema de Königsberg foi dada por Euler. O grafo precisa de *duas
    condições* para ser resolvido:

  - O grafo deve ser *totalmente conectado*
  - O grafo deve ter exatamente *0 ou 2 vértices de grau ímpar*
]

== #link(
  "https://en.wikipedia.org/wiki/Four_color_theorem",
)[O teorema das 4 cores]

#align(horizon)[
  *Não mais do que quatro cores são necessárias para colorir as regiões de qualquer
    mapa, de modo que duas regiões adjacentes não tenham a mesma cor.*
]

#pagebreak()

#align(horizon + center)[
  #figure(
    image("images/four_color_graph.svg", width: 50%),
    caption: "Abstração de um mapa com 4 cores usando grafos",
  ) <four-color-map>
]

#text(size: 14pt)[
  O teorema foi provado em 1976 por Kenneth Appel e Wolfgang Haken#footnote[
    um dos primeiros teoremas provados auxiliado por computadores.
  ].
]

== Aplicações dos Grafos

- Itinerários de companhias aéreas: Calcular o fluxo máximo em um grafo
  direcionado.
- Software de roteamento (GPS): Calcular o menor caminho entre dois pontos.
- Solucionar um sudoku: Resolver um problema de coloração de grafos.
- Algoritmos de busca online: Determinar centralidades de vértices com base em
  temas.
- Redes sociais: encontrar a maior comunidade de amigos.

== Subgrafos

Um *subgrafo* de um grafo $G$ é outro grafo formado a partir de um *subconjunto
  dos vértices e arestas de $G$*. O subconjunto de vértices deve incluir todos os
vértices das arestas, mas pode incluir vértices adicionais.

#align(horizon + center)[
  #figure(
    image("images/subgraph.svg", width: 40%),
    caption: "Subgrafo",
  ) <subgraph>
]

== Subgrafo Induzido

Um *subgrafo induzido* é um subgrafo que *inclui todos os vértices e arestas*
cujos extremos pertencem ao subconjunto de vértices.

#align(horizon + center)[
  #figure(
    image("images/induced_subgraph.svg", width: 50%),
    caption: "Subgrafo Induzido",
  ) <induced-subgraph>
]

== Isomorfismo

Um isomorfismo dos grafos $G$ e $H$ e uma bijeção#footnote[
  uma função que estabelece uma correspondência biunívoca entre os elementos de
  dois conjuntos.
]
entre os conjuntos de vértices de $G$ e $H$:

$ f: V(G) -> V(H) $

#align(horizon + center)[
  #figure(
    image("images/graph_isomorphism.png", width: 66%),
    caption: "Grafos Isomórficos",
  ) <isomorphic-graphs>
]

== Representação de Grafos

#align(horizon)[
  Há várias formas de representar grafos, as mais comuns são:

  - *Matriz de adjacência*
  - *Lista de adjacência*
]

== Matriz de Adjacência

#align(horizon)[
  Uma *matriz de adjacência* é uma matriz quadrada $bold(A)$ de tamanho $n times n$:

  $ bold(A)^(n times n) = a_(i j) $

  onde $a_(i j)$ é o número de arestas entre os vértices $i$ e $j$.
]

#pagebreak()

#align(horizon + center)[
  #figure(
    grid(
      columns: 2,
      gutter: 2mm,
      text[$
        bold(A) = mat(
          1, 1, 0, 0, 1, 0; 1, 0, 1, 0, 1, 0; 0, 1, 0, 1, 0, 0; 0, 0, 1, 0, 1, 1; 1, 1, 0, 1, 0, 0; 0, 0, 0, 1, 0, 0;
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
    caption: "Matriz de adjacência e Grafo",
  ) <adjacency-matrix>
]

#pagebreak()

#align(horizon)[
  Propriedades de uma matriz de adjacência#footnote[
    $n$ é o número de vértices do grafo.
  ]:

  - Simétrica para grafos não-direcionados
  - Não-simétrica para grafos direcionados
  - Custo de espaço $O(n^2)$
  - Custo de construção $O(n^2)$
  - Custo de busca de arestas $O(1)$
]

== Lista de Adjacência

#align(horizon)[
  Uma *lista de adjacência* é uma lista de listas, onde cada lista $L_i$ contém os
  vértices adjacentes ao vértice $i$.
]

#pagebreak()

#align(horizon + center)[
  #figure(
    grid(
      columns: 2,
      gutter: 2mm,
      table(
        columns: 2,
        table.header([*Vértice*], [*Vizinhos*]),
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
    caption: "Lista de adjacência e Grafo",
  ) <adjacency-list>
]

#pagebreak()

#align(horizon)[
  Propriedades de uma lista de adjacência#footnote[
    $n$ é o número de vértices do grafo e $m$ é o número de arestas.
  ]:

  - Custo de espaço $O(n + m)$#footnote[para grafos não-direcionados.]<adjacency-list-cost>
  - Custo de construção $O(m)$#footnote(<adjacency-list-cost>)
  - Custo de busca de arestas $O("deg"(v))$ (pior caso $O(n)$)
]

== Parte Prática (C ou pseudocódigo)

#align(horizon)[
  - Representar um grafo direcionado e não-direcionado
  - Parsear um grafo de uma matriz de adjacência
  - Parsear um grafo de uma lista de adjacência
]

== Caminhos e Ciclos

#align(horizon)[
  #text(size: 14pt)[
    *Caminho* é uma sequência de vértices tal que de cada um dos vértices existe uma
    aresta para o vértice seguinte.

    Um caminho é chamado *simples* se nenhum dos vértices no caminho se repete. O
    *comprimento* do caminho é o número de arestas que o caminho usa, contando-se
    arestas múltiplas vezes.

    O *custo* de um caminho num grafo ponderado é a soma dos custos das arestas
    atravessadas.

    Dois caminhos são *independentes* se não tiverem nenhum vértice em comum, exceto
    o primeiro e o último.
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
    caption: "Caminho de Comprimento 4",
  ) <path>
]

#pagebreak()

#align(horizon)[
  Um *ciclo* é um caminho em que o *primeiro e o último vértice coincidem*, mas
  nenhum outro vértice é *repetido*.
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
    caption: "Ciclo de Comprimento 4",
  ) <cycle>
]

== Caminho Euleriano

#align(horizon)[
  *Caminho Euleriano* é o caminho que usa cada aresta exatamente uma vez. Se tal
  caminho existir, o grafo é chamado traversável.

  Um *ciclo Euleriano* é um ciclo que usa cada aresta exatamente uma vez.
]

== Caminho Hamiltoniano

#align(horizon)[
  *Caminho Hamiltoniano* é o caminho que visita cada vértice exatamente uma vez.

  Um *ciclo Hamiltoniano*#footnote[
    curiosidade: um dos primeiros esquemas de zero-knowledge proofs foi baseado em
    achar um ciclo Hamiltoniano em um grafo gigante. Para mais detalhes, veja a #link("https://en.wikipedia.org/wiki/Zero-knowledge_proof#Hamiltonian_cycle_for_a_large_graph")[Wikipedia]
    e o #link("https://web.archive.org/web/20230103032937/http://euler.nmt.edu/~brian/students/pope.pdf")[paper original].
  ] é um ciclo que visita cada vértice uma só vez.
]

== #link(
  "https://en.wikipedia.org/wiki/Travelling_salesman_problem",
)[Problema do caixeiro-viajante]

O *problema do caixeiro-viajante* (PCV) é um problema que tenta determinar a
menor rota para percorrer uma série de cidades (visitando uma única vez cada uma
delas), retornando à cidade de origem.

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
    caption: "Problema do Cacheiro-Viajante",
  ) <travelling-salesman-problem>
]

#pagebreak()

#align(horizon)[
  Formulando em termos de grafos, o PCV é um problema de encontrar um ciclo
  Hamiltoniano tal que o custo do ciclo seja o menor possível.

  $ C = min_("ciclo") sum_(i=1)^n c_(i, i+1) $
]

== Parte Prática (C ou pseudocódigo)

#align(horizon)[
  - Encontrar um caminho Euleriano em C
  - Encontrar um ciclo Hamiltoniano em C
]

= Árvores

==

#align(horizon + center)[#image("images/trees_meme.jpg", width: 50%)]

== O que são Árvores?

Árvores são grafos não direcionados *conectados* e *acíclicos*.
Uma *árvore enraizada* é obtida ao escolher um vértice como raiz.

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
    caption: "Árvore",
  ) <tree>
]

#pagebreak()

#align(horizon)[
  - *Raiz* (árvore enraizada): vértice inicial escolhido. Uma árvore enraizada
    possui exatamente uma raiz.
  - *Folha* (árvore enraizada): vértice sem filhos (sem arestas saindo na
    representação direcionada).
  - *Nível*: distância da raiz.
  - *Altura*: nível máximo.
  - *Pai*: vértice adjacente com nível imediatamente menor (exceto para a raiz).
  - *Filho*: vértice adjacente com nível imediatamente maior.
  - *Ancestral*: vértice no caminho da raiz até um vértice.
  - *Descendente*: vértice na subárvore de um vértice.
]

== Subárvores

Subárvores são árvores que são subconjuntos de uma árvore.

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
    caption: "Subárvore",
  ) <subtree>
]

== Tipos de Árvores

#align(horizon + center)[
  #figure(
    raw-render(```dot
    digraph G {
      a -> b
      b -> c;
    }
    ```),
    caption: "Árvore Caminho",
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
    caption: "Árvore Estrela",
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
    caption: "Árvore Binária",
  ) <tree-binary>
]

== Árvores Balanceadas

Uma árvore é *balanceada* se a diferença de altura entre as subárvores esquerda
e direita é no máximo 1.

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
    caption: "Árvore Balanceada",
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
    caption: "Árvore Desbalanceada",
  ) <unbalanced-tree>
]

== Parte Prática (C ou pseudocódigo)

#align(horizon)[
  - Detectar se um grafo é uma árvore (ou seja, se é acíclico e conectado)
  - Detectar qual é o vértice raiz de uma árvore
]

= Interlúdio: Funções Polinomiais e Exponenciais

==

#align(horizon + center)[#image("images/polynomials_meme.jpg", width: 50%)]

== Funções Polinomiais

#align(horizon)[
  Uma função é polinomial se ela pode ser expressa na forma

  $ O(a_n dot n^k + a_(n-1) dot n^(k-1) + ... + a_1 dot n + a_0) $

  onde:

  - $n$ é o tamanho da entrada
  - $k$ é uma constante
  - $a_n, a_{n-1}, ..., a_1, a_0$ são coeficientes constantes
]

== Exemplos

#align(horizon)[
  - $n$
  - $n^2 + 3n + 2$
  - $4n^4 + n^3 + 2n^2 + 7$
  - $n^(100) + 500n^(50) + 3000$
]

== Notação $O$

#text(size: 15pt)[
  #align(horizon)[
    A notação $O$ é usada para descrever a complexidade de um algoritmo.

    Por exemplo, na função $n^3 + n^2 + 5n + 100$, a maior constante $k = 3$ assimptoticamente#footnote[
      a medida que algo tende ao infinito, ou seja $lim -> oo$.
    ]
    dominará o tempo de computação, então a complexidade é $O(n^3)$.

    Também em notação $O$, desconhecemos os coeficientes constantes. Por exemplo, $O(3n^2)$ é
    simplificado para $O(n^2)$ e
    $50 O(1)$ é simplificado para $O(1)$.
  ]
]

== Tipos de Complexidade Polinomial

#align(horizon)[
  - Constante: $O(1)$
  - Logarítmica: $O(log n)$
  - Linear: $O(n)$
  - Log-linear#footnote[também chamada de linearítimica.]: $O(n log n)$
  - Quadrática: $O(n^2)$
  - Cúbica: $O(n^3)$
  - Polinomial: $O(n^k)$
]

== Funções Exponenciais

#align(horizon)[
  Uma função tem crescimento exponencial quando pode ser limitada por

  $ O(c^n) $

  onde $c > 1$ é uma constante.

  Por exemplo, $O(2^n)$ é uma complexidade exponencial#footnote[
    o expoente depende de $n$.
  ].
]

== Tipos de Complexidade Exponencial

#align(horizon)[
  - Exponencial: $2^n$
  - Fatorial: $n!$
  - Superexponencial: $n^n$
  - Duplamente exponencial: $2^(2^n)$
]

= Complexidade Computacional

==

#align(horizon + center)[#image("images/big_o_meme.jpg", width: 45%)]

== Definição

#align(horizon)[
  Complexidadiade computacional de um algoritmo é o *número de operações
    computacionais (tais como operaçõe aritméticas, comparações,
    e acessos a memória) requerido para sua execução*.

  #v(1em)

  Este número claramente depende do tamanho e da natureza dos inputs.
]

== Complexidade Limitada

#align(horizon)[
  Se a complexidade de um algoritmo é limitada por uma função $f(n)$, onde $f$ é
  uma função polinomial de $n$ (tamanho do input), então o algoritmo é dito ter
  complexidade *polinomial*.

  #v(1em)

  Algoritmos com complexidade polinomial pertencem a classe $cal(P)$
]

== Classe $cal(P)$

#align(horizon)[
  Um *problema de decisão* é um problema que tem como resposta *sim* ou *não*. Tal
  problema pertence a classe $cal(P)$ se existe um algoritmo que solucione
  qualquer instância do problema em *complexidade polinomial*.
]

== Classe $cal(N P)$

#align(horizon)[
  Um problema de decisão pertence a classe $cal(N P)$ se existe um *algoritmo em
    tempo polinomial que _verifique_ a solução de um problema*.

  #v(1em)

  É trivial estabelecer que $cal(P) subset.eq cal(N P)$.
]

== Exemplos de Problemas $cal(P)$ e $cal(N P)$

#align(horizon)[
  - Classe $cal(P)$:
    - Algoritmos de ordenação
    - Algortimos de busca

  - Classe $cal(N P)$:
    - Problema da fatoração de inteiros em primos
    - Problema do caixeiro-viajante
]

== $cal(P)$ vs $cal(N P)$

#text(size: 9.8pt)[
  #table(
    columns: 3,
    align: left + horizon,
    table.header([], [*$cal(P)$*], [*$cal(N P)$*]),
    [*Solvabilidade*],
    [Solucionável eficientemente em tempo polinomial.],
    [Verificação eficiente, mas a solução pode não ser encontrada eficientemente.],

    [*Complexidade de Tempo*],
    [Algoritmos de tempo polinomial são conhecidos.],
    [Algoritmos de verificação eficiente são conhecidos, mas algoritmos eficientes
      para a solução não são garantidos.],

    [*Natureza das Soluções*],
    [As soluções podem ser encontradas eficientemente.],
    [As soluções, uma vez propostas, podem ser verificadas eficientemente.],

    [*Relação Conhecida*],
    [$cal(P)$ é um subconjunto de $cal(N P)$.],
    [Não se sabe se $cal(P) = cal(N P)$ (ou $cal(P) subset cal(N P)$).],
  )
]

== $cal(P)$ vs $cal(N P)$

#align(horizon + center)[#image("images/p_vs_np.png")]

== $cal(N P)$-completos

#align(horizon)[
  Um problema $cal(N P)$-completo é um problema $cal(N P)$ que é *tão difícil
    quanto qualquer outro problema em $cal(N P)$*. Se um problema $cal(N P)$-completo
  puder ser resolvido em tempo polinomial, então todos os problemas em $cal(N P)$-completo
  também podem.
]

== Satistifabilidade Booleana (SAT)

#align(horizon)[
  O problema de satisfatibilidade booleana (SAT) busca determinar se uma *fórmula
    proposicional pode ser tornada verdadeira* por meio de uma atribuição adequada
  ("solução") de valores de verdade para suas variáveis.

  $ (a and b and c) or (d and e and f) or (g and h and i) or (j and k and l) $

  onde $a, b, c, d, e, f, g, h, i, j, k, l$ são variáveis booleanas, e $and$ (`AND`)
  e $or$ (`OR`) são operadores booleanos.

  #pagebreak()

  #v(1em)

  Embora seja fácil verificar se uma determinada atribuição torna a fórmula
  verdadeira, não se conhece um método essencialmente mais rápido para encontrar
  uma atribuição satisfatória além de testar todas as atribuições sucessivamente.

  #v(1em)

  #link(
    "https://en.wikipedia.org/wiki/Cook%E2%80%93Levin_theorem",
  )[Cook e Levin provaram]
  que todo problema de fácil verificação pode ser resolvido tão rapidamente quanto
  o SAT, que, por isso, é NP-completo.
]

== $cal(N P)$-difíceis

#align(horizon)[
  Um problema $cal(N P)$-difícil é um problema para o qual todo problema em
  $cal(N P)$ pode ser reduzido em tempo polinomial.

  #v(1em)

  Problemas $cal(N P)$-difíceis não precisam ser problemas de decisão (nem estar
  em $cal(N P)$). Se um problema $cal(N P)$-difícil que também esteja em $cal(N P)$
  for resolvido em tempo polinomial, então $cal(P) = cal(N P)$.
]

== $cal(P)$ vs $cal(N P)$-completo e $cal(N P)$-difícil

#align(horizon + center)[#image("images/P_np_np-complete_np-hard.svg")]

== $cal(P)$ vs $cal(N P)$-completo e $cal(N P)$-difícil

#align(horizon)[
  - $cal(N P)$-completo:
    - Problema do Caixeiro Viajante na forma de decisão: "Existe um caminho de custo
      menor ou igual a X?"

  - $cal(N P)$-difícil:
    - Problema do Caixeiro Viajante na forma de otimização: "Qual é o caminho de custo
      mínimo?"
]

== Parte Prática (C)

#text(size: 14pt)[
  #align(horizon)[
    #link(
      "https://en.wikipedia.org/wiki/Knapsack_problem",
    )[*Problema da Mochila (_Knapsack Problem_)*]

    Você é um aventureiro e encontrou uma caverna cheia de tesouros. No entanto, sua
    mochila tem uma capacidade limitada e você precisa decidir quais itens levar
    para maximizar o valor total, sem exceder a capacidade da mochila.

    Você tem uma lista de `n` itens, onde cada item `i` tem:

    - *Valor*: $v[i]$ (em ouro)
    - *Peso*: $w[i]$ (em quilogramas)

    A capacidade da sua mochila é $W$ (em quilogramas).

    #pagebreak()

    - Escrever um algoritmo que determine o subconjunto de itens que maximiza o valor
      total na mochila sem exceder o peso total $W$.

    - Escrever um algoritmo que dado um certo input de itens e capacidade, determine
      se é possível colocar todos os itens na mochila.
  ]
]

= Identificando a Complexidade de Algoritmos

==

#align(horizon + center)[#image(
  "images/recursion_joker_debugging_meme.jpg",
  width: 80%,
)]

== Introdução

#align(horizon)[
  A análise de complexidade é fundamental para avaliar a *eficiência de
    algoritmos*. Ela nos ajuda a prever o comportamento de um algoritmo à medida que
  a entrada aumenta, o que é crucial para a *otimização* e escolha do *algoritmo
    certo* para uma aplicação específica.
]

== Notação Big-O

#align(horizon)[
  A notação Big-O ($O$) é usada para descrever o *pior caso do tempo de execução
    de um algoritmo* em função do *tamanho de entrada $n$*.
]

== Passos para Determinar a Complexidade

#align(horizon)[
  1. *Identifique as operações dominantes*: Concentre-se nas operações que são
    executadas repetidamente, como _loops_, recursões e chamadas de funções.

  #pagebreak()

  2. *Estime o número de vezes que essas operações são executadas*: Analise a
    profundidade e o número de iterações dos _loops_ e recursões.

  #pagebreak()

  3. *Ignore constantes e termos não dominantes*: Na notação Big-O, ignoramos
    constantes multiplicativas e termos de ordem inferior.

  #pagebreak()

  4. *Escolha a notação Big-O apropriada*: Use o resultado das etapas anteriores para
    identificar a complexidade Big-O correta.
]

#pagebreak()

#align(horizon + center)[#image(
  "images/recursion_world_burn_meme.jpeg",
  width: 80%,
)]

== Estruturas de Controle

- *Estruturas Sequenciais*: complexidade constante $O(1)$
- *Estruturas Condicionais*: complexidade constante $O(1)$
- *_Loops_*: complexidade linear $O(n)$
- *_Loop_ Aninhado*: complexidade quadrática $O(n^2)$
- *Recursão*:
  - *Linear*: complexidade linear $O(n)$
  - *Divisão e conquista*: tipicamente entre $O(log n)$ e $O(n log n)$ (depende do trabalho por nível)
  - *Binária*: complexidade $O(n log n)$
  - *Exponencial*: complexidade exponencial $O(2^n)$

== Estruturas Sequenciais

#align(horizon)[
  Estruturas de controle que não envolvem _loops_ ou recursão têm complexidade
  constante $O(1)$.

  ```c
  int x = 5;
  int y = 10;
  int z = x + y;  // O(1)
  ```
]

== Estruturas Condicionais

#align(horizon)[
  Condicionais simples, como `if`-`else`, não afetam a complexidade, mas a
  execução de blocos internos deve ser considerada.

  ```c
  if (x > y) {
      z = x - y;  // O(1)
  } else {
      z = y - x;  // O(1)
  }
  // Complexidade total: O(1)
  ```
]

== _Loops_

#align(horizon)[
  A complexidade de um _loop_ depende do número de iterações:

  - *_Loop_ Simples*:

    ```c
    for (int i = 0; i < n; i++) {
        // operação de O(1)
    }
    // Complexidade total: O(n)
    ```

  #pagebreak()

  - *_Loop_ Aninhado*:

    ```c
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            // operação de O(1)
        }
    }
    // complexidade total: O(n^2)
    ```

  #pagebreak()

  - *_Loop_ com incremento multiplicativo*:

    ```c
    for (int i = 1; i < n; i *= 2) {
        // operação de O(1)
    }
    // Complexidade total: O(log n)
    ```
]

== Recursão

#align(horizon + center)[#image(
  "images/recursion_joker_stackoverflow_meme.jpeg",
  width: 80%,
)]

#pagebreak()

#align(horizon)[
  A complexidade de algoritmos recursivos depende do número de chamadas recursivas
  e do tamanho da entrada em cada chamada.

  - *Recursão Linear*:

    ```c
    void recursao_linear(int n) {
        if (n == 0) return;
        // operação de O(1)
        recursao_linear(n-1);
    }
    // Complexidade total: O(n)
    ```

  #pagebreak()

  - *Recursão Divisória*:

    ```c
    void recursao_divisoria(int n) {
        if (n == 0) return;
        // operação de O(1)
        recursao_divisoria(n/2);
    }
    // Complexidade total: O(log n)
    ```

  #pagebreak()

  - *Recursão Binária (como _Merge Sort_)*:

    ```c
    void merge_sort(int arr[], int n) {
        if (n < 2) return;
        int mid = n / 2;
        merge_sort(arr, mid);
        merge_sort(arr + mid, n - mid);
        merge(arr, mid, n - mid);  // O(n)
    }
    // Complexidade total: O(n log n)
    ```

  #pagebreak()

  - *Recursão Exponencial*:
    ```c
    int fibonacci(int n) {
        if (n <= 1) return n;
        return fibonacci(n-1) + fibonacci(n-2);
    }
    // Complexidade total: O(2^n)
    ```
]

== Exemplo Prático - Busca Linear

#align(horizon)[
  ```c
  int busca_linear(int arr[], int n, int x) {
      for (int i = 0; i < n; i++) {
          if (arr[i] == x) return i;
      }
      return -1;
  }
  // Complexidade total: O(n)
  ```
]

== Exemplo Prático - Busca Binária

#align(horizon)[
  ```c
  int busca_binaria(int arr[], int n, int x) {
      int inicio = 0, fim = n - 1;
      while (inicio <= fim) {
          int meio = (inicio + fim) / 2;
          if (arr[meio] == x) return meio;
          if (arr[meio] < x) inicio = meio + 1;
          else fim = meio - 1;
      }
      return -1;
  }
  // Complexidade total: O(log n)
  ```
]

== Exemplo Prático - _Bubble Sort_

#text(size: 14pt)[
  #align(horizon)[
    ```c
    void bubble_sort(int arr[], int n) {
        for (int i = 0; i < n-1; i++) {
            for (int j = 0; j < n-i-1; j++) {
                if (arr[j] > arr[j+1]) {
                    // troca de elementos
                    int temp = arr[j];
                    arr[j] = arr[j+1];
                    arr[j+1] = temp;
                }
            }
        }
    }
    // Complexidade total: O(n^2)
    ```
  ]
]

== Parte Prática (C ou pseudocódigo)

#pagebreak()

#align(horizon)[
  - Implementar e determinar a complexidade de um algoritmo que conta o número de
    ocorrências de um elemento em uma matriz.

  - Descobrir uma maneira de reduzir a complexidade do cálculo de Fibonacci.
]

= Interlúdio: Analisando a Complexidade de Algoritmos com Código C

==

#align(horizon + center)[#image("images/programming_meme.jpg", width: 50%)]

#pagebreak()

#align(horizon)[
  #text(size: 12pt)[

    Revertendo um _array_:
    // Complexidade total: O(n)
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

    Checando se uma _string_ é um
    #link("https://en.wikipedia.org/wiki/Palindrome")[palíndromo]:
    // Complexidade total: O(n)
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

    Achando o maior elemento em um _array_
    que o maior elemento vem antes do menor:
    // Complexidade total: O(n)
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

    Ordenando um _array_ usando o algoritmo
    #link("https://en.wikipedia.org/wiki/Insertion_sort")[_insertion sort_]:
    // Complexidade total: O(n^2)
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

    Encontrar os elementos duplicados em um _array_:
    // Complexidade total: O(n^2)
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

    Computando a potência de um número:
    // Complexidade total: O(log n)
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

    Encontrar o
    #link(
      "https://en.wikipedia.org/wiki/Greatest_common_divisor",
    )[_greatest common divisor_]
    (minimo múltiplo comum) de dois números:
    // Complexidade total: O(log(min(a, b)))
    ```c
    int gcd(int a, int b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }
    ```

    #pagebreak()

    #link("https://en.wikipedia.org/wiki/Primality_test")[Teste de primalidade]
    (método ingênuo)
    // Complexidade total: O(sqrt(n))
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

    Achando o elemento majoritário (um elemento que aparece mais de $n/2$ vezes)
    usando o algoritmo de votação de
    #link("https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm")[Boyer-Moore's].
    // Complexidade total: O(n)
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

    Gerando o
    #link(
      "https://en.wikipedia.org/wiki/Pascal%27s_triangle",
    )[Triângulo de Pascal]
    // Complexidade total: O(n^2)
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

    Algoritmo de
    #link(
      "https://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane's_algorithm",
    )[Kadane]
    para encontrar a soma do _subarray_ máximo:
    // Complexidade total: O(n)
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

    Encontrar a maior subsequência comum
    #link("https://en.wikipedia.org/wiki/Longest_common_subsequence")[(_longest common subsequence_ -- LCS)]:
    // Complexidade total: O(n * m)
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

    Fundir duas _arrays_ ordeanadas:
    // Complexidade total: O(N log N)
    #text(size: 8pt)[
      ```c
      void merge_sorted_arrays(int A[], int B[], int m, int n, int C[]) {
          int i = 0, j = 0, k = 0;
          int N = m + n;  // N representa o tamanho combinado de A e B

          while (i < m && j < n) {
              if (A[i] <= B[j]) {
                  C[k++] = A[i++];
              } else {
                  C[k++] = B[j++];
              }
          }

          // Copia os elementos restantes de A, se houver
          while (i < m) {
              C[k++] = A[i++];
          }

          // Copia os elementos restantes de B, se houver
          while (j < n) {
              C[k++] = B[j++];
          }
      }
      ```
    ]

    #pagebreak()

    Esse algoritmo é uma
    #link("https://en.wikipedia.org/wiki/Fast_inverse_square_root")[maneira rápida de calcular a raiz quadrada inversa],
    $1 / sqrt(x)$, que ficou famoso por seu uso no jogo Quake III Arena por
    #link("https://en.wikipedia.org/wiki/John_Carmack")[John Carmack]. O método usa
    uma aproximação inteligente e uma única iteração do of #link("https://en.wikipedia.org/wiki/Newton%27s_method")[método de Newton]
    para calcular a raiz quadrada inversa de um número.

    // Complexidade total: O(1)
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

= Algoritmos de Busca

==

#align(horizon + center)[#image(
  "images/search_algorithms_meme.png",
  width: 100%,
)]

== O que é um Algoritmo de Busca?

#align(horizon)[
  Um *algoritmo de busca* é uma sequência de instruções que permite encontrar um
  determinado elemento dentro de uma estrutura de dados. É fundamental em ciência da
  computação, pois otimiza o acesso e a manipulação de dados.
]

== Por que Estudar Algoritmos de Busca?

#align(horizon)[
  - *Eficiência*: Algoritmos de busca eficientes economizam tempo e recursos.
  - *Fundamentos*: São a base para algoritmos mais complexos e estruturas de dados.
  - *Aplicações Práticas*: Usados em bancos de dados, sistemas operacionais,
    inteligência artificial, entre outros.
]

== Tipos de Algoritmos de Busca

#align(horizon)[
  - *Busca Linear* (_Linear Search_)
  - *Busca Binária* (_Binary Search_)
  - *Busca em Grafos*:
    - *Busca em Largura* (_Breadth-First Search - BFS_)
    - *Busca em Profundidade* (_Depth-First Search - DFS_)
]

== Busca Linear

=== Conceito

#align(horizon)[
  A *busca linear* é o algoritmo mais simples de busca. Ela percorre cada elemento
  da estrutura de dados até encontrar o elemento desejado ou até o final da estrutura.
]

#pagebreak()

=== Características da Busca Linear

#align(horizon)[
  - *Simples de Implementar*
  - *_Não_ Requer Estrutura Ordenada*
  - *Complexidade de Tempo*: $O(n)$, onde $n$ é o número de elementos.
]

#pagebreak()

=== Pseudoalgoritmo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [Busca Linear],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[dado uma lista $A$ de $n$ elementos com valores $A_0, dots A_(n-1)$, e valor-alvo $T$:],
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

=== Exemplo em C

#align(horizon)[
  #text(size: 14pt)[
    ```c
    int busca_linear(int arr[], int n, int x) {
        for (int i = 0; i < n; i++) {
            if (arr[i] == x)
                // Elemento encontrado na posição i
                return i;
        }
        return -1;  // Elemento não encontrado
    }
    ```
  ]
]

#pagebreak()

=== Análise da Complexidade

#align(horizon)[
  - *Melhor Caso*: O elemento está na primeira posição; $O(1)$.
  - *Pior Caso*: O elemento está na última posição ou não está presente; $O(n)$.
  - *Caso Médio*: Em média, percorre metade dos elementos; $1/2 O(n) = O(n)$.
]

== Busca Binária

#align(horizon)[
  A *busca binária* é um algoritmo eficiente para encontrar um elemento em uma lista
  ordenada, reduzindo o espaço de busca pela metade a cada iteração.
]

#pagebreak()

=== Características da Busca Binária

#align(horizon)[
  - *Requer Estrutura Ordenada*
  - *Complexidade de Tempo*: $O(log n)$
  - *Mais Eficiente que a Busca Linear em Grandes Conjuntos de Dados*
]

#pagebreak()

=== Pseudoalgoritmo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [Busca Binária],
    text(size: 9pt)[
      #pseudocode-list(
        title: smallcaps[dado uma lista _ordenada_ $A$ de $n$ elementos com valores $A_0, dots A_(n-1)$, e valor-alvo $T$:],
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

=== Exemplo em C

#align(horizon)[
  #text(size: 12pt)[
    ```c
    int busca_binaria(int arr[], int n, int x) {
        int inicio = 0, fim = n - 1;
        while (inicio <= fim) {
            int meio = inicio + (fim - inicio) / 2;
            if (arr[meio] == x)
            // Elemento encontrado
                return meio;
            if (arr[meio] < x)
                inicio = meio + 1;
            else
                fim = meio - 1;
        }
        // Elemento não encontrado
        return -1;
    }
    ```
  ]
]

#pagebreak()

=== Análise da Complexidade

#align(horizon)[
  - A cada iteração, o algoritmo reduz o espaço de busca pela metade.
  - *Complexidade de Tempo*: $O(log n)$
  - *Eficiência*: Muito mais rápido que a busca linear em grandes conjuntos de dados.
]

#pagebreak()

=== E se tivermos $k > 1$ pivôs?

#align(horizon)[
  #text(size: 14pt)[
    Em cada passo:

    - Divide a _array_ em $k+1$ partições
    - O espaço de busca é reduzido para $n / (k+1)$
    - As comparações aumentam de $1$ para $k$

    Total de comparações:

    - Total de iterações: $log_(k+1) n$
    - Total de comparações: $k dot.c log_(k+1) n$

    Complexidade:

    - $O (k dot.c (log n) / (log(k+1))) = O(log n)$
  ]
]

== Busca em Grafos

#pagebreak()

=== Tipos de Busca em Grafos

#align(horizon)[
  - *Busca em Largura* (_Breadth-First Search - BFS_)
  - *Busca em Profundidade* (_Depth-First Search - DFS_)
]

#pagebreak()

=== Aplicações

#align(horizon)[
  - *Encontrar Caminhos*: Entre dois vértices em um grafo.
  - *Verificar Conectividade*: Se todos os vértices são alcançáveis.
  - *Detecção de Ciclos*: Em grafos direcionados e não direcionados.
]

== Busca em Largura (BFS)

#align(horizon)[
  Busca em Largura (_Breadth-First Search_) é um algoritmo de busca em grafos que explora todos os vértices
]

#pagebreak()

=== Conceito

#align(horizon)[
  A *busca em largura* explora um grafo visitando todos os vértices na mesma camada
  de distância da origem antes de passar para a próxima camada.
]

#pagebreak()

=== Características da BFS

#align(horizon)[
  - *Usa Fila (_Queue_)*
  - *Garante o Caminho Mais Curto em Grafos Não Ponderados*
  - *Complexidade de Tempo*: $O(V + E)$, onde $V$ é o número de vértices e $E$ é o número de arestas.
]

#pagebreak()

=== Pseudoalgoritmo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [Busca em Largura],
    text(size: 8pt)[
      #pseudocode-list(
        title: smallcaps[dado um grafo $G$, um vértice raiz _root_, e valor-alvo $T$:],
      )[
        + $Q := "queue"$
        + _root_.explored $:= "true"$
        + $Q$.enqueue(_root_)
        + *while* $!Q$.empty():
          + $v := Q$.dequeue()
          + *if* $v = T$
            + *return* $v$
          + *for* todas as arestas de $v$ para $w$ *in* $G$.adjacentEdges(v):
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

=== Exemplo em C

#align(horizon)[
  #text(size: 7pt)[
    ```c
    int bfs(int grafo[][MAX], int inicio, int n, int alvo) {
        int visitado[MAX] = {0};
        int fila[MAX], frente = 0, traseira = 0;

        visitado[inicio] = 1;
        fila[traseira++] = inicio;

        while (frente < traseira) {
            int atual = fila[frente++];
            if (atual == alvo)
                return atual;

            for (int i = 0; i < n; i++) {
                if (grafo[atual][i] && !visitado[i]) {
                    visitado[i] = 1;
                    fila[traseira++] = i;
                }
            }
        }
        return -1;
    }
    ```
  ]
]

#pagebreak()

=== Ilustração da BFS

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph BFS {
        rankdir=TB;
        node [shape=circle, style=filled, color=lightgrey];

        // Definição dos nós e suas labels
        A [label="1"];
        B [label="2"];
        C [label="3"];
        D [label="4"];
        E [label="5"];
        F [label="6"];
        G [label="7"];
        H [label="8"];

        // Definição das arestas
        A -> {B; C};
        B -> {D; E};
        C -> {F; G};
        E -> H;
      }
      ```,
      width: 35%,
    ),
    caption: "Ilustração da BFS em um digrafo com os vértices numerados pela ordem de visitação",
  )
]

== Busca em Profundidade (DFS)

#align(horizon)[
  Busca em Profundidade (_Depth-First Search_) é um algoritmo de busca em grafos que explora todos os vértices
]

#pagebreak()

=== Conceito

#align(horizon)[
  A *busca em profundidade* explora o grafo o mais profundo possível antes de retroceder.
]

#pagebreak()

=== Características da DFS

#align(horizon)[
  - *Usa Pilha (_Stack_)* (pode ser implementada recursivamente)
  - *Não Garante o Caminho Mais Curto*
  - *Complexidade de Tempo*: $O(V + E)$
]

#pagebreak()

=== Pseudoalgoritmo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [Busca em Profundidade],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[dado um grafo $G$, um vértice $v$, e valor-alvo $T$:],
      )[
        + $v$.discovered $:= "true"$
        + *if* $v = T$
          + *return* $v$
        + *for* todas as arestas de $v$ para $w$ *in* $G$.adjacentEdges(v):
          + *if* $!w$.discovered:
            + $r := "DFS"(G, w)$
            + *if* $r \neq "null"$:
              + *return* $r$
        + *return* _null_
      ]
    ],
  ) <depth-first-search>
]

#pagebreak()

=== Exemplo em C (Recursivo)

#align(horizon)[
  #text(size: 13pt)[
    ```c
    int dfs(int grafo[][MAX], int atual, int visitado[], int n, int alvo) {
        if (atual == alvo)
            return atual;
        visitado[atual] = 1;

        for (int i = 0; i < n; i++) {
            if (grafo[atual][i] && !visitado[i]) {
                int encontrado = dfs(grafo, i, visitado, n, alvo);
                if (encontrado != -1)
                    return encontrado;
            }
        }
        return -1;
    }
    ```
  ]
]

#pagebreak()

=== Ilustração da DFS

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph DFS {
        rankdir=TB;
        node [shape=circle, style=filled, color=lightgrey];

        // Definição dos nós e suas labels
        A [label="1"];
        B [label="2"];
        C [label="6"];
        D [label="3"];
        E [label="4"];
        F [label="7"];
        G [label="8"];
        H [label="5"];

        // Definição das arestas
        A -> {B; C};
        B -> {D; E};
        C -> {F; G};
        E -> H;
      }
      ```,
      width: 35%,
    ),
    caption: "Ilustração da DFS em um digrafo com os vértices numerados pela ordem de visitação",
  )
]

== Comparação entre BFS e DFS

#align(horizon)[
  #text(size: 12pt)[
    #table(
      columns: 2,
      align: left + horizon,
      table.header(
        [*Característica*], [*BFS*],
        [*DFS*],
      ),
      [*Estrutura de Dados*], [Fila (_Queue_)],
      [Pilha (_Stack_)], [*Uso de Memória*],
      [Maior (guarda todos os vizinhos)], [Menor (apenas caminho atual)],
      [*Caminho Mais Curto*], [Sim (em grafos não ponderados)],
      [Não necessariamente], [*Completo*],
      [Sim], [Sim],
      [*Aplicações*], [Caminho mais curto, nível dos nós],
      [Detecção de ciclos, ordenação topológica],
    )
  ]
]

= Algoritmos de Ordenação

==

#align(horizon + center)[
  #image(
    "images/sorting_algorithms_meme.jpg",
    width: 60%,
  )
]

== Introdução

#align(horizon)[
  *Algoritmos de ordenação* são algoritmos que colocam elementos de uma lista em uma certa ordem.
  As ordens mais frequentemente usadas são ordem numérica e ordem lexicográfica.

  A ordenação é importante porque:

  - Organiza os dados para torná-los mais utilizáveis.
  - Otimiza a eficiência de outros algoritmos que requerem dados ordenados.
  - Facilita a busca e a representação de dados.
]

== Tipos de Algoritmos de Ordenação

#align(horizon)[
  #text(size: 14pt)[
    Os algoritmos de ordenação podem ser classificados com base em vários fatores:

    - *Baseados em comparação vs. Não-comparação*: Se os elementos são comparados
      para determinar sua ordem.
    - *Estável vs. Instável*: Se elementos equivalentes mantêm sua ordem relativa
      original.
    - *Complexidade de Tempo*: Como o tempo de execução aumenta com o número de elementos.
    - *Complexidade de Espaço*: A quantidade de memória necessária além dos dados de entrada.
  ]
]

== Estável vs. Instável

#align(horizon + center)[
  #image(
    "images/sort_stable_vs_unstable.png",
    width: 100%,
  )
]

== Algoritmos de Ordenação Comuns

#align(horizon)[
  - *_Bubble Sort_*
  - *_Selection Sort_*
  - *_Insertion Sort_*
  - *_Merge Sort_*
  - *_Quick Sort_*
  - *_Heap Sort_*
  - *_Counting Sort_*
  - *_Radix Sort_*
  - *_Bucket Sort_*
]

== _Bubble Sort_

#align(horizon)[
  *_Bubble Sort_* é um algoritmo simples baseado em comparação onde
  cada par de elementos adjacentes é comparado,
  e os elementos são trocados se estiverem na ordem errada.
  Este processo é repetido até que nenhuma troca seja necessária.

  É chamado de "bolha" porque elementos menores "sobem" para
  o topo da lista.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Compare cada par de itens adjacentes.
  2. Troque-os se estiverem na ordem errada.
  3. Repita os passos 1 e 2 para todos os elementos.
  4. Continue o processo até que uma passagem complete sem trocas.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Bubble Sort_],
    text(size: 12pt)[
      #pseudocode-list(title: smallcaps[Dado um _array_ $A$ de $n$ elementos:])[
        + *para* $i$ de $0$ até $n - 1$:
          + *para* $j$ de $0$ até $n - i - 1$:
            + *se* $A[j] > A[j + 1]$:
              + troque $A[j]$ e $A[j + 1]$
      ]
    ],
  ) <bubble-sort>
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 13pt)[
    ```c
    void bubble_sort(int arr[], int n) {
        for (int i = 0; i < n - 1; i++) {
            // Os últimos i elementos já estão
            // na posição correta
            for (int j = 0; j < n - i - 1; j++) {
                if (arr[j] > arr[j + 1]) {
                    // Troca arr[j] e arr[j + 1]
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

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*:
    - Melhor Caso: $O(n)$ (quando o _array_ já está ordenado)
    - Caso Médio: $O(n^2)$
    - Pior Caso: $O(n^2)$
  - *Complexidade de Espaço*: $O(1)$ (ordenamento _in-place_)
  - *Estabilidade*: Estável (elementos iguais mantêm sua ordem relativa)
]

== _Selection Sort_

#align(horizon)[
  *_Selection Sort_* divide a lista de entrada em duas partes:
  uma sublista de elementos ordenados construída da esquerda para a direita,
  e uma sublista dos elementos restantes não ordenados.
  Repetidamente seleciona o menor (ou maior) elemento da sublista não ordenada,
  trocando-o com o elemento não ordenado mais à esquerda.

  O processo continua movendo os limites da sublista um elemento à direita.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Defina o primeiro elemento não ordenado como o mínimo.
  2. Compare este mínimo com o próximo elemento.
  3. Se o próximo elemento for menor, defina-o como o novo mínimo.
  4. Continue até o final do _array_.
  5. Troque o mínimo com a primeira posição não ordenada.
  6. Mova o limite um elemento à direita.
  7. Repita até que o _array_ esteja ordenado.
]

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Selection Sort_],
    text(size: 12pt)[
      #pseudocode-list(title: smallcaps[Dado um _array_ $A$ de $n$ elementos:])[
        + *para* $i$ de $0$ até $n - 1$:
          + $"minIdx" := i$
          + *para* $j$ de $i + 1$ até $n$:
            + *se* $A[j] < A["minIdx"]$:
              + minIdx := $j$
          + troque $A[i]$ e $A["minIdx"]$
      ]
    ],
  ) <selection-sort>
]

#pagebreak()

=== Exemplo em C

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
            // Troca o menor elemento encontrado com
            // o primeiro elemento
            int temp = arr[min_idx];
            arr[min_idx] = arr[i];
            arr[i] = temp;
        }
    }
    ```
  ]
]

#pagebreak()

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*:
    - Melhor Caso: $O(n^2)$
    - Caso Médio: $O(n^2)$
    - Pior Caso: $O(n^2)$
  - *Complexidade de Espaço*: $O(1)$
  - *Estabilidade*: Instável (elementos iguais podem não manter sua ordem relativa)
]

== _Insertion Sort_

#align(horizon)[
  *_Insertion Sort_* constrói o _array_ ordenado final um elemento de cada vez.
  Assume que o primeiro elemento já está ordenado,
  então insere cada elemento subsequente na posição correta em relação
  à porção ordenada.

  É semelhante à forma como as pessoas organizam cartas em um jogo de baralho.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Comece pelo segundo elemento (índice $1$).
  2. Compare o elemento atual com os elementos na porção ordenada.
  3. Desloque todos os elementos maiores na porção ordenada uma posição à direita.
  4. Insira o elemento atual em sua posição correta.
  5. Repita até que o _array_ esteja ordenado.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Insertion Sort_],
    text(size: 12pt)[
      #pseudocode-list(title: smallcaps[Dado um _array_ $A$ de $n$ elementos:])[
        + *para* $i$ de $1$ até $n - 1$:
          + $"key" := A[i]$
          + $j := i - 1$
          + *enquanto* $j >= 0$ *e* $A[j] > "key"$:
            + $A[j + 1] := A[j]$
            + $j := j - 1$
          + $A[j + 1] := "key"$
      ]
    ],
  ) <insertion-sort>
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 11pt)[
    ```c
    void insertion_sort(int arr[], int n) {
        for (int i = 1; i < n; i++) {
            int key = arr[i];
            int j = i - 1;

            // Move elementos de arr[0..i-1],
            // que são maiores que key,
            // para uma posição à frente
            // de sua posição atual
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

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*:
    - Melhor Caso: $O(n)$ (quando o _array_ já está ordenado)
    - Caso Médio: $O(n^2)$
    - Pior Caso: $O(n^2)$
  - *Complexidade de Espaço*: $O(1)$
  - *Estabilidade*: Estável
]

== _Merge Sort_

#align(horizon)[
  *_Merge Sort_* é um algoritmo de divisão e conquista que divide o _array_ ao meio,
  ordena cada metade e, em seguida, mescla-as de volta.

  É eficiente e tem um tempo de execução garantido de $O(n log n)$.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Se o _array_ tiver comprimento $0$ ou $1$, já está ordenado.
  2. Divida o _array_ em duas metades.
  3. Ordene recursivamente cada metade.
  4. Mescle as duas metades ordenadas em um _array_ ordenado.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Merge Sort_],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Função merge_sort(_array_ A, left, right):],
      )[
        + *se* $"left" < "right"$:
          + $"mid" := ("left" + "right") / 2$
          + $"merge_sort"(A, "left", "mid")$
          + $"merge_sort"(A, "mid" + 1, "right")$
          + $"merge"(A, "left", "mid", "right")$
      ]
    ],
  ) <merge-sort>
]

#pagebreak()

=== Função `merge`

#align(horizon)[
  A função `merge` combina duas _subarrays_ ordenadas em um _array_ ordenado.

  - _Subarray_ esquerda: $A["left".."mid"]$
  - _Subarray_ direita: $A["mid"+1.."right"]$
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 6pt)[
    ```c
    void merge(int arr[], int l, int m, int r) {
        int n1 = m - l + 1;
        int n2 = r - m;
        // Cria arrays temporários
        int L[n1], R[n2];
        // Copia os dados para os arrays temporários L[] e R[]
        for (int i = 0; i < n1; i++)
            L[i] = arr[l + i];
        for (int j = 0; j < n2; j++)
            R[j] = arr[m + 1 + j];
        // Mescla os arrays temporários de volta em arr[l..r]
        int i = 0, j = 0, k = l;
        while (i < n1 && j < n2) {
            if (L[i] <= R[j]) {
                arr[k++] = L[i++];
            } else {
                arr[k++] = R[j++];
            }
        }
        // Copia os elementos restantes de L[], se houver
        while (i < n1)
            arr[k++] = L[i++];
        // Copia os elementos restantes de R[], se houver
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

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*:
    - Melhor Caso: $O(n log n)$
    - Caso Médio: $O(n log n)$
    - Pior Caso: $O(n log n)$
  - *Complexidade de Espaço*: $O(n)$ (devido aos _arrays_ auxiliares)
  - *Estabilidade*: Estável
]

== _Quick Sort_

#align(horizon)[
  *_Quick Sort_* é um algoritmo de divisão e conquista que seleciona um elemento *pivô*
  e particiona o _array_ em torno do pivô,
  de modo que elementos menores que o pivô fiquem à esquerda,
  e elementos maiores fiquem à direita.

  Em seguida, ordena recursivamente os _subarrays_ em ambos os lados do pivô.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Escolha um elemento pivô.
  2. Particione o _array_ em dois _subarrays_:
    - Elementos menores que o pivô.
    - Elementos maiores que o pivô.
  3. Aplique recursivamente os passos acima aos _subarrays_.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Quick Sort_],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Função quick_sort(_array_ A, low, high):],
      )[
        + *se* $"low" < "high"$:
          + $pi := "partition"(A, "low", "high")$
          + $"quick_sort"(A, "low", pi - 1)$
          + $"quick_sort"(A, pi + 1, "high")$
      ]
    ],
  ) <quick-sort>
]

#pagebreak()

=== Função `partition`

#align(horizon)[
  A função `partition` rearranja o _array_ de modo que:

  - Todos os elementos menores que o pivô venham antes dele.
  - Todos os elementos maiores que o pivô venham após ele.
  - O pivô está em sua posição final ordenada.

  Ela retorna o índice do pivô.
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 9pt)[
    ```c
    int partition(int arr[], int low, int high) {
        int pivot = arr[high]; // pivô
        int i = low - 1;       // Índice do menor elemento

        for (int j = low; j <= high - 1; j++) {
            // Se o elemento atual é menor ou igual ao pivô
            if (arr[j] <= pivot) {
                i++;    // incrementa o índice do menor elemento
                int temp = arr[i];
                arr[i] = arr[j];
                arr[j] = temp;
            }
        }
        // Troca arr[i + 1] e arr[high] (ou pivô)
        int temp = arr[i + 1];
        arr[i + 1] = arr[high];
        arr[high] = temp;

        return i + 1;
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

            // Separa a ordenação dos elementos antes
            // e depois da partição
            quick_sort(arr, low, pi - 1);
            quick_sort(arr, pi + 1, high);
        }
    }
    ```
  ]
]

#pagebreak()

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*:
    - Melhor Caso: $O(n log n)$
    - Caso Médio: $O(n log n)$
    - Pior Caso: $O(n^2)$
      (quando o menor ou maior elemento é sempre escolhido como pivô)
  - *Complexidade de Espaço*: $O(log n)$ (devido às chamadas recursivas)
  - *Estabilidade*: Instável
]

== _Heap Sort_

#align(horizon)[
  *_Heap Sort_* envolve construir um `max heap` a partir dos dados de entrada,
  e então repetidamente extrair o elemento máximo do _heap_
  e reconstruir o _heap_.

  Utiliza as propriedades de uma estrutura de dados _heap_ para ordenar elementos.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Construa um `max heap` a partir dos dados de entrada.
  2. Troque a raiz (valor máximo) do _heap_ com o último elemento.
  3. Reduza o tamanho do _heap_ em um.
  4. `heapify` o elemento raiz para obter novamente o maior elemento na raiz.
  5. Repita os passos 2 a 4 até que o tamanho do _heap_ seja maior que 1.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Heap Sort_],
    text(size: 12pt)[
      #pseudocode-list(title: smallcaps[Função heap_sort(_array_ A, n):])[
        + *para* $i$ de $n / 2 - 1$ até $0$:
          + heapify(A, n, i)
        + *para* $i$ de $n - 1$ até $0$:
          + troque $A[0]$ e $A[i]$
          + heapify(A, i, 0)
      ]
    ],
  ) <heap-sort>
]

#pagebreak()

=== Função `heapify`

#align(horizon)[
  A função `heapify` garante que a subárvore enraizada no índice $i$ satisfaça
  a propriedade de _max heap_.

  - Se os filhos do nó $i$ forem _max heaps_
    mas o nó $i$ pode ser menor que seus filhos.
  - Troque o nó $i$ com seu maior filho.
  - Recursivamente aplique `heapify` na subárvore afetada.
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 9pt)[
    ```c
    void heapify(int arr[], int n, int i) {
        int largest = i;   // Inicializa largest como raiz
        int l = 2 * i + 1; // esquerda = 2*i + 1
        int r = 2 * i + 2; // direita = 2*i + 2
        // Se o filho esquerdo é maior que a raiz
        if (l < n && arr[l] > arr[largest])
            largest = l;
        // Se o filho direito é maior que largest até agora
        if (r < n && arr[r] > arr[largest])
            largest = r;
        // Se largest não é a raiz
        if (largest != i) {
            int swap = arr[i];
            arr[i] = arr[largest];
            arr[largest] = swap;
            // Recursivamente heapify a subárvore afetada
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
        // Constrói o heap (rearranja o array)
        for (int i = n / 2 - 1; i >= 0; i--)
            heapify(arr, n, i);

        // Um a um extrai um elemento do heap
        for (int i = n - 1; i >= 0; i--) {
            // Move a raiz atual para o fim
            int temp = arr[0];
            arr[0] = arr[i];
            arr[i] = temp;

            // chama max heapify no heap reduzido
            heapify(arr, i, 0);
        }
    }
    ```
  ]
]

#pagebreak()

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*:
    - Melhor Caso: $O(n log n)$
    - Caso Médio: $O(n log n)$
    - Pior Caso: $O(n log n)$
  - *Complexidade de Espaço*: $O(1)$
  - *Estabilidade*: Instável
]

== _Counting Sort_

#align(horizon)[
  *_Counting Sort_* é um algoritmo de ordenação de inteiros que opera contando
  o número de objetos que possuem valores de chave distintos (tipo um _hashing_).
  Não é uma ordenação por comparação e tem um tempo de execução de $O(n + k)$ onde $k$
  é o intervalo dos dados de entrada.

  É eficiente quando o intervalo dos dados de entrada não é significativamente maior
  que o número de objetos a serem ordenados.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Encontre o elemento máximo no _array_.
  2. Inicialize um _array_ de contagem de tamanho ($max + 1$) com zeros.
  3. Armazene a contagem de cada elemento em seu índice respectivo.
  4. Modifique o _array_ de contagem adicionando as contagens anteriores.
  5. Construa o _array_ de saída colocando os elementos em suas posições corretas.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Counting Sort_],
    text(size: 10pt)[
      #pseudocode-list(title: smallcaps[Dado um _array_ $A$ de $n$ elementos:])[
        + $max := "find_max"(A)$
        + inicialize o _array_ de contagem $C[0..max]$
        + *para* cada elemento em $A$:
          + $C["element"] := C["element"] + 1$
        + *para* $i$ de $1$ até $max$:
          + $C[i] := C[i] + C[i - 1]$
        + *para* $i$ de $n - 1$ até $0$:
          + $"output"[C[A[i]] - 1] := A[i]$
          + $C[A[i]] := C[A[i]] - 1$
        + copie o _array_ de saída para $A$
      ]
    ],
  ) <counting-sort>
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 5.8pt)[
    ```c
    void counting_sort(int arr[], int n) {
        int output[n];
        int max = arr[0];
        // Encontra o maior elemento no array
        for (int i = 1; i < n; i++) {
            if (arr[i] > max)
                max = arr[i];
        }
        int count[max + 1];
        // Inicializa o array de contagem com zeros
        for (int i = 0; i <= max; ++i)
            count[i] = 0;
        // Armazena a contagem de cada elemento
        for (int i = 0; i < n; i++)
            count[arr[i]]++;
        // Armazena a contagem cumulativa
        for (int i = 1; i <= max; i++)
            count[i] += count[i - 1];
        // Encontra o índice de cada elemento do array original no array de contagem e
        // coloca os elementos no array de saída
        for (int i = n - 1; i >= 0; i--) {
            output[count[arr[i]] - 1] = arr[i];
            count[arr[i]]--;
        }
        // Copia os elementos ordenados para o array original
        for (int i = 0; i < n; i++)
            arr[i] = output[i];
    }
    ```
  ]
]

#pagebreak()

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*: $O(n + k)$ onde $k$ é o intervalo dos dados de entrada.
  - *Complexidade de Espaço*: $O(n + k)$
  - *Estabilidade*: Estável
  - *Limitação*: Funciona apenas com inteiros e quando $k$ não é significativamente maior que $n$.
]

== _Radix Sort_

#align(horizon)[
  *_Radix Sort_* é um algoritmo de ordenação não por comparação que ordena dados
  com chaves inteiras agrupando chaves por dígitos individuais que compartilham
  a mesma posição significativa e valor.

  Ele usa _Counting Sort_ como uma sub-rotina para ordenar elementos.
]

#pagebreak()

=== Passos do Algoritmo

#align(horizon)[
  1. Encontre o número máximo para saber o número de dígitos.
  2. Faça `counting_sort` para cada dígito, começando do dígito menos significativo
    para o mais significativo.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [Radix Sort],
    text(size: 12pt)[
      #pseudocode-list(title: smallcaps[Função radix_sort(_array_ A, n):])[
        + $max := "find_max"(A)$
        + *para* $"exp" := 1$; $floor("max" / "exp") > 0$; $"exp" *= 10$:
          + $"counting_sort_by_digit"(A, n, "exp")$
      ]
    ],
  ) <radix-sort>
]

#pagebreak()

=== Função `counting_sort_by_digit`

#align(horizon)[
  A função `counting_sort_by_digit` ordena o _array_ de acordo
  com o dígito representado por `exp` (expoente).

  - Para $"exp" = 1$, ordena de acordo com o dígito menos significativo.
  - Para $"exp" = 10$, ordena de acordo com o segundo dígito menos significativo.
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 9pt)[
    ```c
    void counting_sort_by_digit(int arr[], int n, int exp) {
        int output[n];
        int count[10] = {0};
        // Armazena a contagem de ocorrências em count[]
        for (int i = 0; i < n; i++)
            count[(arr[i] / exp) % 10]++;
        // Altera count[i] para que count[i] contenha a posição real
        // deste dígito no array de saída
        for (int i = 1; i < 10; i++)
            count[i] += count[i - 1];
        // Constrói o array de saída
        for (int i = n - 1; i >= 0; i--) {
            output[count[(arr[i] / exp) % 10] - 1] = arr[i];
            count[(arr[i] / exp) % 10]--;
        }
        // Copia o array de saída para arr[], de modo que arr[] agora
        // contenha números ordenados de acordo com o dígito atual
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
        // Encontra o número máximo para saber o número de dígitos
        int max = arr[0];
        for (int i = 1; i < n; i++)
            if (arr[i] > max)
                max = arr[i];

        // Faz counting sort para cada dígito
        for (int exp = 1; max / exp > 0; exp *= 10)
            counting_sort_by_digit(arr, n, exp);
    }
    ```
  ]
]

#pagebreak()

=== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo*: $O(d dot.c (n + b))$
    - $d$: Número de dígitos
    - $b$: Base do sistema numérico (10 para decimal)
  - *Complexidade de Espaço*: $O(n + b)$
  - *Estabilidade*: Estável
  - *Limitação*: Funciona melhor quando $d$ não é significativamente grande.
]

== Comparação - Algos de Ordenação

#align(horizon)[
  #text(size: 9pt)[
    #table(
      columns: 7,
      align: left + horizon,
      table.header(
        [*Algoritmo*],
        [*Melhor Caso*],
        [*Caso Médio*],
        [*Pior Caso*],
        [*Espaço*],
        [*Estável*],
        [*Método*],
      ),

      [*_Bubble Sort_*],
      [$O(n)$],
      [$O(n^2)$],
      [$O(n^2)$],
      [$O(1)$],
      [Sim],
      [Troca],

      [*_Selection Sort_*],
      [$O(n^2)$],
      [$O(n^2)$],
      [$O(n^2)$],
      [$O(1)$],
      [Não],
      [Seleção],

      [*_Insertion Sort_*],
      [$O(n)$],
      [$O(n^2)$],
      [$O(n^2)$],
      [$O(1)$],
      [Sim],
      [Inserção],

      [*_Merge Sort_*],
      [$O(n log n)$],
      [$O(n log n)$],
      [$O(n log n)$],
      [$O(n)$],
      [Sim],
      [Intercalação],

      [*_Quick Sort_*],
      [$O(n log n)$],
      [$O(n log n)$],
      [$O(n^2)$],
      [$O(log n)$],
      [Não],
      [Partição],

      [*_Heap Sort_*],
      [$O(n log n)$],
      [$O(n log n)$],
      [$O(n log n)$],
      [$O(1)$],
      [Não],
      [Seleção],

      [*_Counting Sort_*],
      [$O(n + k)$],
      [$O(n + k)$],
      [$O(n + k)$],
      [$O(n + k)$],
      [Sim],
      [Contagem],

      [*_Radix Sort_*],
      [$O(n k)$],
      [$O(n k)$],
      [$O(n k)$],
      [$O(n + k)$],
      [Sim],
      [Dígito],
    )
  ]
]

== Seção Prática (C ou pseudocódigo)

#align(horizon)[
  - *Tarefa*: Implemente um algoritmo de ordenação de sua escolha
    e analise sua complexidade de tempo e espaço.
  - *Tarefa*: Modifique o algoritmo _Quick Sort_ para usar um pivô aleatório
    para melhorar o desempenho em arrays já ordenados.
  - *Tarefa*: Implemente uma versão estável da _Selection Sort_.
]

= Recursão

==

#align(horizon + center)[
  #image(
    "images/recursion_meme.jpg",
    width: 50%,
  )
]

== O que é Recursão?

#align(horizon)[
  *Recursão* é uma técnica de programação onde uma função chama a si mesma
  para resolver um problema menor do mesmo tipo _até_ atingir um caso base.
  É uma forma de *dividir um problema complexo em subproblemas mais simples e manejáveis*.
]

== Como Funciona?

#align(horizon)[
  1. *Caso Base*: Define quando a função recursiva deve parar de chamar a si mesma.
    É a condição de parada.

  2. *Chamada Recursiva*: A função chama a si mesma com uma entrada modificada
    que a aproxima do caso base.

  3. *Resolução*: As chamadas recursivas retornam valores que
    são combinados para resolver o problema original.
]

== Exemplo Clássico: Fatorial

#align(horizon)[
  - *Definição Matemática*:

    $
      n! = cases(
        1 "se" n = 0,
        n times (n - 1)! "se" n > 0
      )
    $

  - *Implementação Recursiva em C*:

    ```c
    int fatorial(int n) {
      if (n == 0)
        return 1;
      else
        return n * fatorial(n - 1);
    }
    ```

  #pagebreak()

  - *Bonus: Implementação em Haskell*:

    ```haskell
    fatorial :: Int -> Int
    fatorial 0 = 1
    fatorial n = n * fatorial (n - 1)
    ```
]

== Visualização da Recursão do Fatorial

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
      digraph Fatorial {
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
    caption: "Árvore de Recursão para fatorial(4)",
  )
]

== Quando Usar Recursão?

#align(horizon)[
  - *Problemas que podem ser divididos em subproblemas semelhantes*:
    Como árvores, grafos e estruturas hierárquicas.

  - *Algoritmos que requerem _backtracking_*:
    Como busca em profundidade (DFS), algoritmos de permutação e combinação.

  - *Facilitar a implementação*: Alguns algoritmos são mais fáceis de
    implementar de forma recursiva do que iterativa.
]

== Vantagens e Desvantagens

#align(horizon)[
  *Vantagens*:

  - Código mais limpo e legível para certos problemas.
  - Naturalmente adequada para estruturas de dados recursivas (árvores, grafos).
  - Facilita a solução de problemas complexos ao dividi-los em partes menores.

  #pagebreak()

  *Desvantagens*:

  - Consome mais memória devido à pilha de chamadas.
  - Pode ser menos eficiente em termos de tempo em comparação com soluções iterativas.
  - Risco de estouro de pilha#footnote[_stack overflow_] se a recursão for muito profunda.
]

== Recursão vs Iteração

#align(horizon)[
  #text(size: 15pt)[
    - *Recursão*:
      - Usa chamadas de função para repetir o código.
      - Pode ser menos eficiente devido ao overhead de chamadas de função.
      - Mais intuitiva para problemas que são naturalmente recursivos.
    - *Iteração*:
      - Usa estruturas de repetição como loops (`for`, `while`).
      - Geralmente mais eficiente em termos de uso de memória e tempo.
      - Pode ser menos intuitiva para certos problemas.
  ]

  #pagebreak()

  *Exemplo*: Cálculo do fatorial de `n`.

  - *Recursivo*:

    ```c
    int fatorial(int n) {
      if (n == 0)
        return 1;
      else
        return n * fatorial(n - 1);
    }
    ```

  #pagebreak()

  - *Iterativo*:

    ```c
    int fatorial(int n) {
      int resultado = 1;
      for (int i = 2; i <= n; i++) {
        resultado *= i;
      }
      return resultado;
    }
    ```
]

== Cautela com Recursão Excessiva

#align(horizon)[
  - *Estouros de Pilha*:
    Cada chamada recursiva adiciona um quadro à pilha de chamadas.
    Recursão muito profunda pode levar a estouros.

  - *Redundância de Cálculos*:
    Em algumas recursões, como no cálculo ingênuo de Fibonacci,
    muitos cálculos são repetidos.

  - *Otimização*: Técnicas como *_memoization_* ou
    transformar a recursão em iteração podem melhorar a eficiência.
]

== _Memoization_

#align(horizon)[
  Armazena os resultados de subproblemas já resolvidos para evitar cálculos repetidos.

  *Exemplo com Fibonacci*:

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

== Recursão de Cauda

#align(horizon)[
  - *Definição*:
    Uma recursão onde a chamada recursiva é a última operação da função.

  - *Benefícios*: Alguns compiladores podem otimizar recursões de cauda
    para evitar o crescimento da pilha (eliminação de recursão de cauda).

  - *Exemplo*:

  #text(size: 11pt)[
    ```c
    int fatorial_tail(int n, int acumulador) {
      if (n == 0)
        return acumulador;
      else
        return fatorial_tail(n - 1, n * acumulador);
    }
    ```
  ]
]

== Parte Prática (C ou pseudocódigo)

#align(horizon)[
  - *Problema*: Implemente uma função recursiva que determina
    se uma palavra é um palíndromo.

  - *Dica*: Compare o primeiro e o último caractere da _string_
    e chame a função recursivamente para a _substring_ interna.
]

= Divisão e Conquista

==

#align(horizon + center)[
  #image(
    "images/divide_and_conquer_meme.png",
    width: 50%,
  )
]

== O que é Divisão e Conquista?

#align(horizon)[
  *Divisão e Conquista* é um paradigma de projeto de algoritmos que consiste
  em dividir um problema em subproblemas menores,
  resolver esses subproblemas de forma recursiva
  e então combinar as soluções para obter a solução final.
]

== Como Funciona?

#align(horizon)[
  1. *Dividir*: O problema é dividido em subproblemas menores que
    são instâncias do mesmo tipo do problema original.

  2. *Conquistar*: Os subproblemas são resolvidos recursivamente.
    Se forem suficientemente pequenos, são resolvidos diretamente.

  3. *Combinar*: As soluções dos subproblemas são combinadas para
    resolver o problema original.
]

== Exemplos Clássicos

#align(horizon)[
  - *_Merge Sort_*: Um algoritmo de ordenação que divide o _array_ ao meio,
    ordena cada metade e então combina as duas metades ordenadas.

  - *_Quick Sort_*: Um algoritmo que seleciona um pivô,
    divide o _array_ em _subarrays_ menores e maiores que o pivô,
    e então ordena recursivamente os _subarrays_.

  - *Busca Binária*: Um método de busca que
    divide o espaço de busca pela metade a cada iteração.
]

== Teorema mestre

#align(horizon)[
  Na análise de algoritmos,
  o #link("https://en.wikipedia.org/wiki/Master_theorem_(analysis_of_algorithms)")[*teorema mestre*]
  para recorrências de divisão e conquista fornece
  uma análise assintótica (usando a notação Big-O)
  para *relações de recorrência* que ocorrem na análise de
  muitos algoritmos de divisão e conquista.

  #pagebreak()

  Considere um problema que pode ser resolvido usando um algoritmo recursivo
  como o algoritmo a seguir:

  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [Exemplo de Recursão],
    text(size: 12pt)[
      #pseudocode-list(
        title: smallcaps[Procedimento $p$(entrada $x$ de tamanho$n$)],
      )[
        + *if* $n < "alguma constante" k$:
          + resolver $x$ diretamente, sem recursão
        + *else*:
          + criar a subproblemas de $x$, cada um com tamanho $n/b$
          + chamar o procedimento $p$ recursivamente em cada subproblema
          + combinar os resultados dos subproblemas
      ]
    ],
  ) <master-theorem>

  #pagebreak()

  - A árvore de chamadas tem um nó para cada chamada recursiva.
  - Os nós-folha são os casos base da recursão:
    subproblemas de tamanho menor que $k$ que não se resolve recursivamente.
  - Cada nó realiza uma quantidade de trabalho que corresponde ao tamanho do
    subproblema $m$ dada por $p(m)$.
  - A quantidade total de trabalho realizado pelo algoritmo completo é
    a soma do trabalho realizado por todos os nós na árvore.

  #pagebreak()

  #align(horizon + center)[
    #image(
      "images/master_theorem_intuition.png",
      width: 100%,
    )
  ]
]

== Análise de Complexidade

#align(horizon)[
  A complexidade de algoritmos de divisão e conquista pode ser
  expressa pela *recorrência Mestre*:

  $ T(n) = a T(n / b) + f(n) $

  #pagebreak()

  Onde:

  - $T(n)$: Tempo de execução do algoritmo em uma entrada $n$.
  - $a$: Número de subproblemas.
  - $b$: Fator pelo qual o tamanho do problema é dividido.
  - $f(n)$: Custo de dividir e combinar os subproblemas.

  #pagebreak()

  A solução dessa recorrência depende da relação entre $f(n)$ e $n^(log_b a)$.

  - *Caso 1*: Se $f(n) = O(n^(log_b a - epsilon))$ para alguma constante $epsilon > 0$,
    então $T(n) = O(n^(log_b a))$.

  - *Caso 2*: Se $f(n) = O(n^(log_b a))$, então $T(n) = O(n^(log_b a) log n)$.

  - *Caso 3*: Se $f(n) = O(n^(log_b a + epsilon))$ para alguma constante $epsilon > 0$
    e se $a f(n/b) <= c f(n)$ para alguma constante $c < 1$,
    então $T(n) = O(f(n))$.
]

== Exemplo: _Merge Sort_

#align(horizon)[
  #text(size: 14pt)[
    - *_Merge Sort_* divide o problema em 2 subproblemas de tamanho $n/2$:

    $ T(n) = 2 T(n / 2) + O(n) $

    - Aqui, $a = 2$, $b = 2$, $f(n) = O(n)$.

    - Calculamos $n^(log_b a) = n^(log_2 2) = n^1$.

    - Como $f(n) = O(n^(log_b a))$, estamos no *Caso 2* do Teorema Mestre.

    - Portanto, $T(n) = O(n log n)$.
  ]
]

== Exemplo: _Quick Sort_ (Pior Caso)

#align(horizon)[
  - No pior caso, o *_Quick Sort_* divide o problema em
    um subproblema de tamanho $n-1$ e outro de tamanho 0:

  $ T(n) = T(n - 1) + O(n) $

  - Essa recorrência resolve para $T(n) = O(n^2)$.

  - No melhor caso (partições equilibradas), a complexidade é $O(n log n)$.
]

== Aplicações de Divisão e Conquista

#align(horizon)[
  - *Multiplicação de Inteiros Grandes*
    (#link("https://en.wikipedia.org/wiki/Karatsuba_algorithm")[Algoritmo de Karatsuba])
  - *Transformada Rápida de Fourier*:
    (#link("https://en.wikipedia.org/wiki/Fast_Fourier_transform")[FFT])
  - *Multiplicação de Matrizes*
    (#link("https://en.wikipedia.org/wiki/Strassen_algorithm")[Algoritmo de Strassen])
  - *Problemas de Geometria Computacional*:
    (#link("https://en.wikipedia.org/wiki/Convex_hull")[Fecho Convexo], etc.)
]

== Vantagens e Desvantagens

#align(horizon)[
  #text(size: 14pt)[
    *Vantagens*:

    - Pode reduzir a complexidade de problemas complexos.
    - Utiliza a recursividade, facilitando a implementação de algoritmos complexos.

    *Desvantagens*:

    - Pode ter sobrecarga de tempo e espaço devido às chamadas recursivas.
    - Nem todos os problemas são naturalmente divisíveis em subproblemas menores.
  ]
]

== Parte Prática (C ou pseudocódigo)

#align(horizon)[
  - *Problema*: Implemente um algoritmo que eleva um número $x$
    a uma potência $n$ utilizando o paradigma de divisão e conquista,
    otimizando para $O(log n)$.

  - *Dica*: Utilize a propriedade que $x^n = (x^(n/2))^2$ para $n$ par.
]

= Algoritmos Gulosos

==

#align(horizon + center)[
  #image(
    "images/greedy_algo_meme.jpeg",
    width: 95%,
  )
]

== Introdução

#align(horizon)[
  *Algoritmos Gulosos* são uma abordagem para resolver problemas de otimização
  que segue a estratégia de fazer a melhor escolha local em cada etapa,
  com a esperança de encontrar uma solução global ótima.

  A ideia é construir uma solução peça por peça,
  sempre escolhendo a opção que parece ser a melhor no momento.
]

== Características dos Algoritmos Gulosos

#align(horizon)[
  - *Escolha Gulosa (_Greedy Choice_)*: Em cada passo, escolhe a opção que parece ser a melhor no momento.
  - *Subestrutura Ótima*: Uma solução ótima para o problema contém soluções ótimas para subproblemas.
  - *Não-Reversão*: Decisões tomadas não são revistas posteriormente.
]

== Quando Usar Algoritmos Gulosos

#align(horizon)[
  - Quando o problema exibe a *propriedade gulosa*, ou seja, uma solução ótima pode ser alcançada fazendo escolhas locais ótimas.
  - Quando o problema tem a *propriedade de subestrutura ótima*, permitindo que soluções ótimas sejam construídas a partir de subsoluções ótimas.
]

== Exemplo: _Fractional Knapsack_

#align(horizon)[
  - *Problema:* Dado um conjunto de itens, cada um com um peso e um valor, determine a fração de cada item a ser incluída em uma mochila com capacidade limitada de peso, de modo a maximizar o valor total.
  - *Nota:* Itens podem ser divididos (diferente do _0-1 Knapsack_).
]

#pagebreak()

=== Abordagem Gulosa

#align(horizon)[
  1. Calcular o valor por unidade de peso para cada item $(v_i / w_i)$.
  2. Ordenar os itens em ordem decrescente de valor por unidade de peso.
  3. Incluir o máximo possível do item com o maior valor por unidade de peso.
  4. Se a capacidade permitir, passar para o próximo item na ordem e repetir o passo 3.
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [_Fractional Knapsack_ Guloso],
    text(size: 8pt)[
      #pseudocode-list(
        title: smallcaps[Função fractional_knapsack(capacidade, itens):],
      )[
        + *para* cada item em itens:
          + calcular $"valor_por_peso" := "valor" / "peso"$
        + ordenar itens em ordem decrescente de $"valor_por_peso"$
        + $"valor_total" := 0$
        + *para* cada item em itens ordenados:
          + *se* $"capacidade" > 0$:
            + $"quantidade" := \min("peso_item", "capacidade")$
            + $"valor_total" := "valor_total" + "quantidade" times "valor_por_peso"$
            + $"capacidade" := "capacidade" - "quantidade"$
          + *senão*:
            + *quebrar* o _loop_
        + *retornar* $"valor_total"$
      ]
    ],
  ) <fractional-knapsack>
]

#pagebreak()

=== Exemplo em C

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
        return ratioB - ratioA;
    }

    double fractional_knapsack(Item items[], int n, double capacity) {
        quick_sort(items, n, sizeof(Item), compare);
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

=== Análise de Complexidade

#align(horizon)[
  - *Tempo de Ordenação:* $O(n log n)$ devido ao _quicksort_.
  - *Tempo Total:* $O(n log n)$
  - *Espaço Auxiliar:* $O(1)$ (se a ordenação for _in-place_)
]

== Características Importantes dos Algoritmos Gulosos

#align(horizon)[
  - *Simples e Intuitivos:* Fácil de implementar e entender.
  - *Eficiência:* Muitas vezes têm uma complexidade de tempo menor que algoritmos como programação dinâmica.
  - *Limitações:* Nem sempre fornecem a solução ótima global; dependem das propriedades do problema.
]

== Problemas Comuns Resolvidos com Algoritmos Gulosos

#align(horizon)[
  - Problema de Seleção de Atividades
  - Problema de Codificação de Huffman
  - Problema do Troco com Moedas
  - Caminho de Custo Mínimo em Grafos (Dijkstra)
  - Problema do Caixeiro Viajante (Heurísticas)
]

#pagebreak()

== Quando Algoritmos Gulosos Não Funcionam

#align(horizon)[
  - *Problema do _0-1 Knapsack_*:
    Não é possível dividir itens; a abordagem gulosa não garante a solução ótima.
  - *Problema do Caixeiro Viajante:*
    Escolher a próxima cidade mais próxima não leva à rota ótima global.
]

== Comparação com Programação Dinâmica

#align(horizon)[
  #text(size: 13pt)[
    - *Programação Dinâmica:*
      - Resolve subproblemas menores e armazena seus resultados para evitar recomputação.
      - Pode garantir a solução ótima global quando a recorrência modela corretamente as restrições.
      - Pode ser mais complexo e consumir mais tempo e espaço.
    - *Algoritmos Gulosos:*
      - Fazem a melhor escolha local sem considerar subproblemas.
      - Podem não garantir a solução ótima global.
      - Mais eficientes em termos de tempo e espaço.
  ]
]

#pagebreak()

== Conclusão

#align(horizon)[
  - *Vantagens:*
    - Simplicidade e eficiência.
    - Adequado para problemas com propriedades gulosas e subestrutura ótima.
  - *Desvantagens:*
    - Não aplicável a todos os problemas.
    - Pode não encontrar a solução ótima global.
  - *Importante:*
    - Analisar cuidadosamente o problema para determinar se uma abordagem gulosa é adequada.
]

== Seção Prática

#align(horizon)[
  - *Tarefa:* Resolva o Problema do _0-1 Knapsack_ usando uma abordagem gulosa e compare com a solução ótima.
  - *Tarefa:* Identifique um problema que não possa ser resolvido de forma ótima com algoritmos gulosos e explique por quê.
]

= Programação Dinâmica

==

#align(horizon + center)[
  #image(
    "images/dynamic_programming_meme.png",
    width: 95%,
  )
]

== Introdução

#align(horizon)[
  *Programação Dinâmica* é uma técnica de otimização que resolve problemas complexos
  dividindo-os em subproblemas menores e mais simples, armazenando os resultados
  de subproblemas já solucionados para evitar computações redundantes.

  É particularmente útil para problemas que exibem *subestrutura ótima*
  e *sobreposição de subproblemas*.
]

== Características da Programação Dinâmica

#align(horizon)[
  - *Subestrutura Ótima:* A solução ótima do problema pode ser construída a partir das soluções ótimas de seus subproblemas.
  - *Sobreposição de Subproblemas:* Os subproblemas se repetem várias vezes; portanto, é eficiente armazenar seus resultados.
  - *Memorização:* Armazenamento de resultados de subproblemas já resolvidos.
]

== Abordagens de Implementação

#align(horizon)[
  - *Top-Down com Memorização:*
    - Implementação recursiva.
    - Armazena os resultados de subproblemas em uma estrutura de dados (como uma tabela ou dicionário).
  - *Bottom-Up (Iterativa):*
    - Resolve subproblemas menores primeiro e utiliza seus resultados para construir soluções para subproblemas maiores.
    - Geralmente envolve a construção de uma tabela.
]

== Exemplo: Fibonacci

#align(horizon)[
  - *Definição Recursiva:*
    - $F(0) = 0$
    - $F(1) = 1$
    - $F(n) = F(n-1) + F(n-2)$, para $n >= 2$
  - *Problema:* Calcular o $n$-ésimo número de Fibonacci.
]

#pagebreak()

=== Implementação Recursiva Simples (Ineficiente)

#align(horizon)[
  ```c
  int fibonacci(int n) {
      if (n <= 1)
          return n;
      else
          return fibonacci(n - 1) + fibonacci(n - 2);
  }
  ```
  - *Complexidade de Tempo:* Exponencial, $O(2^n)$
  - *Problema:* Muitas computações redundantes.
]

#pagebreak()

=== Implementação com Memorização (Top-Down)

#align(horizon)[
  #text(size: 11pt)[
    ```c
    int fib_memo[int_MAX]; // Inicialize com -1

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
    - *Complexidade de Tempo:* $O(n)$
    - *Vantagem:* Evita computações redundantes armazenando resultados.
  ]
]

#pagebreak()

=== Implementação Iterativa (Bottom-Up)

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
    - *Complexidade de Tempo:* $O(n)$
    - *Complexidade de Espaço:* $O(n)$
  ]
]

#pagebreak()

=== Otimização de Espaço

#align(horizon)[
  #text(size: 11pt)[
    ```c
      int fibonacci(int n) {
        int a = 0, b = 1, c;
        if (n == 0)
            return a;
        for (int i = 2; i <= n; i++) {
          c = a + b;
          a = b;
          b = c;
      }
      return b;
    }
    ```
    - *Complexidade de Tempo:* $O(n)$
    - *Complexidade de Espaço:* $O(1)$
    - *Vantagem:* Utiliza apenas variáveis escalares, economizando memória.
  ]
]

== Exemplo: 0-1 _Knapsack_

#align(horizon)[
  - *Problema:* Dado um conjunto de itens, cada um com um peso e um valor, determinar o número de cada item a ser incluído em uma coleção de modo que o peso total seja menor ou igual a uma capacidade dada e o valor total seja o máximo possível.
  - *Restrições:*
    - Cada item pode ser incluído no máximo uma vez.
    - Itens não podem ser fracionados.
]
#pagebreak()

=== Abordagem com Programação Dinâmica

#align(horizon)[
  - *Definição da Função:*
    $K(n, W)$ é o valor máximo que pode ser obtido considerando os primeiros $n$ itens e capacidade máxima $W$.
  - *Recorrência:*
    - Se $w_n > W$ (peso do item $n$ é maior que a capacidade atual):
      $K(n, W) = K(n - 1, W)$
    - Senão:
      $K(n, W) = max(K(n - 1, W), v_n + K(n - 1, W - w_n))$
]

#pagebreak()

=== Pseudocódigo

#align(horizon)[
  #figure(
    kind: "algorithm",
    supplement: [Algoritmo],
    caption: [0-1 _Knapsack_ com Programação Dinâmica],
    text(size: 8pt)[
      #pseudocode-list(
        title: smallcaps[Função knapsack(valores, pesos, n, W):],
      )[
        + criar tabela $K[0..n][0..W]$
        + para $i$ de $0$ até $n$:
        + para $w$ de $0$ até $W$:
          + se $i == 0$ ou $w == 0$:
            + $K[i][w] = 0$
          + senão se $"pesos"[i - 1] \leq w$:
            + $K[i][w] = max("valores"[i - 1] + K[i - 1][w - "pesos"[i - 1]], \ K[i - 1][w])$
          + senão:
            + $K[i][w] = K[i - 1][w]$
        + retornar $K[n][W]$
      ]
    ],
  ) <knapsack>
]

#pagebreak()

=== Exemplo em C

#align(horizon)[
  #text(size: 10pt)[
    ```c
    int knapsack(int W, int weights[], int values[], int n) {
      int i, w;
      int K[n + 1][W + 1];
      // Construir tabela K[][] de forma bottom-up
      for (i = 0; i <= n; i++) {
        for (w = 0; w <= W; w++) {
          if (i == 0 || w == 0)
            K[i][w] = 0;
          else if (weights[i - 1] <= w)
            K[i][w] = max(values[i - 1] +
                K[i - 1][w - weights[i - 1]], K[i - 1][w]);
          else
            K[i][w] = K[i - 1][w];
        }
      }

      return K[n][W];
    }
    ```
  ]
]

== Análise de Complexidade

#align(horizon)[
  - *Complexidade de Tempo:* $O(n W)$, onde $n$ é o número de itens e $W$ é a capacidade da mochila.
  - *Complexidade de Espaço:* $O(n W)$, devido à tabela utilizada.
]

== Outros Problemas Clássicos

#align(horizon)[
  - *Problema da Subsequência Comum Máxima (LCS)*
  - *Problema do Caminho Mínimo* (como o algoritmo de Floyd-Warshall)
  - *Problema de Corte de Hastes*
  - *Problema de Escalonamento de Tarefas*
]

== Como Identificar Problemas Adequados

#align(horizon)[
  #text(size: 14pt)[
    - *Subestrutura Ótima:*
      A solução ótima do problema pode ser construída a partir das soluções ótimas de seus subproblemas.
    - *Sobreposição de Subproblemas:*
      O problema pode ser dividido em subproblemas que são reutilizados várias vezes.
    - *Exemplos de Perguntas:*
      - O problema pode ser dividido em etapas menores?
      - As etapas menores se repetem?
  ]
]

== Comparação com Algoritmos Gulosos

#align(horizon)[
  #text(size: 14pt)[
    - *Algoritmos Gulosos:*
      - Fazem a melhor escolha local em cada etapa.
      - Não revisitam decisões anteriores.
      - Podem não encontrar a solução ótima global.
    - *Programação Dinâmica:*
      - Considera todas as possibilidades e escolhe a melhor.
      - Armazena resultados de subproblemas para evitar recomputação.
      - Pode garantir a solução ótima global quando a recorrência está correta.
  ]
]

== Técnicas de Otimização

#align(horizon)[
  #text(size: 14pt)[
    - *Otimização de Espaço:*
      Reduzir o espaço utilizado armazenando apenas o necessário.
      - Exemplo: Usar vetores unidimensionais em vez de matrizes bidimensionais quando possível.
    - *Reutilização de Subproblemas:*
      Identificar subproblemas idênticos para evitar recomputação.
    - *Programação Dinâmica com Memoização vs. Iterativa:*
      Escolher a abordagem que melhor se adapta ao problema e às restrições de recursos.
  ]
]

== Conclusão

#align(horizon)[
  #text(size: 14pt)[
    - *Vantagens:*
      - Pode garantir a solução ótima global quando o modelo de PD é válido.
      - Evita computações redundantes.
    - *Desafios:*
      - Pode consumir muito tempo e espaço para problemas com grandes entradas.
      - Identificar a subestrutura ótima e a sobreposição de subproblemas nem sempre é trivial.
    - *Importante:*
      - Formular corretamente a recorrência e as condições iniciais.
      - Decidir entre abordagem Top-Down e Bottom-Up.
  ]
]

== Seção Prática

#align(horizon)[
  - *Tarefa:* Implementar o algoritmo de LCS (Subsequência Comum Máxima) e analisar sua complexidade.
  - *Tarefa:* Resolver o Problema do Corte de Hastes usando programação dinâmica e comparar com a abordagem recursiva simples.
  - *Tarefa:* Identificar um problema real que pode ser otimizado usando programação dinâmica e desenvolver a solução.
]
