#import "@preview/slydst:0.1.0": *
#import "@preview/diagraph:0.2.5": *

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
      image("images/turing.jpg", width: 60%), image("images/church.jpg", width: 60%),
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
    - Inserção e remoção em uma árvore binária de busca

  #pagebreak()

  - $O(n)$ (complexidade *linear*):
    - Percorrer um _array_
    - Percorrer uma lista encadeada
    - Comparar duas strings
  - $O(n log n)$ (complexidade *log-linear*):
    - Algoritmo de ordenação _Quick Sort_
    - Algoritmo de ordenação _Merge Sort_

  #pagebreak()

  - $O(n^2)$ (complexidade *quadrática*):
    - Percorrer uma matriz
    - Algoritmo de ordenação _Bubble Sort_
    - Algoritmo de ordenação _Insertion Sort_
  - $O(n^3)$ (complexidade *cúbica*):
    - Multiplicação de matrizes (abordagem ingênua)
  - $O(n!)$ (complexidade *fatorial*):
    - Solução do problema do caixeiro-viajante
    - Gerar todas as permutações de uma lista
]

= Grafos

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

Grande parte dos grafos são *ponderados*, isto é, possuem valores associados às
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

== #link("https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg")[As 7 pontes de Königsberg]

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

== #link("https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg")[As 7 pontes de Königsberg]

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

== #link("https://en.wikipedia.org/wiki/Four_color_theorem")[O teorema das 4 cores]

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
  - Custo de busca de arestas $O(n)$
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

    O *custo* de um caminho num grafo balanceado é a soma dos custos das arestas
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
    achar um ciclo Hamiltoniano em um grafo gigante. Para mais detalhes, veja a #link(
      "https://en.wikipedia.org/wiki/Zero-knowledge_proof#Hamiltonian_cycle_for_a_large_graph",
    )[Wikipedia]
    e o #link(
      "https://web.archive.org/web/20230103032937/http://euler.nmt.edu/~brian/students/pope.pdf",
    )[paper original].
  ] é um ciclo que visita cada vértice uma só vez.
]

== #link("https://en.wikipedia.org/wiki/Travelling_salesman_problem")[Problema do caixeiro-viajante]

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

#align(horizon + center)[#image("images/trees_meme.jpg", width: 50%)]

== O que são Árvores?

Árvores são grafos *acíclicos* e *conectados*.

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
  - *Raiz*: o vértice sem arestas entrantes. Todas as árvores têm (apenas) um
    vértice raiz.
  - *Folha*: vértice sem arestas saindo.
  - *Nível*: distância da raiz.
  - *Altura*: nível máximo.
  - *Pai*: vértice(s) com menor nível (mais próximo da raiz).
  - *Filho*: vértice(s) maior nível (mais distante da raiz).
  - *Ancestral*: vértice(s) com menor nível.
  - *Descendente*: vértice(s) com maior nível.
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
  Uma função é exponencial se ela pode ser reduzida usando notação $O$ para

  $ O(n^m) $

  onde $m$ *_não_* é uma constante positiva.

  Por exemplo, $O(2^n)$ é uma complexidade exponencial#footnote[
    note que $n$ não é constante.
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
    [Não se sabe se $cal(N P)$ é um subconjunto próprio de $cal(P)$ ou se são iguais.],
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

  #link("https://en.wikipedia.org/wiki/Cook%E2%80%93Levin_theorem")[Cook e Levin provaram]
  que todo problema de fácil verificação pode ser resolvido tão rapidamente quanto
  o SAT, que, por isso, é NP-completo.
]

== $cal(N P)$-difíceis

#align(horizon)[
  Um problema $cal(N P)$-difícil é um problema para o qual *não se conhece um
  algoritmo eficiente para resolvê-lo*. No entanto, se um algoritmo eficiente para
  um problema $cal(N P)$-difícil for encontrado, então todos os problemas em $cal(N P)$ podem
  ser resolvidos eficientemente.
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
    #link("https://en.wikipedia.org/wiki/Knapsack_problem")[*Problema da Mochila (_Knapsack Problem_)*]

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
  - *Divisória*: complexidade logarítmica $O(log n)$
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
    #link("https://en.wikipedia.org/wiki/Greatest_common_divisor")[_greatest common divisor_]
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
    #link(
  "https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm",
)[Boyer-Moore's].
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
    #link("https://en.wikipedia.org/wiki/Pascal%27s_triangle")[Triângulo de Pascal]
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
    #link("https://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane's_algorithm")[Kadane]
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
    #link(
  "https://en.wikipedia.org/wiki/Longest_common_subsequence",
)[(_longest common subsequence_ -- LCS)]:
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
    #link(
  "https://en.wikipedia.org/wiki/Fast_inverse_square_root",
)[maneira rápida de calcular a raiz quadrada inversa],
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
