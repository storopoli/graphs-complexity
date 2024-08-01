#import "@preview/slydst:0.1.0": *
#import "@preview/diagraph:0.2.5": *

#set text(lang: "pt")

#show: slides.with(
  title: "Teoria dos Grafos e Complexidade Computacional",
  subtitle: none,
  date: none,
  authors: ("Jose Storopoli, PhD", ),
  layout: "medium",
  ratio: 4/3,
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

= Por que estudar Teoria dos Grafos e Complexidade Computacional?;

#align(horizon + center)[#image("images/algorithm_analysis_meme.jpg", width: 50%)]

== Teoria Computação

#align(horizon)[
  A *teoria da computação* é subcampo da ciência da computação e matemática
  que busca determinar quais problemas podem ser computados
  em um dado modelo de computação.

  A *computação* pode ser definida como o cálculo de uma função por meio de
  um algoritmo.
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
    caption: "Alan Turing e Alonzo Church"
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
  *Algoritmo* é uma sequência finita de ações executáveis que visam obter
  uma solução para um determinado tipo de problema.
]

== Teoria dos Grafos

#align(horizon)[
  Por que estudar Grafos?

  #v(1em)

  #align(center)[
    _Quase_ tudo que você faz em computação pode ser modelado
    como um *problema de grafos*.
  ]
]

== Complexidade Computacional

#align(horizon)[
  *Complexidade Computacional* é um campo da ciência da computação que
  estuda a quantidade de recursos necessários para resolver um problema
  computacional#footnote[
    um problema decidível.
  ].
]

#pagebreak()

#align(horizon)[
  Usamos a notação $O$ para descrever a complexidade de um algoritmo.
  - $O(1)$ (complexidade *constante*):
    - Acessar uma array
    - Inserir um nó em uma lista encadeada
    - Inserção e remoção em uma fila
  - $O(log n)$ (complexidade *logarítmica*):
    - Busca binária
    - Inserção e remoção em uma árvore binária de busca

  #pagebreak()

  - $O(n)$ (complexidade *linear*):
    - Percorrer um array
    - Percorrer uma lista encadeada
    - Comparar duas strings
  - $O(n log n)$ (complexidade *log-linear*):
    - Algoritmo de ordenação _Quick Sort_
    - Algoritmo de ordenação _Merge Sort_

  #pagebreak()

  - $(O(n^2))$ (complexidade *quadrática*):
    - Percorrer uma matriz
    - Algoritmo de ordenação _Bubble Sort_
    - Algoritmo de ordenação _Insertion Sort_
  - $(O(n^3))$ (complexidade *cúbica*):
    - Multiplicação de matrizes (abordagem ingênua)
  - $(O(n!))$ (complexidade *fatorial*):
    - Solução do problema do caixeiro-viajante
    - Gerar todas as permutações de uma lista
]

= Grafos

#align(horizon + center)[#image("images/graph_isomorphism_meme.jpg", width: 50%)]

== O que são Grafos?

Grafos são estruturas matemáticas que modelam *relações entre objetos*.

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
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
      ```
  ),
    caption: "Grafo"
  ) <grafo>
]

== Formalmente

Grafos são *pares ordenados* $G = (V, E)$ onde:

- $V$ é um conjunto finito de *vértices* (também chamados de nós)
- $E$ é um conjunto finito de *arestas* (também chamadas de arcos)
  representado por um par de vértices $(u, v)$

A @grafo, por exemplo:

#text(size: 14pt)[
  $ V = \{a, b, c, d, e, f\} $
  $ E = \{(a, b), (a, c), (b, c), (b, d), (c, e), (d, e), (e, f)\} $
]

== Grafos Direcionados

Grafos podem ser *direcionados* ou *_não_-direcionados*.

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
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
      ```
  ),
    caption: "Grafo Direcionado"
  ) <grafo-direcionado>
]

== Grafos Ponderados

Grande parte dos grafos são *ponderados*, isto é,
possuem valores associados às arestas.

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
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
      ```
  ),
    caption: "Grafo Ponderado"
  ) <grafo-ponderado>
]

== Exemplos de Grafos

#align(horizon)[
  - Redes de computadores
  - Redes sociais
  - Mapas de cidades
  - Estruturas moleculares
]

== #link("https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg")[As 7 pontes de Königsberg]

Primeira aplicação prática da teoria dos grafos,
resolvida por Euler em 1736.

#align(center)[
  *É possível atravessar todas as pontes sem repetir nenhuma?*
]

#align(horizon + center)[
  #figure(
    image("images/konigsberg_briges.png", width: 35%),
    caption: "As 7 pontes de Königsberg"
  ) <pontes-konigsberg>
]

== #link("https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg")[As 7 pontes de Königsberg]

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
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
      ```
  ),
    caption: "Grafo das 7 pontes de Königsberg"
  ) <grafo-pontes-konigsberg>
]

== Solução das 7 pontes

#align(horizon)[
  A solução do problema de Königsberg foi dada por Euler.
  O grafo precisa de *duas condições* para ser resolvido:

  - O grafo deve ser *totalmente conectado*
  - O grafo deve ter exatamente *0 ou 2 vértices de grau ímpar*
]

== #link("https://en.wikipedia.org/wiki/Four_color_theorem")[O teorema das 4 cores]

#align(horizon)[
  *Não mais do que quatro cores são necessárias para
  colorir as regiões de qualquer mapa,
  de modo que duas regiões adjacentes não tenham a mesma cor.*
]

#pagebreak()

#align(horizon + center)[
  #figure(
    image("images/four_color_graph.svg", width: 50%),
    caption: "Abstração de um mapa com 4 cores usando grafos"
  ) <four-color-map>
]

#text(size: 14pt)[
  O teorema foi provado em 1976 por Kenneth Appel e Wolfgang Haken#footnote[
    um dos primeiros teoremas provados auxiliado por computadores.
  ].
]

== Subgrafos

Um *subgrafo* de um grafo $G$ é outro grafo formado a partir de
um *subconjunto dos vértices e arestas de $G$*.
O subconjunto de vértices deve incluir todos os vértices das arestas,
mas pode incluir vértices adicionais.

#align(horizon + center)[
  #figure(
    image("images/subgraph.svg", width: 40%),
    caption: "Subgrafo"
  ) <subgraph>
]

== Subgrafo Induzido

Um *subgrafo induzido* é um subgrafo que *inclui todos os vértices e arestas*
cujos extremos pertencem ao subconjunto de vértices.

#align(horizon + center)[
  #figure(
    image("images/induced_subgraph.svg", width: 50%),
    caption: "Subgrafo Induzido"
  ) <induced-subgraph>
]

== Isomorfismo

Um isomorfismo dos grafos $G$ e $H$ e uma bijeção#footnote[
  uma função que estabelece uma correspondência biunívoca
  entre os elementos de dois conjuntos.
]
entre os conjuntos
de vértices de $G$ e $H$:


$ f: V(G) -> V(H) $

#align(horizon + center)[
  #figure(
    image("images/graph_isomorphism.png", width: 66%),
    caption: "Grafos Isomórficos"
  ) <isomorphic-graphs>
]

== Representação de Grafos

#align(horizon)[
  Há várias formas de representar grafos,
  as mais comuns são:

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
      text[$ bold(A) = mat(
        1, 1, 0, 0, 1, 0;
        1, 0, 1, 0, 1, 0;
        0, 1, 0, 1, 0, 0;
        0, 0, 1, 0, 1, 1;
        1, 1, 0, 1, 0, 0;
        0, 0, 0, 1, 0, 0;
      ) $],
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
        width: 80%
      ),
    ),
    caption: "Matriz de adjacência e Grafo"
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
  Uma *lista de adjacência* é uma lista de listas,
  onde cada lista $L_i$ contém os vértices adjacentes ao vértice $i$.
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
        width: 80%
      ),
    ),
    caption: "Lista de adjacência e Grafo"
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
    *Caminho* é uma sequência de vértices tal que de cada um dos vértices existe
    uma aresta para o vértice seguinte.

    Um caminho é chamado *simples* se nenhum dos vértices no caminho se repete.
    O *comprimento* do caminho é o número de arestas que o caminho usa,
    contando-se arestas múltiplas vezes.

    O *custo* de um caminho num grafo balanceado é a soma dos custos
    das arestas atravessadas.

    Dois caminhos são *independentes* se não tiverem nenhum vértice em comum,
    exceto o primeiro e o último.
  ]
]

#pagebreak()

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
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
      ```
  ),
    caption: "Caminho de Comprimento 4"
  ) <path>
]

#pagebreak()

#align(horizon)[
  Um *ciclo* é um caminho em que o *primeiro e o último vértice coincidem*,
  mas nenhum outro vértice é *repetido*.
]

#pagebreak()

#align(horizon + center)[
  #figure(
    raw-render(
      ```dot
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
      ```
  ),
    caption: "Ciclo de Comprimento 4"
  ) <cycle>
]

== Caminho Euleriano

#align(horizon)[
  *Caminho Euleriano* é o caminho que usa cada aresta exatamente uma vez.
  Se tal caminho existir, o grafo é chamado traversável.

  Um *ciclo Euleriano* é um ciclo que usa cada aresta exatamente uma vez.
]

== Caminho Hamiltoniano

#align(horizon)[
  *Caminho Hamiltoniano* é o caminho que visita cada vértice exatamente uma vez.

  Um *ciclo Hamiltoniano*#footnote[
    curiosidade: um dos primeiros esquemas de zero-knowledge proofs foi baseado
    em achar um ciclo Hamiltoniano em um grafo gigante.
    Para mais detalhes, veja a #link("https://en.wikipedia.org/wiki/Zero-knowledge_proof#Hamiltonian_cycle_for_a_large_graph")[Wikipedia]
    e o #link("https://web.archive.org/web/20230103032937/http://euler.nmt.edu/~brian/students/pope.pdf")[paper original].
  ] é um ciclo que visita cada vértice uma só vez.
]

== #link("https://en.wikipedia.org/wiki/Travelling_salesman_problem")[Problema do caixeiro-viajante]

O *problema do caixeiro-viajante* (PCV) é um problema que tenta determinar
a menor rota para percorrer uma série de cidades (visitando uma única vez cada uma delas),
retornando à cidade de origem.

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
      width: 50%
  ),
    caption: "Problema do Cacheiro-Viajante"
  ) <travelling-salesman-problem>
]

#pagebreak()

#align(horizon)[
  Formulando em termos de grafos, o PCV é um problema de encontrar um ciclo Hamiltoniano
  tal que o custo do ciclo seja o menor possível.

  $ C = min_("ciclo") sum_(i=1)^n c_(i, i+1) $
]

== Parte Prática (C ou pseudocódigo)

#align(horizon)[
  - Encontrar um caminho Euleriano em C
  - Encontrar um ciclo Hamiltoniano em C
]
