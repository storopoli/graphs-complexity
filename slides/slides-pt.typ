#import "@preview/slydst:0.1.0": *
#import "@preview/diagraph:0.2.5": *
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
    - Inserção e remoção em uma árvore binária de busca

  #pagebreak()

  - $O(n)$ (complexidade *linear*):
    - Percorrer um _array_
    - Percorrer uma lista encadeada
    - Comparar duas _strings_
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

= Algoritmos de Busca

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
    int bfs(int grafo[][MAX], int inicio, int n) {
        if (inicio->value == T)
          return inicio->id;
        int visitado[MAX] = {0};
        int fila[MAX], frente = 0, traseira = 0;

        visitado[inicio] = 1;
        fila[traseira++] = inicio;

        while (frente < traseira) {
            int atual = fila[frente++];

            for (int i = 0; i < n; i++) {
                if (atual->value == T)
                  return atual->id;
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
            + DFS($G$, $w$)
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
    int dfs(int grafo[][MAX], int atual, int visitado[], int n) {
        if (atual->value == T)
          return atual->id;
        visitado[atual] = 1;

        for (int i = 0; i < n; i++) {
            if (grafo[atual][i] && !visitado[i]) {
                dfs(grafo, i, visitado, n);
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
      table.header([*Característica*], [*BFS*], [*DFS*]),
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

= Recursão

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
  - Cada nó realiza uma quanidade de trabalho que corresponde ao tamanho do
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
  expressa pela *recorrência de Mestre*:

  $ T(n) = a T(n / b) + f(n) $

  #pagebreak()

  Onde:

  - $T(n)$: Tempo de execução do algoritmo em uma entrada $n$.
  - $a$: Número de subproblemas.
  - $b$: Fator pelo qual o tamanho do problema é dividido.
  - $f(n)$: Custo de dividir e combinar os subproblemas.

  #pagebreak()

  A solução dessa recorrência depende da relação entre $f(n)$ e $n^(log_b a)$.

  - *Caso 1*: Se $f(n) = O(n^(log_b a - epsilon))$ para algum $epsilon > 0$,
    então $T(n) = O(n^(log_b a))$.

  - *Caso 2*: Se $f(n) = O(n^(log_b a) log^k n)$ para algum $k >= 0$,
    então $T(n) = O(n^(log_b a) log^(k+1) n)$.

  - *Caso 3*: Se $f(n) = O(n^(log_b a + epsilon))$ para algum $epsilon > 0$
    e se $a f(n/b) <= c f(n)$ para algum $c < 1$,
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
