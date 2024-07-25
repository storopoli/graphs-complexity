#import "@preview/slydst:0.1.0": *
#import "@preview/cetz:0.2.2": canvas, draw, tree

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

== Conteúdo

#outline()

= Primeira Seção

A fórmula para entropia é $ H = - sum p(x) log p(x). $

== Fatoração de Números

#align(horizon)[
    #text(size: 22pt)[
        Fatore esse número em primos:

        $ #{1279 * 31 } $
    ]
]

== Grafos

Grafos são estruturas matemáticas que modelam relações entre objetos.
#align(horizon + center)[
  #figure(
    canvas(length: 1cm, {
      import draw: *

      set-style(content: (padding: .2),
        fill: gray.lighten(70%),
        stroke: gray.lighten(70%))

      tree.tree(([A], ([B], [C], [D]), ([E], [F])), spread: 2.5, grow: 1.5, draw-node: (node, ..) => {
        circle((), radius: .45, stroke: none)
        content((), node.content)
      }, draw-edge: (from, to, ..) => {
        line((a: from, number: .6, b: to),
             (a: to, number: .6, b: from), mark: (end: ">"))
      }, name: "tree")
    }),
    caption: "Árvore"
  )
]
