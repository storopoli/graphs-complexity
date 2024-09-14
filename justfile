alias b := build
alias w := watch

# List all the available commands
default:
  just --list

# Build the slides
build:
    @echo "Building slides"
    typst c slides/slides.typ

# Compilar os slides em português
build-pt:
    @echo "Compilando slides"
    typst c slides/slides-pt.typ

# Watch the slides for changes and rebuild
watch:
    @echo "Watching slides"
    typst w slides/slides.typ

# Vigie os slides para alterações e reconstrua
watch-pt:
    @echo "Vigiando os slides"
    typst w slides/slides-pt.typ

# Compile the C code in "code/"
compile:
    #!/usr/bin/env bash
    echo "Compiling all *.c code to output"
    rm -r ./output;mkdir ./output
    for FILE in $(ls ./code/)
    do
    cc ./code/$FILE -o ./output/$FILE.out
    done
