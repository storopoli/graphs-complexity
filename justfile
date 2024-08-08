alias b := build
alias w := watch

# List all the available commands
default:
  just --list

# Build the slides
build:
    @echo "Building slides"
    typst c slides.typ

# Watch the slides for changes and rebuild
watch:
    @echo "Watching slides"
    typst w slides.typ

# Compile the C code in "code/"
compile:
    #!/usr/bin/env bash
    echo "Compiling all *.c code to output"
    rm -r ./output;mkdir ./output
    for FILE in $(ls ./code/)
    do
    cc ./code/$FILE -o ./output/$FILE.out
    done
