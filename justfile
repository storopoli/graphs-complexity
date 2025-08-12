alias b := build
alias b-pt := build-pt
alias w := watch
alias w-pt := watch-pt
alias c := c-compile
alias z := z-compile
alias h := hs-build
alias hr := hs-run
alias hl := hs-list

# List all the available commands
default:
  just --list

# Clean the PDFs and the output directory
clean:
    @echo "Cleaning up"
    rm slides/slides.pdf
    rm slides/slides-pt.pdf
    rm -rf output

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

# Compile the C code in "code/c/"
c-compile:
    #!/usr/bin/env bash
    set -e
    echo "Compiling all *.c code in performance mode to \"output/\""
    mkdir -p ./output
    rm -rf ./output/*
    for FILE in $(ls ./code/c/); do
        BASENAME="${FILE%.c}"
        cc "./code/c/$FILE" -O3 -o "./output/$BASENAME.out"
    done

# Compile and run the C code in "code/c/"
c-run: c-compile
    #!/usr/bin/env bash
    set -e
    echo "Running all compiled C code in \"output\""
    for FILE in $(ls ./output/); do
        "./output/$FILE"
    done

# Compile the Zig code in "code/zig/"
z-compile:
    #!/usr/bin/env bash
    set -e
    echo "Compiling all *.zig code in performance mode to \"output/\""
    current_path=$(pwd)
    mkdir -p ./output
    rm -rf ./output/*
    cd ./output
    for FILE in $(ls "$current_path/code/zig/"); do
        BASENAME="${FILE%.zig}"
        zig build-exe "$current_path/code/zig/$FILE" -O ReleaseFast
    done

# Compile and run the Zig code in "code/zig/"
z-run: z-compile
    #!/usr/bin/env bash
    set -e
    echo "Running all compiled Zig code in \"output\""
    for FILE in $(ls ./output/*.o); do
        BASENAME="${FILE%.o}"
        "$BASENAME"
    done

# Format Zig code in "code/zig/"
z-fmt:
    #!/usr/bin/env bash
    set -e
    zig fmt ./code/zig/*.zig

# Build all Haskell executables
hs-build:
    @echo "Building all Haskell executables"
    stack build

# List all available Haskell executables
hs-list:
    #!/usr/bin/env bash
    set -e
    echo "Available Haskell executables:"
    echo ""
    echo "Graph Algorithms:"
    grep "graphs-" package.yaml | grep ":" | sed 's/  //' | sed 's/:$//' | grep "graphs-" | sort
    echo ""
    echo "General Algorithms:"
    grep "algorithms-" package.yaml | grep ":" | sed 's/  //' | sed 's/:$//' | grep "algorithms-" | sort

# Run specific Haskell executable, or all if no argument (usage: just hs-run [executable])
hs-run executable="":
    #!/usr/bin/env bash
    set -e
    if [ "{{executable}}" = "" ]; then
        echo "Running all Haskell executables"
        for exec in $(grep "^  [a-z]" package.yaml | grep ":" | sed 's/  //' | sed 's/:$//' | sort); do
            echo "Running: $exec"
            stack run "$exec"
            echo ""
        done
    else
        echo "Running Haskell executable: {{executable}}"
        stack run {{executable}}
    fi
