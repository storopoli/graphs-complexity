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
