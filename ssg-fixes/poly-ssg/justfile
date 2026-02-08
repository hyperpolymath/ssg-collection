# poly-ssg justfile
# Polyglot Static Site Generator Framework
#
# Run `just` to see all available recipes

set shell := ["bash", "-uc"]

# Default recipe - show help
default:
    @just --list --unsorted

# ============================================================================
# Core Build Recipes
# ============================================================================

# Build with a specific engine
build engine src="test-corpus/posts" dest="output" *args="":
    @just _build-{{engine}} "{{src}}" "{{dest}}" {{args}}

# Build with all engines in parallel
build-all src="test-corpus/posts" dest="output":
    @parallel just build {} {{src}} {{dest}}/{} ::: $(just engines)

# Watch source and rebuild on changes
watch engine src="test-corpus/posts" dest="output":
    @echo "Watching {{src}} for changes..."
    @while inotifywait -r -e modify,create,delete {{src}} 2>/dev/null; do \
        just build {{engine}} {{src}} {{dest}}; \
    done

# ============================================================================
# Engine-Specific Recipes
# ============================================================================

# Build with Forth Estate
_build-forth-estate src dest *args:
    @cd engines/forth-estate && \
    gforth forth-estate.fs -e 's" {{src}}" s" {{dest}}" build bye' {{args}}

# Test Forth Estate interactively
forth-estate-repl:
    @cd engines/forth-estate && gforth forth-estate.fs

# Run Forth Estate tests
forth-estate-test *tests="all":
    #!/usr/bin/env bash
    cd engines/forth-estate
    case "{{tests}}" in
        all)
            gforth forth-estate.fs -e "test-markdown cr test-frontmatter cr test-full bye"
            ;;
        markdown)
            gforth forth-estate.fs -e "test-markdown bye"
            ;;
        frontmatter)
            gforth forth-estate.fs -e "test-frontmatter bye"
            ;;
        full)
            gforth forth-estate.fs -e "test-full bye"
            ;;
        *)
            echo "Unknown test: {{tests}}"
            echo "Available: all, markdown, frontmatter, full"
            exit 1
            ;;
    esac

# ============================================================================
# Testing Recipes
# ============================================================================

# Run tests for specific engine
test engine *args="":
    @just {{engine}}-test {{args}}

# Run tests for all engines
test-all:
    @for engine in $(just engines); do \
        echo "=== Testing $engine ==="; \
        just test $engine || true; \
        echo ""; \
    done

# Test against corpus with specific engine
test-corpus engine:
    @just build {{engine}} test-corpus/posts test-corpus/output/{{engine}}
    @echo "Output written to test-corpus/output/{{engine}}"

# Validate engine manifest
validate-manifest engine:
    @nickel export engines/{{engine}}/manifest.ncl 2>/dev/null || \
     cat engines/{{engine}}/manifest.json | jq .

# ============================================================================
# Engine Discovery
# ============================================================================

# List all available engines
engines:
    @ls -1 engines/ 2>/dev/null | while read dir; do \
        [ -f "engines/$dir/manifest.json" ] || [ -f "engines/$dir/manifest.ncl" ] && echo "$dir"; \
    done

# Show engine info
info engine:
    @echo "=== {{engine}} ==="
    @cat engines/{{engine}}/manifest.json 2>/dev/null | jq . || \
     nickel export engines/{{engine}}/manifest.ncl 2>/dev/null || \
     echo "No manifest found"

# List engine features
features engine:
    @cat engines/{{engine}}/manifest.json 2>/dev/null | jq -r '.features[]' || \
     nickel query engines/{{engine}}/manifest.ncl --field features 2>/dev/null

# ============================================================================
# Configuration (Nickel)
# ============================================================================

# Generate config from Nickel
config *args:
    @nickel export config.ncl {{args}}

# Validate Nickel configs
validate-config:
    @nickel typecheck config.ncl && echo "Config valid"

# Export config as JSON
config-json:
    @nickel export config.ncl --format json

# Export config as YAML
config-yaml:
    @nickel export config.ncl --format yaml

# ============================================================================
# Installation Recipes
# ============================================================================

# Install Gforth (auto-detect distro)
install-gforth:
    #!/usr/bin/env bash
    if command -v dnf &>/dev/null; then
        sudo dnf install -y gforth
    elif command -v apt &>/dev/null; then
        sudo apt install -y gforth
    elif command -v brew &>/dev/null; then
        brew install gforth
    elif command -v pacman &>/dev/null; then
        sudo pacman -S gforth
    else
        echo "Unknown package manager. Install gforth manually."
        exit 1
    fi

# Install Nickel
install-nickel:
    #!/usr/bin/env bash
    if command -v cargo &>/dev/null; then
        cargo install nickel-lang-cli
    elif command -v nix &>/dev/null; then
        nix-env -i nickel
    else
        echo "Install via cargo or nix, or download from:"
        echo "https://github.com/tweag/nickel/releases"
    fi

# Install all dependencies
install-deps: install-gforth install-nickel
    @echo "Dependencies installed"

# Check if dependencies are available
check-deps:
    #!/usr/bin/env bash
    check() {
        if command -v $1 &>/dev/null; then
            echo "✓ $1: $(command -v $1)"
        else
            echo "✗ $1: not found"
        fi
    }
    check gforth
    check nickel
    check just
    check jq
    check parallel

# ============================================================================
# Cleanup Recipes
# ============================================================================

# Clean all output
clean:
    @rm -rf output/ test-corpus/output/
    @echo "Cleaned output directories"

# Clean specific engine output
clean-engine engine:
    @rm -rf output/{{engine}} test-corpus/output/{{engine}}

# ============================================================================
# Development Recipes
# ============================================================================

# Create a new engine scaffold
new-engine name lang:
    @mkdir -p engines/{{name}}
    @echo '{ "name": "{{name}}", "language": "{{lang}}", "version": "0.1.0" }' | jq . > engines/{{name}}/manifest.json
    @echo "Created engines/{{name}}/"

# Format all Nickel files
fmt:
    @find . -name '*.ncl' -exec nickel format {} \;

# Lint Nickel configs
lint:
    @find . -name '*.ncl' -exec nickel typecheck {} \;

# ============================================================================
# Git Recipes
# ============================================================================

# Stage and show status
stage:
    @git add -A && git status

# Quick commit
commit msg:
    @git add -A && git commit -m "{{msg}}"

# Push to all remotes
push-all:
    @git remote | xargs -I {} git push {} HEAD

# ============================================================================
# Combinatoric Recipes (the billion possibilities)
# ============================================================================

# Build matrix: all engines × all formats
matrix-build:
    @for engine in $(just engines); do \
        for format in html xml json; do \
            echo "Building: $engine → $format"; \
            just build $engine --format=$format 2>/dev/null || true; \
        done; \
    done

# Process single file with engine
process engine file dest="output":
    @just _process-{{engine}} "{{file}}" "{{dest}}"

_process-forth-estate file dest:
    @cd engines/forth-estate && \
    gforth forth-estate.fs -e 's" {{file}}" s" {{dest}}" process-file bye'

# Pipeline: source → engine → post-processor
pipe src engine postproc dest:
    @just build {{engine}} {{src}} /tmp/poly-ssg-pipe
    @{{postproc}} /tmp/poly-ssg-pipe {{dest}}
    @rm -rf /tmp/poly-ssg-pipe

# Diff output between two engines
diff-engines e1 e2 src="test-corpus/posts":
    @just build {{e1}} {{src}} /tmp/poly-diff-1
    @just build {{e2}} {{src}} /tmp/poly-diff-2
    @diff -r /tmp/poly-diff-1 /tmp/poly-diff-2 || true
    @rm -rf /tmp/poly-diff-1 /tmp/poly-diff-2

# Benchmark engine
bench engine iterations="10":
    @hyperfine --warmup 2 --runs {{iterations}} \
        'just build {{engine}} test-corpus/posts /tmp/bench-out'

# Profile engine (requires perf)
profile engine:
    @perf record -g just build {{engine}} test-corpus/posts /tmp/profile-out
    @perf report
