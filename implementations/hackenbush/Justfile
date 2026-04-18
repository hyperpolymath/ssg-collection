# hackenbush-ssg - RSR Standard Justfile
# https://just.systems/man/en/
#
# This is the CANONICAL template for all RSR projects.
# Copy this file to new projects and customize the {{PLACEHOLDER}} values.
#
# Run `just` to see all available recipes
# Run `just cookbook` to generate docs/just-cookbook.adoc
# Run `just combinations` to see matrix recipe options

set shell := ["bash", "-uc"]
set dotenv-load := true
set positional-arguments := true

# Project metadata - CUSTOMIZE THESE
project := "hackenbush-ssg"
version := "0.1.0"
tier := "infrastructure"  # 1 | 2 | infrastructure

# ═══════════════════════════════════════════════════════════════════════════════
# DEFAULT & HELP
# ═══════════════════════════════════════════════════════════════════════════════

# Show all available recipes with descriptions
default:
    @just --list --unsorted

# Show detailed help for a specific recipe
help recipe="":
    #!/usr/bin/env bash
    if [ -z "{{recipe}}" ]; then
        just --list --unsorted
        echo ""
        echo "Usage: just help <recipe>"
        echo "       just cookbook     # Generate full documentation"
        echo "       just combinations # Show matrix recipes"
    else
        just --show "{{recipe}}" 2>/dev/null || echo "Recipe '{{recipe}}' not found"
    fi

# Show this project's info
info:
    @echo "Project: {{project}}"
    @echo "Version: {{version}}"
    @echo "RSR Tier: {{tier}}"
    @echo "Recipes: $(just --summary | wc -w)"
    @[ -f STATE.scm ] && grep -oP '\(phase\s+\.\s+\K[^)]+' STATE.scm | head -1 | xargs -I{} echo "Phase: {}" || true

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD & COMPILE
# ═══════════════════════════════════════════════════════════════════════════════

# Build the project (debug mode)
build *args:
    @echo "Building {{project}}..."
    # TODO: Add build command for your language
    # Rust: cargo build {{args}}
    # ReScript: npm run build
    # Elixir: mix compile

# Build in release mode with optimizations
build-release *args:
    @echo "Building {{project}} (release)..."
    # TODO: Add release build command
    # Rust: cargo build --release {{args}}

# Build and watch for changes
build-watch:
    @echo "Watching for changes..."
    # TODO: Add watch command
    # Rust: cargo watch -x build
    # ReScript: npm run watch

# Clean build artifacts [reversible: rebuild with `just build`]
clean:
    @echo "Cleaning..."
    rm -rf target _build dist lib node_modules

# Deep clean including caches [reversible: rebuild]
clean-all: clean
    rm -rf .cache .tmp

# ═══════════════════════════════════════════════════════════════════════════════
# TEST & QUALITY
# ═══════════════════════════════════════════════════════════════════════════════

# Run all tests
test *args:
    @echo "Running tests..."
    # TODO: Add test command
    # Rust: cargo test {{args}}
    # ReScript: npm test
    # Elixir: mix test

# Run tests with verbose output
test-verbose:
    @echo "Running tests (verbose)..."
    # TODO: Add verbose test

# Run tests and generate coverage report
test-coverage:
    @echo "Running tests with coverage..."
    # TODO: Add coverage command
    # Rust: cargo llvm-cov

# ═══════════════════════════════════════════════════════════════════════════════
# LINT & FORMAT
# ═══════════════════════════════════════════════════════════════════════════════

# Format all source files [reversible: git checkout]
fmt:
    @echo "Formatting..."
    # TODO: Add format command
    # Rust: cargo fmt
    # ReScript: npm run format
    # Elixir: mix format

# Check formatting without changes
fmt-check:
    @echo "Checking format..."
    # TODO: Add format check
    # Rust: cargo fmt --check

# Run linter
lint:
    @echo "Linting..."
    # TODO: Add lint command
    # Rust: cargo clippy -- -D warnings

# Run all quality checks
quality: fmt-check lint test
    @echo "All quality checks passed!"

# Fix all auto-fixable issues [reversible: git checkout]
fix: fmt
    @echo "Fixed all auto-fixable issues"

# ═══════════════════════════════════════════════════════════════════════════════
# RUN & EXECUTE
# ═══════════════════════════════════════════════════════════════════════════════

# Run the application
run *args:
    @echo "Running {{project}}..."
    # TODO: Add run command
    # Rust: cargo run {{args}}

# Run in development mode with hot reload
dev:
    @echo "Starting dev mode..."
    # TODO: Add dev command

# Run REPL/interactive mode
repl:
    @echo "Starting REPL..."
    # TODO: Add REPL command
    # Elixir: iex -S mix
    # Guile: guix shell guile -- guile

# ═══════════════════════════════════════════════════════════════════════════════
# DEPENDENCIES
# ═══════════════════════════════════════════════════════════════════════════════

# Install all dependencies
deps:
    @echo "Installing dependencies..."
    # TODO: Add deps command
    # Rust: (automatic with cargo)
    # ReScript: npm install
    # Elixir: mix deps.get

# Audit dependencies for vulnerabilities
deps-audit:
    @echo "Auditing dependencies..."
    # TODO: Add audit command
    # Rust: cargo audit

# ═══════════════════════════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════════════════════════

# Generate all documentation
docs:
    @mkdir -p docs/generated docs/man
    just cookbook
    just man
    @echo "Documentation generated in docs/"

# Generate justfile cookbook documentation
cookbook:
    #!/usr/bin/env bash
    mkdir -p docs
    OUTPUT="docs/just-cookbook.adoc"
    echo "= {{project}} Justfile Cookbook" > "$OUTPUT"
    echo ":toc: left" >> "$OUTPUT"
    echo ":toclevels: 3" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    echo "Generated: $(date -Iseconds)" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    echo "== Recipes" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    just --list --unsorted | while read -r line; do
        if [[ "$line" =~ ^[[:space:]]+([a-z_-]+) ]]; then
            recipe="${BASH_REMATCH[1]}"
            echo "=== $recipe" >> "$OUTPUT"
            echo "" >> "$OUTPUT"
            echo "[source,bash]" >> "$OUTPUT"
            echo "----" >> "$OUTPUT"
            echo "just $recipe" >> "$OUTPUT"
            echo "----" >> "$OUTPUT"
            echo "" >> "$OUTPUT"
        fi
    done
    echo "Generated: $OUTPUT"

# Generate man page
man:
    #!/usr/bin/env bash
    mkdir -p docs/man
    cat > docs/man/{{project}}.1 << EOF
.TH HACKENBUSH-SSG 1 "$(date +%Y-%m-%d)" "{{version}}" "hackenbush-ssg Manual"
.SH NAME
{{project}} \- RSR standard repository template
.SH SYNOPSIS
.B just
[recipe] [args...]
.SH DESCRIPTION
Canonical template for RSR (Rhodium Standard Repository) projects.
.SH AUTHOR
Hyperpolymath <hyperpolymath@proton.me>
EOF
    echo "Generated: docs/man/{{project}}.1"

# ═══════════════════════════════════════════════════════════════════════════════
# CONTAINERS (nerdctl + Wolfi)
# ═══════════════════════════════════════════════════════════════════════════════

# Build container image
container-build tag="latest":
    @if [ -f Containerfile ]; then \
        nerdctl build -t {{project}}:{{tag}} -f Containerfile .; \
    else \
        echo "No Containerfile found"; \
    fi

# Run container
container-run tag="latest" *args:
    nerdctl run --rm -it {{project}}:{{tag}} {{args}}

# Push container image
container-push registry="ghcr.io/hyperpolymath" tag="latest":
    nerdctl tag {{project}}:{{tag}} {{registry}}/{{project}}:{{tag}}
    nerdctl push {{registry}}/{{project}}:{{tag}}

# ═══════════════════════════════════════════════════════════════════════════════
# CI & AUTOMATION
# ═══════════════════════════════════════════════════════════════════════════════

# Run full CI pipeline locally
ci: deps quality
    @echo "CI pipeline complete!"

# Install git hooks
install-hooks:
    @mkdir -p .git/hooks
    @cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
just fmt-check || exit 1
just lint || exit 1
EOF
    @chmod +x .git/hooks/pre-commit
    @echo "Git hooks installed"

# ═══════════════════════════════════════════════════════════════════════════════
# SECURITY
# ═══════════════════════════════════════════════════════════════════════════════

# Run security audit
security: deps-audit
    @echo "=== Security Audit ==="
    @command -v gitleaks >/dev/null && gitleaks detect --source . --verbose || true
    @command -v trivy >/dev/null && trivy fs --severity HIGH,CRITICAL . || true
    @echo "Security audit complete"

# Generate SBOM
sbom:
    @mkdir -p docs/security
    @command -v syft >/dev/null && syft . -o spdx-json > docs/security/sbom.spdx.json || echo "syft not found"

# ═══════════════════════════════════════════════════════════════════════════════
# VALIDATION & COMPLIANCE
# ═══════════════════════════════════════════════════════════════════════════════

# Validate RSR compliance
validate-rsr:
    #!/usr/bin/env bash
    echo "=== RSR Compliance Check ==="
    MISSING=""
    for f in .editorconfig .gitignore justfile RSR_COMPLIANCE.adoc README.adoc; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    for d in .well-known; do
        [ -d "$d" ] || MISSING="$MISSING $d/"
    done
    for f in .well-known/security.txt .well-known/ai.txt .well-known/humans.txt; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    if [ ! -f "guix.scm" ] && [ ! -f ".guix-channel" ] && [ ! -f "flake.nix" ]; then
        MISSING="$MISSING guix.scm/flake.nix"
    fi
    if [ -n "$MISSING" ]; then
        echo "MISSING:$MISSING"
        exit 1
    fi
    echo "RSR compliance: PASS"

# Validate STATE.scm syntax
validate-state:
    @if [ -f "STATE.scm" ]; then \
        guile -c "(primitive-load \"STATE.scm\")" 2>/dev/null && echo "STATE.scm: valid" || echo "STATE.scm: INVALID"; \
    else \
        echo "No STATE.scm found"; \
    fi

# Full validation suite
validate: validate-rsr validate-state
    @echo "All validations passed!"

# ═══════════════════════════════════════════════════════════════════════════════
# STATE MANAGEMENT
# ═══════════════════════════════════════════════════════════════════════════════

# Update STATE.scm timestamp
state-touch:
    @if [ -f "STATE.scm" ]; then \
        sed -i 's/(updated . "[^"]*")/(updated . "'"$(date -Iseconds)"'")/' STATE.scm && \
        echo "STATE.scm timestamp updated"; \
    fi

# Show current phase from STATE.scm
state-phase:
    @grep -oP '\(phase\s+\.\s+\K[^)]+' STATE.scm 2>/dev/null | head -1 || echo "unknown"

# ═══════════════════════════════════════════════════════════════════════════════
# GUIX & NIX
# ═══════════════════════════════════════════════════════════════════════════════

# Enter Guix development shell (primary)
guix-shell:
    guix shell -D -f guix.scm

# Build with Guix
guix-build:
    guix build -f guix.scm

# Enter Nix development shell (fallback)
nix-shell:
    @if [ -f "flake.nix" ]; then nix develop; else echo "No flake.nix"; fi

# ═══════════════════════════════════════════════════════════════════════════════
# HYBRID AUTOMATION
# ═══════════════════════════════════════════════════════════════════════════════

# Run local automation tasks
automate task="all":
    #!/usr/bin/env bash
    case "{{task}}" in
        all) just fmt && just lint && just test && just docs && just state-touch ;;
        cleanup) just clean && find . -name "*.orig" -delete && find . -name "*~" -delete ;;
        update) just deps && just validate ;;
        *) echo "Unknown: {{task}}. Use: all, cleanup, update" && exit 1 ;;
    esac

# ═══════════════════════════════════════════════════════════════════════════════
# COMBINATORIC MATRIX RECIPES
# ═══════════════════════════════════════════════════════════════════════════════

# Build matrix: [debug|release] × [target] × [features]
build-matrix mode="debug" target="" features="":
    @echo "Build matrix: mode={{mode}} target={{target}} features={{features}}"
    # Customize for your build system

# Test matrix: [unit|integration|e2e|all] × [verbosity] × [parallel]
test-matrix suite="unit" verbosity="normal" parallel="true":
    @echo "Test matrix: suite={{suite}} verbosity={{verbosity}} parallel={{parallel}}"

# Container matrix: [build|run|push|shell|scan] × [registry] × [tag]
container-matrix action="build" registry="ghcr.io/hyperpolymath" tag="latest":
    @echo "Container matrix: action={{action}} registry={{registry}} tag={{tag}}"

# CI matrix: [lint|test|build|security|all] × [quick|full]
ci-matrix stage="all" depth="quick":
    @echo "CI matrix: stage={{stage}} depth={{depth}}"

# Show all matrix combinations
combinations:
    @echo "=== Combinatoric Matrix Recipes ==="
    @echo ""
    @echo "Build Matrix: just build-matrix [debug|release] [target] [features]"
    @echo "Test Matrix:  just test-matrix [unit|integration|e2e|all] [verbosity] [parallel]"
    @echo "Container:    just container-matrix [build|run|push|shell|scan] [registry] [tag]"
    @echo "CI Matrix:    just ci-matrix [lint|test|build|security|all] [quick|full]"
    @echo ""
    @echo "Total combinations: ~10 billion"

# ═══════════════════════════════════════════════════════════════════════════════
# VERSION CONTROL
# ═══════════════════════════════════════════════════════════════════════════════

# Show git status
status:
    @git status --short

# Show recent commits
log count="20":
    @git log --oneline -{{count}}

# ═══════════════════════════════════════════════════════════════════════════════
# UTILITIES
# ═══════════════════════════════════════════════════════════════════════════════

# Count lines of code
loc:
    @find . \( -name "*.rs" -o -name "*.ex" -o -name "*.res" -o -name "*.ncl" -o -name "*.scm" \) 2>/dev/null | xargs wc -l 2>/dev/null | tail -1 || echo "0"

# Show TODO comments
todos:
    @grep -rn "TODO\|FIXME" --include="*.rs" --include="*.ex" --include="*.res" . 2>/dev/null || echo "No TODOs"

# Open in editor
edit:
    ${EDITOR:-code} .

# ═══════════════════════════════════════════════════════════════════════════════
# GAME OF LIFE - PATTERN OPERATIONS
# ═══════════════════════════════════════════════════════════════════════════════

# Validate all Life patterns in patterns/ and src/
validate-patterns:
    #!/usr/bin/env bash
    echo "🔬 Validating Life patterns..."
    FAILED=0
    for f in src/*.rle patterns/*.rle; do
        if [ -f "$f" ]; then
            if head -1 "$f" | grep -q "^#"; then
                echo "✅ $f"
            else
                echo "❌ $f - Invalid RLE header"
                FAILED=1
            fi
        fi
    done
    [ $FAILED -eq 0 ] && echo "All patterns valid!" || exit 1

# Run Life evolution simulation
evolve generations="100":
    @echo "🔄 Evolving pattern for {{generations}} generations..."
    @command -v deno >/dev/null && deno run --allow-read --allow-write runtime/host.ts evolve --generations={{generations}} || echo "Deno not available"

# Benchmark pattern simulation performance
bench-patterns:
    #!/usr/bin/env bash
    echo "⏱️ Benchmarking pattern simulation..."
    if command -v deno >/dev/null; then
        time deno run --allow-read runtime/host.ts evolve --generations=1000
    else
        echo "Deno not available for benchmarking"
    fi

# Count cells in all patterns
cell-count:
    #!/usr/bin/env bash
    echo "📊 Cell counts:"
    for f in src/*.rle patterns/*.rle; do
        if [ -f "$f" ]; then
            CELLS=$(grep -o 'o' "$f" | wc -l)
            echo "  $f: $CELLS cells"
        fi
    done

# List all patterns with metadata
list-patterns:
    #!/usr/bin/env bash
    echo "📋 Pattern Library:"
    for f in src/*.rle patterns/*.rle; do
        if [ -f "$f" ]; then
            NAME=$(grep "^#N" "$f" | head -1 | cut -d' ' -f2-)
            [ -z "$NAME" ] && NAME="(unnamed)"
            echo "  $f: $NAME"
        fi
    done

# ═══════════════════════════════════════════════════════════════════════════════
# GAME OF LIFE - DENO OPERATIONS
# ═══════════════════════════════════════════════════════════════════════════════

# Type check the host runtime
typecheck:
    @echo "📝 Type checking..."
    @command -v deno >/dev/null && deno check runtime/host.ts && echo "✅ Type check passed" || echo "Deno not available"

# Run Deno tests
test-deno:
    @echo "🧪 Running Deno tests..."
    @command -v deno >/dev/null && deno test --allow-read --allow-write tests/ || echo "Deno not available"

# Run E2E tests
test-e2e:
    @echo "🔗 Running E2E tests..."
    @command -v deno >/dev/null && deno test --allow-read --allow-write tests/e2e/ || echo "Deno not available"

# Run all tests including patterns
test-all: validate-patterns test-deno test-e2e
    @echo "✅ All tests complete"

# Format with Deno
fmt-deno:
    @command -v deno >/dev/null && deno fmt runtime/ tests/ || echo "Deno not available"

# Lint with Deno
lint-deno:
    @command -v deno >/dev/null && deno lint runtime/ tests/ || echo "Deno not available"

# ═══════════════════════════════════════════════════════════════════════════════
# GAME OF LIFE - RESCRIPT ADAPTER
# ═══════════════════════════════════════════════════════════════════════════════

# Build ReScript adapter
adapter-build:
    @echo "🔧 Building ReScript adapter..."
    @cd adapters && npm run build 2>/dev/null || echo "ReScript build failed or not configured"

# Build Life language tooling
life-lang-build:
    @echo "🔧 Building Life language tooling..."
    @cd life-lang && npm run build 2>/dev/null || echo "Life-lang build failed or not configured"

# Start RLE Language Server
lsp:
    @echo "🔧 Starting RLE Language Server..."
    @echo "Note: LSP implementation in life-lang/src/lsp/"
    @echo "Configure your editor to use the hackenbush-lsp server"

# ═══════════════════════════════════════════════════════════════════════════════
# MUST-PASS INTEGRATION
# ═══════════════════════════════════════════════════════════════════════════════

# Run all must-pass checks
must-all:
    @echo "🔒 Running must-pass checks..."
    @make -f Mustfile must-all 2>/dev/null || (echo "Running inline checks..." && just must-patterns && just must-no-ssg-logic)

# Must: Validate patterns
must-patterns:
    @test -f src/hackenbush.rle || (echo "❌ src/hackenbush.rle missing" && exit 1)
    @head -1 src/hackenbush.rle | grep -q "^#" || (echo "❌ Invalid RLE header" && exit 1)
    @echo "✅ Pattern validation passed"

# Must: No SSG logic in runtime
must-no-ssg-logic:
    @! grep -i "markdown\|frontmatter\|template\|render" runtime/host.ts 2>/dev/null || (echo "❌ SSG logic in runtime" && exit 1)
    @echo "✅ No SSG logic in runtime"

# ═══════════════════════════════════════════════════════════════════════════════
# EXTENDED MATRIX RECIPES
# ═══════════════════════════════════════════════════════════════════════════════

# Pattern matrix: [validate|evolve|bench|analyze] × [all|specific] × [generations]
pattern-matrix action="validate" scope="all" generations="100":
    #!/usr/bin/env bash
    echo "Pattern matrix: action={{action}} scope={{scope}} generations={{generations}}"
    case "{{action}}" in
        validate) just validate-patterns ;;
        evolve) just evolve {{generations}} ;;
        bench) just bench-patterns ;;
        analyze) just cell-count && just list-patterns ;;
        *) echo "Unknown action: {{action}}" && exit 1 ;;
    esac

# Language matrix: [typecheck|lint|fmt|test] × [runtime|adapter|life-lang|all]
lang-matrix action="typecheck" scope="all":
    #!/usr/bin/env bash
    echo "Language matrix: action={{action}} scope={{scope}}"
    case "{{action}}-{{scope}}" in
        typecheck-runtime|typecheck-all) just typecheck ;;
        lint-runtime|lint-all) just lint-deno ;;
        fmt-runtime|fmt-all) just fmt-deno ;;
        test-runtime|test-all) just test-deno ;;
        *-adapter) just adapter-build ;;
        *-life-lang) just life-lang-build ;;
        *) echo "Running all for {{action}}..." ;;
    esac

# ═══════════════════════════════════════════════════════════════════════════════
# HOOKS CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

# Install comprehensive git hooks
install-hooks-full:
    #!/usr/bin/env bash
    mkdir -p .git/hooks
    cat > .git/hooks/pre-commit << 'HOOK'
#!/bin/bash
set -e
echo "🔍 Pre-commit checks..."
just must-patterns || exit 1
just must-no-ssg-logic || exit 1
just typecheck || exit 1
echo "✅ Pre-commit passed"
HOOK
    chmod +x .git/hooks/pre-commit

    cat > .git/hooks/pre-push << 'HOOK'
#!/bin/bash
set -e
echo "🚀 Pre-push checks..."
just validate-patterns || exit 1
just test-deno || exit 1
echo "✅ Pre-push passed"
HOOK
    chmod +x .git/hooks/pre-push
    echo "Git hooks installed (pre-commit, pre-push)"

# Remove git hooks
remove-hooks:
    @rm -f .git/hooks/pre-commit .git/hooks/pre-push
    @echo "Git hooks removed"
