# SPDX-License-Identifier: AGPL-3.0-or-later
# Justfile for zigzag-ssg - Zig Static Site Generator
# See: https://github.com/casey/just

# Default recipe - show available commands
default:
    @just --list

# ============================================================================
# BUILD RECIPES
# ============================================================================

# Build the zigzag-ssg binary
build:
    zig build

# Build optimized release binary
release:
    zig build -Doptimize=ReleaseFast

# Build with debug info
debug:
    zig build -Doptimize=Debug

# Build for specific target (cross-compilation)
build-target target:
    zig build -Dtarget={{target}}

# Clean build artifacts
clean:
    rm -rf zig-cache zig-out

# ============================================================================
# TEST RECIPES
# ============================================================================

# Run all unit tests
test:
    zig build test

# Run tests with verbose output
test-verbose:
    zig build test 2>&1 | cat

# Run end-to-end tests
test-e2e:
    zig build run -- test-full

# Run markdown parser test
test-markdown:
    zig build run -- test-markdown

# Run frontmatter parser test
test-frontmatter:
    zig build run -- test-frontmatter

# Run all tests (unit + e2e)
test-all: test test-e2e

# ============================================================================
# RUN RECIPES
# ============================================================================

# Build and run with default args
run *args:
    zig build run -- {{args}}

# Run in watch mode (future)
watch:
    @echo "Watch mode not yet implemented"
    @echo "See STATE.scm roadmap v1.3"

# ============================================================================
# FORMAT & LINT RECIPES
# ============================================================================

# Format all Zig source files
fmt:
    zig fmt src/

# Check formatting without modifying
fmt-check:
    zig fmt --check src/

# Lint (using Zig's built-in checks)
lint: build
    @echo "Lint passed (Zig compiler checks)"

# ============================================================================
# ADAPTER RECIPES
# ============================================================================

# Build ReScript MCP adapter
adapter-build:
    cd adapters && npm install && npm run build

# Clean adapter build artifacts
adapter-clean:
    cd adapters && npm run clean

# Watch adapter for changes
adapter-watch:
    cd adapters && npm run watch

# ============================================================================
# DOCUMENTATION RECIPES
# ============================================================================

# Generate HTML documentation
docs:
    @command -v asciidoctor >/dev/null && asciidoctor *.adoc docs/*.adoc 2>/dev/null || echo "asciidoctor not found, skipping"

# Serve documentation locally (if python available)
docs-serve:
    @echo "Serving docs at http://localhost:8000"
    python3 -m http.server 8000

# ============================================================================
# CONTAINER RECIPES
# ============================================================================

# Build container image
container:
    podman build -t zigzag-ssg .

# Build with docker (alias)
docker:
    docker build -t zigzag-ssg .

# Run container
container-run:
    podman run --rm -it zigzag-ssg

# ============================================================================
# QUALITY RECIPES
# ============================================================================

# Run all quality checks
check: fmt-check test build
    @echo "All checks passed!"

# Pre-commit check
pre-commit: fmt-check lint test
    @echo "Ready to commit"

# Full CI simulation
ci: check test-e2e
    @echo "CI simulation complete"

# ============================================================================
# RELEASE RECIPES
# ============================================================================

# Prepare for release
release-prep version:
    @echo "Preparing release {{version}}"
    @grep -q "version.*{{version}}" STATE.scm || echo "Update STATE.scm version!"
    @test -f CHANGELOG.md || echo "Create CHANGELOG.md!"

# Create release tag
release-tag version:
    git tag -a v{{version}} -m "Release v{{version}}"
    git push origin v{{version}}

# ============================================================================
# UTILITY RECIPES
# ============================================================================

# Show project info
info:
    @echo "zigzag-ssg - Zig Static Site Generator"
    @echo "Language: Zig (mandatory)"
    @echo "Zig version:"
    @zig version || echo "Zig not installed"

# Show component status from STATE.scm
status:
    @echo "Project Status:"
    @grep -A2 "state-summary" STATE.scm | tail -n 8

# Install development tools
setup:
    @echo "Setting up development environment..."
    @command -v zig >/dev/null || echo "Install Zig: https://ziglang.org/download/"
    @command -v just >/dev/null || echo "Install Just: cargo install just"
    @command -v npm >/dev/null || echo "Install Node.js for adapter development"

# Verify language compliance
language-check:
    @echo "Checking for forbidden languages in src/..."
    @! find src/ -type f \( -name "*.py" -o -name "*.js" -o -name "*.ts" -o -name "*.rs" -o -name "*.rb" -o -name "*.go" \) 2>/dev/null | grep . || echo "OK: Only Zig files in src/"

# ============================================================================
# NICKEL INTEGRATION (Future)
# ============================================================================

# Evaluate Nickel configuration (when available)
nickel-eval config="zigzag.ncl":
    @command -v nickel >/dev/null && nickel eval {{config}} || echo "Nickel not installed"

# ============================================================================
# COMBINATORICS - Command Combinations
# ============================================================================

# Build + Test + Format
btf: build test fmt

# Clean + Build + Test
cbt: clean build test

# Full development cycle
dev: clean build test fmt docs
    @echo "Development cycle complete"

# Quick iteration
quick: fmt build run

# ============================================================================
# ALIASES
# ============================================================================

alias b := build
alias t := test
alias r := run
alias f := fmt
alias c := check
alias w := watch
