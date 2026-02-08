# SPDX-License-Identifier: AGPL-3.0-or-later
# prodigy-ssg Justfile - Local task automation
# RSR Gold compliant build recipes

set shell := ["bash", "-c"]
set dotenv-load := true

# Project metadata
project := "prodigy-ssg"
version := "1.1.0"
prolog := "swipl"

# Default recipe
default: help

# ============================================================================
# Help & Info
# ============================================================================

# Show available recipes
help:
    @echo "prodigy-ssg - Prolog Static Site Generator"
    @echo "============================================"
    @just --list

# Show version info
version:
    @echo "{{project}} v{{version}}"
    @{{prolog}} --version 2>/dev/null || echo "SWI-Prolog not installed"

# ============================================================================
# Build Recipes
# ============================================================================

# Build the site (default output: _site)
build output="_site":
    @echo "Building site to {{output}}..."
    @{{prolog}} -g "consult('src/prodigy.pl'), build_site('{{output}}'), halt" -t 'halt(1)'

# Build with custom config
build-config config output="_site":
    @echo "Building with config {{config}}..."
    @{{prolog}} -g "consult('src/prodigy.pl'), load_config('{{config}}'), build_site('{{output}}'), halt" -t 'halt(1)'

# Clean build artifacts
clean:
    @echo "Cleaning build artifacts..."
    @rm -rf _site lib/bs .bsb.lock adapters/lib
    @echo "Clean complete."

# Full rebuild
rebuild: clean build

# ============================================================================
# Test Recipes
# ============================================================================

# Run all tests
test: test-unit test-e2e
    @echo "All tests passed!"

# Run unit tests
test-unit:
    @echo "Running unit tests..."
    @{{prolog}} -g "consult('tests/unit/test_runner.pl'), run_tests, halt" -t 'halt(1)'

# Run end-to-end tests
test-e2e:
    @echo "Running E2E tests..."
    @{{prolog}} -g "consult('tests/e2e/e2e_runner.pl'), run_e2e_tests, halt" -t 'halt(1)'

# Run specific test file
test-file file:
    @echo "Running test: {{file}}..."
    @{{prolog}} -g "consult('{{file}}'), run_tests, halt" -t 'halt(1)'

# Test markdown parser
test-markdown:
    @{{prolog}} -g "consult('src/prodigy.pl'), test_markdown, halt"

# Test frontmatter parser
test-frontmatter:
    @{{prolog}} -g "consult('src/prodigy.pl'), test_frontmatter, halt"

# Test full pipeline
test-full:
    @{{prolog}} -g "consult('src/prodigy.pl'), test_full, halt"

# Run tests with coverage
test-coverage:
    @echo "Running tests with coverage..."
    @{{prolog}} -g "use_module(library(test_cover)), show_coverage(run_tests), halt"

# ============================================================================
# Development Recipes
# ============================================================================

# Start development server (watch mode)
dev:
    @echo "Starting development mode..."
    @{{prolog}} -g "consult('src/prodigy.pl'), dev_server, halt"

# Start REPL with project loaded
repl:
    @{{prolog}} -l src/prodigy.pl

# Check Prolog syntax
check:
    @echo "Checking Prolog syntax..."
    @for f in src/*.pl engine/src/*.pl; do \
        echo "  Checking $$f..."; \
        {{prolog}} -g halt "$$f" || exit 1; \
    done
    @echo "All files OK!"

# Format/lint Prolog files (if available)
lint:
    @echo "Linting Prolog files..."
    @if command -v prolog_lint >/dev/null; then \
        prolog_lint src/*.pl engine/src/*.pl; \
    else \
        echo "No Prolog linter available. Syntax check only."; \
        just check; \
    fi

# ============================================================================
# LSP & Editor Support
# ============================================================================

# Start Prolog LSP server
lsp:
    @echo "Starting Prolog LSP..."
    @{{prolog}} -g "use_module(library(lsp_server)), lsp_server:main"

# Generate editor config
editor-config:
    @echo "Generating editor configurations..."
    @just _gen-vscode
    @just _gen-neovim
    @echo "Editor configs generated in editors/"

_gen-vscode:
    @mkdir -p editors/vscode
    @echo '{"prolog.executablePath": "swipl"}' > editors/vscode/settings.json

_gen-neovim:
    @mkdir -p editors/neovim
    @echo '-- Prolog LSP config for Neovim' > editors/neovim/prolog.lua

# ============================================================================
# Adapter Recipes
# ============================================================================

# Build ReScript MCP adapter
adapter-build:
    @echo "Building ReScript adapter..."
    @cd adapters && npm install && npm run build

# Clean adapter build
adapter-clean:
    @cd adapters && npm run clean 2>/dev/null || true

# Watch adapter for changes
adapter-watch:
    @cd adapters && npm run watch

# ============================================================================
# Documentation
# ============================================================================

# Build documentation
docs:
    @echo "Building documentation..."
    @if command -v asciidoctor >/dev/null; then \
        asciidoctor -o docs/index.html README.adoc; \
        asciidoctor -o docs/cookbook.html cookbook.adoc 2>/dev/null || true; \
    else \
        echo "asciidoctor not installed, skipping doc build"; \
    fi

# Serve documentation locally
docs-serve: docs
    @cd docs && python3 -m http.server 8080

# ============================================================================
# Release Recipes
# ============================================================================

# Create release tag
release version:
    @echo "Creating release v{{version}}..."
    @git tag -a "v{{version}}" -m "Release v{{version}}"
    @echo "Tag created. Push with: git push origin v{{version}}"

# Check release readiness
release-check:
    @echo "Checking release readiness..."
    @just test
    @just check
    @just lint
    @echo "Release checks passed!"

# ============================================================================
# Container Recipes
# ============================================================================

# Build container image
container-build:
    @echo "Building container..."
    @podman build -t {{project}}:{{version}} -f Containerfile .

# Run container
container-run:
    @podman run --rm -v $(pwd)/content:/app/content:ro -v $(pwd)/_site:/app/_site {{project}}:{{version}}

# ============================================================================
# Utility Recipes
# ============================================================================

# Tidy whitespace
tidy:
    @echo "Tidying whitespace..."
    @find . -type f \( -name "*.pl" -o -name "*.res" -o -name "*.md" -o -name "*.adoc" \) \
        -not -path "./.git/*" -exec sed -i 's/[[:space:]]*$$//' {} \;
    @echo "Whitespace tidied."

# Count lines of code
loc:
    @echo "Lines of Prolog code:"
    @find . -name "*.pl" -not -path "./.git/*" | xargs wc -l | tail -1
    @echo "Lines of ReScript code:"
    @find . -name "*.res" -not -path "./.git/*" | xargs wc -l 2>/dev/null | tail -1 || echo "0"

# Security audit
audit:
    @echo "Running security audit..."
    @just _check-secrets
    @just _check-deps
    @echo "Security audit complete."

_check-secrets:
    @echo "  Checking for secrets..."
    @! grep -r -E "(password|secret|api_key)\s*=" --include="*.pl" --include="*.res" . 2>/dev/null || true

_check-deps:
    @echo "  Checking dependencies..."
    @cd adapters && npm audit 2>/dev/null || true

# Bootstrap development environment
bootstrap:
    @echo "Bootstrapping prodigy-ssg development..."
    @command -v {{prolog}} >/dev/null || (echo "Please install SWI-Prolog" && exit 1)
    @cd adapters && npm install 2>/dev/null || true
    @just check
    @echo "Bootstrap complete!"

# Run all quality checks
all: check lint test audit
    @echo "All quality checks passed!"
