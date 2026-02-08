# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
#
# NoteG SSG - Justfile
# Mill-based static site generator build commands

set shell := ["bash", "-euo", "pipefail", "-c"]
set dotenv-load := true

# Default recipe
default: help

# Project metadata
project := "noteg-ssg"
version := "0.1.0"

# Directories
engine_dir := "engine"
ssg_dir := "ssg"
lang_dir := "noteg-lang"
content_dir := "content"
output_dir := "public"
test_dir := "tests"

# ============================================================================
# HELP
# ============================================================================

# Show available commands
help:
    @echo "NoteG SSG - Mill-Based Static Site Generator"
    @echo "============================================="
    @echo ""
    @just --list --unsorted

# ============================================================================
# BUILD COMMANDS
# ============================================================================

# Build everything
build: build-engine build-ssg build-lang
    @echo "✓ Build complete"

# Build Ada/SPARK engine
build-engine:
    @echo "Building Ada/SPARK engine..."
    cd {{engine_dir}} && gprbuild -P noteg_engine.gpr -XMODE=release

# Build with SPARK verification
build-spark:
    @echo "Building with SPARK verification..."
    cd {{engine_dir}} && gnatprove -P noteg_engine.gpr --level=2

# Build ReScript SSG
build-ssg:
    @echo "Building ReScript SSG..."
    cd {{ssg_dir}} && npx rescript build

# Build NoteG language tooling
build-lang:
    @echo "Building NoteG language..."
    cd {{lang_dir}} && npx rescript build

# Clean build artifacts
clean:
    @echo "Cleaning..."
    rm -rf {{engine_dir}}/obj {{engine_dir}}/bin
    cd {{ssg_dir}} && npx rescript clean
    cd {{lang_dir}} && npx rescript clean
    rm -rf {{output_dir}}

# ============================================================================
# TEST COMMANDS
# ============================================================================

# Run all tests
test: test-unit test-integration
    @echo "✓ All tests passed"

# Run unit tests
test-unit:
    @echo "Running unit tests..."
    cd {{ssg_dir}} && npx rescript build && node tests/unit_test.mjs
    cd {{lang_dir}} && npx rescript build && node tests/unit_test.mjs

# Run integration tests
test-integration:
    @echo "Running integration tests..."
    cd {{test_dir}} && deno test --allow-read --allow-write integration/

# Run end-to-end tests
test-e2e:
    @echo "Running e2e tests..."
    cd {{test_dir}}/e2e && deno test --allow-all

# Run all tests including e2e
test-all: test test-e2e
    @echo "✓ All tests including e2e passed"

# Run Bernoulli verification
test-bernoulli trials="1000":
    @echo "Running Bernoulli verification with {{trials}} trials..."
    cd {{ssg_dir}} && node tests/bernoulli_test.mjs --trials={{trials}}

# ============================================================================
# LANGUAGE SERVER
# ============================================================================

# Start NoteG language server
lsp:
    @echo "Starting NoteG Language Server..."
    cd {{lang_dir}} && node src/lsp/server.mjs

# Start LSP in debug mode
lsp-debug:
    @echo "Starting NoteG LSP in debug mode..."
    NOTEG_DEBUG=true cd {{lang_dir}} && node --inspect src/lsp/server.mjs

# ============================================================================
# COMPILE COMMANDS
# ============================================================================

# Compile a .noteg file
compile file:
    @echo "Compiling {{file}}..."
    cd {{lang_dir}} && node src/cli.mjs compile "{{file}}"

# Run a .noteg file
run file:
    @echo "Running {{file}}..."
    cd {{lang_dir}} && node src/cli.mjs run "{{file}}"

# Check a .noteg file for errors
check file:
    @echo "Checking {{file}}..."
    cd {{lang_dir}} && node src/cli.mjs check "{{file}}"

# ============================================================================
# SITE GENERATION
# ============================================================================

# Generate site
generate:
    @echo "Generating site..."
    cd {{ssg_dir}} && node src/cli.mjs build

# Serve site locally
serve port="8080":
    @echo "Serving site on port {{port}}..."
    cd {{output_dir}} && python3 -m http.server {{port}}

# Watch for changes and rebuild
watch:
    @echo "Watching for changes..."
    watchexec -e res,md,json,noteg just generate

# ============================================================================
# CONTAINER COMMANDS
# ============================================================================

# Build container image
container-build:
    @echo "Building container..."
    podman build -t {{project}}:{{version}} .

# Run container
container-run:
    @echo "Running container..."
    podman run --rm -it -v $(pwd)/content:/app/content:ro -v $(pwd)/public:/app/public {{project}}:{{version}}

# ============================================================================
# DEVELOPMENT
# ============================================================================

# Install dependencies
deps:
    @echo "Installing dependencies..."
    cd {{ssg_dir}} && npm install
    cd {{lang_dir}} && npm install

# Format code
fmt:
    @echo "Formatting code..."
    cd {{ssg_dir}} && npx rescript format -all
    cd {{lang_dir}} && npx rescript format -all
    deno fmt adapters/

# Lint code
lint:
    @echo "Linting code..."
    cd {{ssg_dir}} && npx rescript build 2>&1 | grep -i warning || true
    cd {{lang_dir}} && npx rescript build 2>&1 | grep -i warning || true
    deno lint adapters/

# Type check
typecheck:
    @echo "Type checking..."
    cd {{ssg_dir}} && npx rescript build
    cd {{lang_dir}} && npx rescript build

# ============================================================================
# SECURITY
# ============================================================================

# Run security audit
audit:
    @echo "Running security audit..."
    cd {{ssg_dir}} && npm audit
    cd {{lang_dir}} && npm audit

# Check for vulnerable dependencies
check-deps:
    @echo "Checking dependencies..."
    cd {{ssg_dir}} && npm outdated || true
    cd {{lang_dir}} && npm outdated || true

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate documentation
docs:
    @echo "Generating documentation..."
    asciidoctor docs/*.adoc -D {{output_dir}}/docs

# Serve documentation
docs-serve: docs
    cd {{output_dir}}/docs && python3 -m http.server 8081
