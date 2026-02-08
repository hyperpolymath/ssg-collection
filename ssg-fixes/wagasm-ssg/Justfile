# SPDX-License-Identifier: AGPL-3.0-or-later
# wagasm-ssg Justfile - Pure WebAssembly Text SSG
# https://github.com/casey/just

set shell := ["bash", "-euo", "pipefail", "-c"]
set dotenv-load := true

# Project metadata
project := "wagasm-ssg"
version := "0.1.0"
wat_source := "src/wagasm-ssg.wat"
wasm_output := "src/wagasm-ssg.wasm"
host_runtime := "runtime/host.ts"
output_dir := "_site"

# Colors for output
red := '\033[0;31m'
green := '\033[0;32m'
yellow := '\033[0;33m'
blue := '\033[0;34m'
nc := '\033[0m'

# Default recipe - show help
default: help

# =============================================================================
# HELP & INFO
# =============================================================================

# Show available recipes
help:
    @echo -e "{{blue}}wagasm-ssg{{nc}} - Pure WebAssembly Text SSG"
    @echo ""
    @echo "Usage: just <recipe>"
    @echo ""
    @just --list --unsorted

# Show project version
version:
    @echo "{{project}} v{{version}} (EXPERIMENTAL)"
    @echo "Language: WebAssembly Text (WAT)"

# Show tool versions
tools:
    @echo "=== Tool Versions ==="
    @wat2wasm --version 2>/dev/null || echo "wat2wasm: NOT INSTALLED"
    @wasm-validate --version 2>/dev/null || echo "wasm-validate: NOT INSTALLED"
    @deno --version 2>/dev/null || echo "deno: NOT INSTALLED"

# =============================================================================
# BUILD RECIPES
# =============================================================================

# Compile WAT to WASM
compile:
    @echo -e "{{blue}}Compiling WAT to WASM...{{nc}}"
    wat2wasm {{wat_source}} -o {{wasm_output}}
    @echo -e "{{green}}Compiled: {{wasm_output}}{{nc}}"

# Compile with debug names
compile-debug:
    @echo -e "{{blue}}Compiling WAT with debug names...{{nc}}"
    wat2wasm --debug-names {{wat_source}} -o {{wasm_output}}
    @echo -e "{{green}}Compiled with debug symbols{{nc}}"

# Validate WASM binary
validate: compile
    @echo -e "{{blue}}Validating WASM...{{nc}}"
    wasm-validate {{wasm_output}}
    @echo -e "{{green}}WASM validation passed{{nc}}"

# Build the site (compile + run)
build: compile
    @echo -e "{{blue}}Building site...{{nc}}"
    deno run --allow-read --allow-write {{host_runtime}}
    @echo -e "{{green}}Site built in {{output_dir}}/{{nc}}"

# Run without recompiling
run:
    @echo -e "{{blue}}Running SSG...{{nc}}"
    deno run --allow-read --allow-write {{host_runtime}}

# Clean build artifacts
clean:
    @echo -e "{{yellow}}Cleaning build artifacts...{{nc}}"
    rm -rf {{wasm_output}} {{output_dir}}/
    @echo -e "{{green}}Clean complete{{nc}}"

# Full rebuild
rebuild: clean build

# =============================================================================
# TEST RECIPES
# =============================================================================

# Run all tests
test: test-compile test-validate test-language test-build
    @echo -e "{{green}}All tests passed!{{nc}}"

# Test WAT compilation
test-compile:
    @echo -e "{{blue}}Testing WAT compilation...{{nc}}"
    wat2wasm {{wat_source}} -o /dev/null
    @echo -e "{{green}}Compilation test passed{{nc}}"

# Test WASM validation
test-validate: compile
    @echo -e "{{blue}}Testing WASM validation...{{nc}}"
    wasm-validate {{wasm_output}}
    @echo -e "{{green}}Validation test passed{{nc}}"

# Test language compliance (no forbidden files in src/)
test-language:
    @echo -e "{{blue}}Testing language compliance...{{nc}}"
    @forbidden=$$(find src/ -type f \( -name "*.ts" -o -name "*.js" -o -name "*.rs" -o -name "*.py" \) 2>/dev/null || true); \
    if [ -n "$$forbidden" ]; then \
        echo -e "{{red}}ERROR: Found FORBIDDEN language files:{{nc}}"; \
        echo "$$forbidden"; \
        exit 1; \
    fi
    @echo -e "{{green}}Language compliance test passed (WAT only in src/){{nc}}"

# Test build output
test-build: build
    @echo -e "{{blue}}Testing build output...{{nc}}"
    @if [ ! -f {{output_dir}}/index.html ]; then \
        echo -e "{{red}}ERROR: index.html not generated{{nc}}"; \
        exit 1; \
    fi
    @echo -e "{{green}}Build output test passed{{nc}}"

# Run end-to-end tests
test-e2e: test
    @echo -e "{{blue}}Running E2E tests...{{nc}}"
    @if [ -d tests/e2e ]; then \
        for test in tests/e2e/*.sh; do \
            echo "Running $$test..."; \
            bash "$$test" || exit 1; \
        done; \
    else \
        echo "No E2E tests found"; \
    fi
    @echo -e "{{green}}E2E tests passed{{nc}}"

# Run all tests including E2E
test-all: test test-e2e

# =============================================================================
# DEVELOPMENT RECIPES
# =============================================================================

# Watch for changes and rebuild
watch:
    @echo -e "{{blue}}Watching for changes...{{nc}}"
    @echo "Press Ctrl+C to stop"
    @while true; do \
        inotifywait -q -e modify {{wat_source}} {{host_runtime}} 2>/dev/null || sleep 2; \
        just build; \
    done

# Disassemble WASM to WAT (for debugging)
disassemble:
    @echo -e "{{blue}}Disassembling WASM...{{nc}}"
    wasm2wat {{wasm_output}} -o {{wasm_output}}.disasm.wat
    @echo -e "{{green}}Disassembled to {{wasm_output}}.disasm.wat{{nc}}"

# Show WASM info
info: compile
    @echo -e "{{blue}}WASM Module Info:{{nc}}"
    wasm-objdump -h {{wasm_output}}

# Dump WASM sections
dump: compile
    @echo -e "{{blue}}WASM Section Dump:{{nc}}"
    wasm-objdump -x {{wasm_output}}

# =============================================================================
# ADAPTER RECIPES
# =============================================================================

# Build ReScript adapter
adapter-build:
    @echo -e "{{blue}}Building ReScript adapter...{{nc}}"
    cd adapters && npm run build

# Clean adapter build
adapter-clean:
    @echo -e "{{yellow}}Cleaning adapter...{{nc}}"
    cd adapters && npm run clean

# Install adapter dependencies
adapter-install:
    @echo -e "{{blue}}Installing adapter dependencies...{{nc}}"
    cd adapters && npm install

# =============================================================================
# LINT & FORMAT
# =============================================================================

# Check for issues
lint: test-language
    @echo -e "{{blue}}Running lints...{{nc}}"
    @# Check host runtime doesn't contain SSG logic
    @if grep -qE 'generate|template|markdown|parse|route' {{host_runtime}} 2>/dev/null; then \
        echo -e "{{yellow}}WARNING: Host runtime may contain SSG logic{{nc}}"; \
    fi
    @echo -e "{{green}}Lint complete{{nc}}"

# =============================================================================
# CI/CD RECIPES
# =============================================================================

# CI pipeline (what GitHub Actions runs)
ci: lint test
    @echo -e "{{green}}CI pipeline passed{{nc}}"

# Pre-commit checks
pre-commit: lint test-compile test-language
    @echo -e "{{green}}Pre-commit checks passed{{nc}}"

# Pre-push checks
pre-push: test
    @echo -e "{{green}}Pre-push checks passed{{nc}}"

# =============================================================================
# RELEASE RECIPES
# =============================================================================

# Prepare release
release-prepare version:
    @echo -e "{{blue}}Preparing release {{version}}...{{nc}}"
    @sed -i 's/version . "[^"]*"/version . "{{version}}"/' STATE.scm
    @echo -e "{{green}}Updated STATE.scm to v{{version}}{{nc}}"

# Tag release
release-tag version:
    git tag -a "v{{version}}" -m "Release v{{version}}"
    @echo -e "{{green}}Created tag v{{version}}{{nc}}"

# =============================================================================
# UTILITY RECIPES
# =============================================================================

# Show memory layout documentation
memory-layout:
    @echo "=== WASM Memory Layout ==="
    @echo "0-1023:     Generated HTML output buffer"
    @echo "1024-2047:  Title buffer"
    @echo "2048-4095:  Body content buffer"
    @echo "4096+:      Static string data segments"
    @echo "8192+:      Filename buffer"

# Generate documentation
docs:
    @echo -e "{{blue}}Documentation is in README.adoc{{nc}}"
    @ls -la *.adoc *.scm 2>/dev/null || true

# Show project status
status:
    @echo "=== Project Status ==="
    @grep -E "^\s+\(status\s+\." STATE.scm | head -1
    @grep -E "^\s+\(completion\s+\." STATE.scm | head -1
    @echo ""
    @echo "Files:"
    @wc -l src/*.wat runtime/*.ts 2>/dev/null || true
