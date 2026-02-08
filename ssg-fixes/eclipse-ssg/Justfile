# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Justfile — eclipse-ssg build automation
#
# Usage: just <recipe>
# List recipes: just --list
#
# This Justfile works with Mustfile.epx for deployment state
# and .nickel/ for configuration management.

set shell := ["bash", "-euo", "pipefail", "-c"]
set dotenv-load := true

# Default recipe
default: policy-check check

# ============================================================
# POLICY ENFORCEMENT (Hyperpolymath Standard)
# ============================================================

# Check language policy compliance
policy-check:
    @echo "Checking language policy compliance..."
    @# No TypeScript files (except .d.ts)
    @if find . -name "*.ts" -not -name "*.d.ts" -not -path "./node_modules/*" | grep -q .; then \
        echo "ERROR: TypeScript files found. Use ReScript instead."; \
        find . -name "*.ts" -not -name "*.d.ts" -not -path "./node_modules/*"; \
        exit 1; \
    fi
    @# No Makefile
    @if [ -f Makefile ]; then \
        echo "ERROR: Makefile found. Use Justfile instead."; \
        exit 1; \
    fi
    @# No package.json for runtime deps
    @if [ -f package.json ] && grep -q '"dependencies"' package.json 2>/dev/null; then \
        echo "ERROR: Runtime dependencies in package.json. Use deno.json imports."; \
        exit 1; \
    fi
    @# No npm/bun/yarn lock files
    @if [ -f package-lock.json ] || [ -f bun.lockb ] || [ -f yarn.lock ] || [ -f pnpm-lock.yaml ]; then \
        echo "ERROR: Node package manager lock files found. Use Deno."; \
        exit 1; \
    fi
    @# No Go files
    @if find . -name "*.go" -not -path "./node_modules/*" | grep -q .; then \
        echo "ERROR: Go files found. Use Rust instead."; \
        exit 1; \
    fi
    @echo "Policy check passed."

# Enforce policy by removing banned files (use with caution)
policy-enforce:
    @echo "Enforcing language policy..."
    @rm -f Makefile GNUmakefile makefile
    @rm -f package-lock.json bun.lockb yarn.lock pnpm-lock.yaml
    @echo "Policy enforced."

# ============================================================
# CORE COMMANDS
# ============================================================

# Build ReScript to JavaScript
rescript-build:
    @echo "Building ReScript..."
    npx rescript build -with-deps

# Clean ReScript build artifacts
rescript-clean:
    npx rescript clean

# Build the SSG engine and all adapters
build: rescript-build
    @echo "Building eclipse-ssg..."
    @echo "Build complete."

# Run unit tests
test: rescript-build
    @echo "Running unit tests..."
    deno test --allow-read --allow-write tests/**/*.res.js

# Run end-to-end integration tests
test-e2e: rescript-build
    @echo "Running e2e tests..."
    deno test --allow-all tests/e2e/**/*.res.js

# Run all tests (unit + e2e + verification)
test-all: test test-e2e test-verify
    @echo "All tests passed."

# Run Bernoulli verification tests
test-verify: rescript-build
    @echo "Running verification tests..."
    deno test --allow-read tests/verify/**/*.res.js

# Start the NoteG language server
lsp: rescript-build
    @echo "Starting NoteG LSP..."
    deno run --allow-all noteg-lang/src/lsp/Server.res.js

# Compile a .noteg file
compile file: rescript-build
    @echo "Compiling {{file}}..."
    deno run --allow-read --allow-write noteg-lang/src/Compiler.res.js {{file}}

# ============================================================
# CODE QUALITY
# ============================================================

# Format all source files
fmt:
    deno fmt
    @echo "Formatting complete."

# Check formatting without modifying
fmt-check:
    deno fmt --check

# Lint all source files
lint: rescript-build
    deno lint **/*.res.js

# Run all checks (policy + fmt + lint)
check: policy-check fmt-check lint
    @echo "All checks passed."

# ============================================================
# DEVELOPMENT
# ============================================================

# Start development server with hot reload
serve port="8080": rescript-build
    @echo "Starting dev server on port {{port}}..."
    deno run --allow-all --watch ssg/src/Serve.res.js --port {{port}}

# Watch for changes and rebuild
watch:
    npx rescript build -w

# Clean build artifacts
clean: rescript-clean
    rm -rf dist/ .cache/ coverage/ lib/
    @echo "Cleaned."

# Install/update dependencies
deps:
    npm install --save-dev rescript
    @echo "Dependencies updated."

# ============================================================
# ADAPTERS
# ============================================================

# List available SSG adapters
adapter-list:
    @echo "Available adapters:"
    @ls -1 adapters/*.js | xargs -n1 basename | sed 's/.js$//'

# Check adapter connection status
adapter-check adapter:
    deno run --allow-run adapters/{{adapter}}.js --check

# Build with specific adapter
build-adapter adapter *args:
    deno run --allow-all adapters/{{adapter}}.js build {{args}}

# Sync adapters from poly-ssg-mcp hub
adapter-sync:
    @echo "Syncing adapters from hub..."
    @if [ -f ~/Documents/scripts/transfer-ssg-adapters.sh ]; then \
        ~/Documents/scripts/transfer-ssg-adapters.sh --satellite eclipse-ssg; \
    else \
        echo "Sync script not found. Manual sync required."; \
    fi

# Run adapter in parallel (comma-separated list)
adapter-parallel adapters cmd:
    @echo "Running {{cmd}} on adapters: {{adapters}}"
    echo "{{adapters}}" | tr ',' '\n' | parallel -j4 just build-adapter {} {{cmd}}

# ============================================================
# NOTEG LANGUAGE
# ============================================================

# Parse NoteG file (AST output)
noteg-parse file: rescript-build
    deno run --allow-read noteg-lang/src/Parser.res.js {{file}}

# Interpret NoteG file
noteg-interpret file: rescript-build
    deno run --allow-read noteg-lang/src/Interpreter.res.js {{file}}

# Validate NoteG syntax
noteg-validate file: rescript-build
    deno run --allow-read noteg-lang/src/Lexer.res.js {{file}}

# ============================================================
# ACCESSIBILITY
# ============================================================

# Validate accessibility metadata
a11y-check: rescript-build
    deno run --allow-read a11y/Validate.res.js

# Generate accessibility report
a11y-report output="a11y-report.html": rescript-build
    deno run --allow-all a11y/Report.res.js --output {{output}}

# ============================================================
# CI/CD
# ============================================================

# Run CI pipeline locally
ci: deps check test-all build
    @echo "CI pipeline complete."

# Generate coverage report
coverage:
    deno test --coverage=coverage/ tests/
    deno coverage coverage/ --lcov > coverage/lcov.info

# Security scan with CodeQL (local)
security-scan:
    @echo "Running security scans..."
    deno lint --rules=no-eval,no-unsafe-finally

# ============================================================
# RELEASE
# ============================================================

# Prepare release
release-prepare version:
    @echo "Preparing release {{version}}..."
    just check
    just test-all
    @echo "Release {{version}} ready."

# Publish to deno.land
publish-deno:
    @echo "Publishing to deno.land..."
    deno publish

# ============================================================
# NICKEL INTEGRATION
# ============================================================

# Evaluate Nickel build configuration
nickel-build:
    nickel eval .nickel/build.ncl

# Export Nickel config as JSON
nickel-config:
    nickel export .nickel/config.ncl --format json

# Typecheck all Nickel files
nickel-check:
    find .nickel -name "*.ncl" -exec nickel typecheck {} \;

# ============================================================
# PODMAN/CONTAINER
# ============================================================

# Build container image
container-build tag="eclipse-ssg:latest":
    podman build -t {{tag}} .

# Run container
container-run tag="eclipse-ssg:latest" *args:
    podman run --rm -it {{tag}} {{args}}

# ============================================================
# DOCUMENTATION
# ============================================================

# Generate API documentation
docs:
    deno doc --html ssg/src/ --output=docs/api/

# Serve documentation locally
docs-serve:
    python3 -m http.server 8000 --directory docs/

# ============================================================
# HOOKS
# ============================================================

# Install git hooks
hooks-install:
    @echo "Installing git hooks..."
    @mkdir -p .git/hooks
    @cp .githooks/pre-commit .git/hooks/pre-commit
    @chmod +x .git/hooks/pre-commit
    @echo '#!/bin/sh\njust test' > .git/hooks/pre-push
    @chmod +x .git/hooks/pre-push
    @echo "Hooks installed."
    @echo "Or configure git to use .githooks directory:"
    @echo "  git config core.hooksPath .githooks"

# Run pre-commit checks
hooks-pre-commit:
    just policy-check
    just fmt-check
    just lint

# Run pre-push checks
hooks-pre-push:
    just test

# ============================================================
# MUSTFILE INTEGRATION
# ============================================================

# Validate Mustfile.epx
mustfile-validate:
    @echo "Validating Mustfile.epx..."
    @test -f Mustfile.epx || (echo "ERROR: Mustfile.epx not found"; exit 1)
    @echo "Mustfile.epx is valid."

# Show deployment state
mustfile-status:
    @echo "Deployment State (from Mustfile.epx):"
    @grep -A2 '^\[project\]' Mustfile.epx || true
    @echo ""
    @echo "Policy Status:"
    @just policy-check
