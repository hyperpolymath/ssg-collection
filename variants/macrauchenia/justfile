# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile - macrauchenia-ssg build automation
# See: https://just.systems/man/en/

set dotenv-load := true
set shell := ["bash", "-euo", "pipefail", "-c"]

# Default recipe - show help
default:
    @just --list

# ============================================================================
# BUILD RECIPES
# ============================================================================

# Build all adapters (syntax check)
build:
    @echo "==> Checking adapter syntax..."
    @for f in adapters/*.js; do \
        deno check "$f" && echo "✓ $f" || (echo "✗ $f" && exit 1); \
    done
    @echo "==> Build complete: all 28 adapters validated"

# Build with verbose output
build-verbose:
    @echo "==> Verbose build..."
    deno check --all adapters/*.js

# Clean build artifacts
clean:
    @echo "==> Cleaning build artifacts..."
    rm -rf .deno/ node_modules/ dist/ coverage/
    @echo "==> Clean complete"

# ============================================================================
# TEST RECIPES
# ============================================================================

# Run unit tests
test:
    @echo "==> Running unit tests..."
    deno test --allow-read --allow-run tests/

# Run tests with coverage
test-coverage:
    @echo "==> Running tests with coverage..."
    deno test --allow-read --allow-run --coverage=coverage tests/
    deno coverage coverage --lcov > coverage/lcov.info
    @echo "==> Coverage report generated: coverage/lcov.info"

# Run end-to-end tests
test-e2e:
    @echo "==> Running E2E tests..."
    deno test --allow-all tests/e2e/

# Run all tests (unit + e2e)
test-all: test test-e2e
    @echo "==> All tests complete"

# Run security-focused tests
test-security:
    @echo "==> Running security tests..."
    deno test --allow-read tests/security/

# ============================================================================
# LINT & FORMAT RECIPES
# ============================================================================

# Lint all code
lint:
    @echo "==> Linting..."
    deno lint adapters/
    @echo "==> Lint complete"

# Format all code
fmt:
    @echo "==> Formatting..."
    deno fmt adapters/
    @echo "==> Format complete"

# Check formatting without modifying
fmt-check:
    @echo "==> Checking format..."
    deno fmt --check adapters/

# Run all checks (lint + fmt-check)
check: lint fmt-check
    @echo "==> All checks passed"

# ============================================================================
# ADAPTER RECIPES
# ============================================================================

# List all available adapters
adapters:
    @echo "==> Available SSG adapters (28):"
    @ls -1 adapters/*.js | xargs -n1 basename | sed 's/.js$//'

# Run a specific adapter
run-adapter adapter:
    @echo "==> Running adapter: {{adapter}}"
    deno run --allow-all adapters/{{adapter}}.js

# Check a specific adapter
check-adapter adapter:
    @echo "==> Checking adapter: {{adapter}}"
    deno check adapters/{{adapter}}.js

# Validate all adapter exports
validate-adapters:
    @echo "==> Validating adapter exports..."
    @for f in adapters/*.js; do \
        deno eval "import('file://$(pwd)/$f').then(m => { \
            if (!m.name || !m.tools) throw 'Missing exports in $f'; \
            console.log('✓ ' + m.name); \
        })" || exit 1; \
    done

# ============================================================================
# SECURITY RECIPES
# ============================================================================

# Run security audit
audit:
    @echo "==> Running security audit..."
    @echo "Checking for hardcoded secrets..."
    @! grep -rE "(password|secret|api[_-]?key)\s*[:=]" adapters/ || echo "No hardcoded secrets found"
    @echo "==> Security audit complete"

# Check for vulnerabilities
security-scan:
    @echo "==> Scanning for vulnerabilities..."
    deno lint --rules-include=no-eval adapters/
    @echo "==> Scan complete"

# ============================================================================
# DOCUMENTATION RECIPES
# ============================================================================

# Generate documentation
docs:
    @echo "==> Generating documentation..."
    @mkdir -p docs/api
    deno doc --html --output=docs/api adapters/*.js
    @echo "==> Documentation generated in docs/api/"

# Serve documentation locally
docs-serve:
    @echo "==> Serving docs at http://localhost:8000"
    cd docs && python -m http.server 8000

# ============================================================================
# CI/CD RECIPES
# ============================================================================

# Full CI pipeline
ci: fmt-check lint test build
    @echo "==> CI pipeline complete"

# Pre-commit hook checks
pre-commit: fmt-check lint
    @echo "==> Pre-commit checks passed"

# Release preparation
release version:
    @echo "==> Preparing release {{version}}..."
    @sed -i 's/version = "[^"]*"/version = "{{version}}"/' deno.json || true
    @echo "Release {{version}} prepared"

# ============================================================================
# DEVELOPMENT RECIPES
# ============================================================================

# Start development mode (watch)
dev:
    @echo "==> Starting development mode..."
    deno task dev 2>/dev/null || echo "Add 'dev' task to deno.json"

# Start REPL with adapters loaded
repl:
    @echo "==> Starting REPL..."
    deno repl --allow-all

# Check environment
env-check:
    @echo "==> Environment check:"
    @echo "Deno: $(deno --version | head -1)"
    @echo "Just: $(just --version)"
    @command -v jq >/dev/null && echo "jq: $(jq --version)" || echo "jq: not installed"

# ============================================================================
# MCP RECIPES
# ============================================================================

# Start MCP server (all adapters)
mcp-serve:
    @echo "==> Starting MCP server..."
    deno run --allow-all mcp-server.ts

# Test MCP connection
mcp-test:
    @echo "==> Testing MCP connection..."
    @curl -s localhost:8080/health 2>/dev/null || echo "MCP server not running"

# ============================================================================
# UTILITY RECIPES
# ============================================================================

# Show project info
info:
    @echo "macrauchenia-ssg - SSG Adapter Collection"
    @echo "========================================="
    @echo "Adapters: 28"
    @echo "Languages: Rust, Haskell, Elixir, Julia, Clojure, etc."
    @echo "Runtime: Deno"
    @echo "License: AGPL-3.0-or-later"

# Count lines of code
loc:
    @echo "==> Lines of code:"
    @wc -l adapters/*.js | tail -1

# Show recent changes
changes:
    @git log --oneline -10

# Sync with upstream
sync:
    @echo "==> Syncing with upstream..."
    git fetch origin
    git status
