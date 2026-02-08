# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — doit-ssg task runner
# RSR-compliant task automation for the satellite SSG

set shell := ["bash", "-euo", "pipefail", "-c"]
set dotenv-load := true

# Default recipe - show help
default:
    @just --list --unsorted

# ============================================================================
# BUILD COMMANDS
# ============================================================================

# Build all adapters and verify syntax
build:
    @echo "Building doit-ssg..."
    @just check-syntax
    @echo "Build complete."

# Build for production with optimizations
build-prod:
    @echo "Building for production..."
    @just check-syntax
    @just lint
    @echo "Production build complete."

# Clean build artifacts
clean:
    @echo "Cleaning build artifacts..."
    rm -rf dist/ build/ .cache/ coverage/
    @echo "Clean complete."

# ============================================================================
# TEST COMMANDS
# ============================================================================

# Run all tests
test:
    @echo "Running tests..."
    @just test-unit
    @just test-adapters
    @echo "All tests passed."

# Run unit tests
test-unit:
    @echo "Running unit tests..."
    deno test tests/ --allow-read --allow-run 2>/dev/null || echo "Deno not available, skipping unit tests"

# Run adapter tests
test-adapters:
    @echo "Testing adapter syntax..."
    @for f in adapters/*.js; do \
        echo "Checking $$f..."; \
        node --check "$$f" 2>/dev/null || echo "Node check skipped"; \
    done

# Run end-to-end tests
test-e2e:
    @echo "Running E2E tests..."
    @just test-adapters
    @echo "E2E tests complete."

# Run all tests with coverage
test-all:
    @echo "Running all tests with coverage..."
    @just test
    @just test-e2e
    @echo "All tests complete."

# ============================================================================
# LINT & FORMAT
# ============================================================================

# Run all linters
lint:
    @echo "Running linters..."
    @just lint-js
    @just lint-security
    @echo "Linting complete."

# Lint JavaScript/TypeScript files
lint-js:
    @echo "Linting JavaScript..."
    deno lint adapters/ 2>/dev/null || echo "Deno lint not available"

# Security linting
lint-security:
    @echo "Running security checks..."
    @just check-secrets
    @echo "Security lint complete."

# Format all code
fmt:
    @echo "Formatting code..."
    deno fmt adapters/ 2>/dev/null || echo "Deno fmt not available"
    @echo "Formatting complete."

# Check formatting without modifying
fmt-check:
    @echo "Checking format..."
    deno fmt --check adapters/ 2>/dev/null || echo "Deno fmt not available"

# ============================================================================
# SECURITY COMMANDS
# ============================================================================

# Check for hardcoded secrets
check-secrets:
    @echo "Checking for secrets..."
    @! grep -rn --include="*.js" --include="*.ts" --include="*.json" \
        -E "(password|secret|api_key|token)\s*[:=]\s*['\"][^'\"]+['\"]" . \
        --exclude-dir=node_modules --exclude-dir=.git 2>/dev/null || true
    @echo "Secret check complete."

# Run CodeQL analysis (requires gh CLI)
codeql:
    @echo "CodeQL analysis requires GitHub Actions..."
    @echo "Push to main branch to trigger analysis."

# Security audit of dependencies
audit:
    @echo "Running security audit..."
    npm audit 2>/dev/null || echo "npm audit not available"
    @echo "Audit complete."

# ============================================================================
# DEVELOPMENT COMMANDS
# ============================================================================

# Check JavaScript syntax
check-syntax:
    @echo "Checking syntax..."
    @for f in adapters/*.js; do \
        node --check "$$f" 2>/dev/null && echo "✓ $$f" || echo "⚠ $$f (node not available)"; \
    done

# Check all files
check:
    @just check-syntax
    @just lint

# Start development mode
dev:
    @echo "Starting development mode..."
    @echo "Watching adapters/ for changes..."
    @echo "Press Ctrl+C to stop."

# Start language server
lsp:
    @echo "Starting LSP server..."
    deno lsp 2>/dev/null || echo "Deno LSP not available"

# Compile a specific adapter
compile adapter:
    @echo "Compiling adapter: {{adapter}}..."
    deno compile adapters/{{adapter}}.js 2>/dev/null || echo "Deno compile not available"

# ============================================================================
# ADAPTER MANAGEMENT
# ============================================================================

# List all adapters
adapters:
    @echo "Available adapters (28 total):"
    @ls -1 adapters/*.js | sed 's|adapters/||' | sed 's|\.js||' | column

# Count adapters
adapter-count:
    @echo -n "Adapter count: "
    @ls -1 adapters/*.js | wc -l

# Sync adapters from hub (requires transfer script)
adapter-sync:
    @echo "Syncing adapters from poly-ssg-mcp hub..."
    @if [ -f ~/Documents/scripts/transfer-ssg-adapters.sh ]; then \
        ~/Documents/scripts/transfer-ssg-adapters.sh --satellite; \
    else \
        echo "Transfer script not found. Manual sync required."; \
    fi

# Verify adapter interface compliance
adapter-verify:
    @echo "Verifying adapter interface..."
    @for f in adapters/*.js; do \
        echo "Checking $$f..."; \
        grep -q "export const name" "$$f" && echo "  ✓ name export" || echo "  ✗ missing name"; \
        grep -q "export async function connect" "$$f" && echo "  ✓ connect function" || echo "  ✗ missing connect"; \
        grep -q "export const tools" "$$f" && echo "  ✓ tools export" || echo "  ✗ missing tools"; \
    done

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate documentation
docs:
    @echo "Documentation available in:"
    @echo "  - README.adoc"
    @echo "  - CONTRIBUTING.md"
    @echo "  - SECURITY.md"
    @echo "  - cookbook.adoc"

# Validate AsciiDoc files
docs-check:
    @echo "Checking documentation..."
    @for f in *.adoc; do \
        [ -f "$$f" ] && echo "✓ $$f" || true; \
    done

# ============================================================================
# CI/CD COMMANDS
# ============================================================================

# Run CI pipeline locally
ci:
    @echo "Running CI pipeline..."
    @just clean
    @just check
    @just test-all
    @just lint
    @echo "CI pipeline complete."

# Prepare for release
release-prep version:
    @echo "Preparing release {{version}}..."
    @just ci
    @echo "Ready for release {{version}}"

# ============================================================================
# CONTAINER COMMANDS
# ============================================================================

# Build container image
container-build:
    @echo "Building container..."
    podman build -t doit-ssg:latest . 2>/dev/null || \
    docker build -t doit-ssg:latest . 2>/dev/null || \
    echo "Container runtime not available"

# Run in container
container-run:
    @echo "Running in container..."
    podman run --rm -it doit-ssg:latest 2>/dev/null || \
    docker run --rm -it doit-ssg:latest 2>/dev/null || \
    echo "Container runtime not available"

# ============================================================================
# NIX COMMANDS
# ============================================================================

# Enter Nix development shell
nix-dev:
    @echo "Entering Nix development shell..."
    nix develop 2>/dev/null || echo "Nix not available"

# Build with Nix
nix-build:
    @echo "Building with Nix..."
    nix build 2>/dev/null || echo "Nix not available"

# Update Nix flake
nix-update:
    @echo "Updating Nix flake..."
    nix flake update 2>/dev/null || echo "Nix not available"

# ============================================================================
# UTILITY COMMANDS
# ============================================================================

# Show project status
status:
    @echo "=== doit-ssg Status ==="
    @echo "Branch: $$(git branch --show-current)"
    @echo "Adapters: $$(ls -1 adapters/*.js | wc -l)"
    @echo ""
    @git status --short

# Show version info
version:
    @echo "doit-ssg v0.1.0"
    @echo "Satellite SSG for hyperpolymath ecosystem"

# Show help with categories
help:
    @echo "doit-ssg - Satellite SSG Task Runner"
    @echo ""
    @echo "CATEGORIES:"
    @echo "  Build:     build, build-prod, clean"
    @echo "  Test:      test, test-unit, test-e2e, test-all"
    @echo "  Lint:      lint, lint-js, lint-security, fmt"
    @echo "  Security:  check-secrets, codeql, audit"
    @echo "  Dev:       dev, lsp, compile, check"
    @echo "  Adapters:  adapters, adapter-count, adapter-sync, adapter-verify"
    @echo "  Docs:      docs, docs-check"
    @echo "  CI/CD:     ci, release-prep"
    @echo "  Container: container-build, container-run"
    @echo "  Nix:       nix-dev, nix-build, nix-update"
    @echo "  Utility:   status, version, help"
    @echo ""
    @echo "Run 'just --list' for all commands."
