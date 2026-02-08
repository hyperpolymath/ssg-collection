# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# odd-ssg Justfile - Task Runner Configuration
# Integrates with Mustfile.ncl for deployment transitions

# Default recipe - show help
default:
    @just --list

# ============================================================================
# RESCRIPT BUILD COMMANDS
# ============================================================================

# Build ReScript sources
rescript-build:
    npx rescript build

# Watch ReScript sources for changes
rescript-watch:
    npx rescript build -w

# Clean ReScript build artifacts
rescript-clean:
    npx rescript clean

# ============================================================================
# BUILD COMMANDS
# ============================================================================

# Build the project (ReScript + SSG)
build: rescript-build
    deno run --allow-read --allow-write src/ssg/cli.js build

# Build with verbose output
build-verbose: rescript-build
    deno run --allow-read --allow-write src/ssg/cli.js build --verbose

# Build including draft content
build-drafts: rescript-build
    deno run --allow-read --allow-write src/ssg/cli.js build --drafts

# Clean build artifacts
clean: rescript-clean
    rm -rf dist/ .cache/ coverage/ lib/
    @echo "Cleaned build artifacts"

# Watch for changes and rebuild
watch:
    deno task watch

# ============================================================================
# TEST COMMANDS
# ============================================================================

# Run all tests
test: rescript-build
    deno test --allow-read --allow-write src/tests/

# Run unit tests only
test-unit: rescript-build
    deno test --allow-read --allow-write src/tests/

# Run end-to-end tests
test-e2e: rescript-build
    deno test --allow-read --allow-write --allow-run src/tests/e2e/

# Run all tests with coverage
test-coverage: rescript-build
    deno test --allow-read --allow-write --coverage=coverage/ src/tests/
    deno coverage coverage/

# Run tests in watch mode
test-watch:
    deno test --allow-read --allow-write --watch src/tests/

# ============================================================================
# LANGUAGE SERVER & TOOLING
# ============================================================================

# Start the language server
lsp: rescript-build
    deno run --allow-read --allow-write src/noteg-lang/lsp/main.js

# Lint the codebase
lint:
    deno lint

# Format the codebase
fmt:
    deno fmt

# Check JavaScript (compiled from ReScript)
check:
    deno check src/**/*.js

# ============================================================================
# DEVELOPMENT
# ============================================================================

# Start development server
dev: rescript-build
    deno task dev

# Run the MCP server
mcp: rescript-build
    deno run --allow-read --allow-write --allow-run src/noteg-mcp/main.js

# ============================================================================
# POLICY ENFORCEMENT
# ============================================================================

# Check language policy compliance
policy-check:
    deno run --allow-read scripts/check-policy.js

# Validate Mustfile.ncl
mustfile-validate:
    @nickel export Mustfile.ncl > /dev/null && echo "Mustfile.ncl is valid"

# Run pre-commit checks
pre-commit: fmt lint check policy-check test-unit
    @echo "Pre-commit checks passed"

# ============================================================================
# CONTAINER & DEPLOYMENT
# ============================================================================

# Build container image
container-build:
    podman build -t odd-ssg:latest .

# Run in container
container-run:
    podman run -it --rm -v $(pwd):/app:Z odd-ssg:latest

# Push to registry
container-push registry="ghcr.io/hyperpolymath":
    podman push odd-ssg:latest {{registry}}/odd-ssg:latest

# ============================================================================
# MUSTFILE TRANSITIONS (Contract of Physical State)
# ============================================================================

# Transition: clean -> development
must-dev: rescript-build
    @echo "Transitioned to: development"

# Transition: development -> tested
must-test: test
    @echo "Transitioned to: tested"

# Transition: tested -> built
must-build: must-test build
    @echo "Transitioned to: built"

# Transition: built -> deployed
must-deploy registry="ghcr.io/hyperpolymath": must-build container-build
    podman push odd-ssg:latest {{registry}}/odd-ssg:latest
    @echo "Transitioned to: deployed"

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate documentation
docs: rescript-build
    deno run --allow-read --allow-write scripts/gen-docs.js

# Serve documentation locally
docs-serve: docs
    deno run --allow-read --allow-net scripts/serve-docs.js

# ============================================================================
# RELEASE & CI
# ============================================================================

# Prepare release
release version: must-build docs
    @echo "Preparing release {{version}}..."
    @echo "Release {{version}} ready"

# CI pipeline simulation
ci: policy-check lint must-test
    @echo "CI passed!"

# ============================================================================
# UTILITIES
# ============================================================================

# Show project info
info:
    @echo "odd-ssg - Probabilistic Static Site Generator"
    @echo "Version: 0.1.0"
    @echo "Language: ReScript → JavaScript (Deno runtime)"
    @echo "Deno: $(deno --version | head -1)"

# Update dependencies
update:
    deno cache --reload src/Mod.res.js

# Generate lockfile
lock:
    deno cache --lock=deno.lock --lock-write src/Mod.res.js

# Run arbitrary deno command
deno +args:
    deno {{args}}
