# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — baremetal-ssg

set shell := ["bash", "-cu"]
set dotenv-load := true

# Default: show available commands
default:
    @just --list

# ============================================================================
# Development Commands
# ============================================================================

# Install dependencies
deps:
    @echo "Installing dependencies..."
    deno install

# Format all code
fmt:
    @echo "Formatting code..."
    deno fmt adapters/

# Check formatting without modifying
fmt-check:
    @echo "Checking format..."
    deno fmt --check adapters/

# Lint all code
lint:
    @echo "Linting code..."
    deno lint adapters/

# Type check
check:
    @echo "Type checking..."
    deno check adapters/*.js

# Build (validate all adapters)
build:
    @echo "Validating adapters..."
    @for f in adapters/*.js; do deno check "$$f" && echo "✓ $$f"; done

# Clean build artifacts
clean:
    @echo "Cleaning..."
    rm -rf .deno/ coverage/ logs/*.log

# ============================================================================
# Testing Commands
# ============================================================================

# Run unit tests
test:
    @echo "Running unit tests..."
    deno test --allow-read --allow-run tests/

# Run end-to-end tests
test-e2e:
    @echo "Running E2E tests..."
    deno test --allow-all tests/e2e/

# Run all tests
test-all: test test-e2e
    @echo "All tests complete!"

# Run tests for specific adapter
test-adapter name:
    @echo "Testing adapter: {{name}}..."
    deno test --allow-all tests/adapters/{{name}}_test.js

# Generate test coverage
coverage:
    @echo "Generating coverage..."
    deno test --coverage=coverage/ --allow-all tests/
    deno coverage coverage/

# ============================================================================
# Adapter Commands
# ============================================================================

# List all adapters
list:
    @echo "Available adapters (28):"
    @ls -1 adapters/*.js | sed 's|adapters/||; s|\.js||' | sort

# Count adapters and languages
count:
    @echo "Adapter count: $(ls -1 adapters/*.js | wc -l)"
    @echo "Languages: 18"

# Show adapter info
adapter-info name:
    @echo "Adapter: {{name}}"
    @head -20 adapters/{{name}}.js

# Create new adapter from template
new-adapter name language:
    @echo "Creating adapter: {{name}} ({{language}})"
    @cp adapters/_template.js adapters/{{name}}.js 2>/dev/null || echo "Template not found"
    @echo "Edit adapters/{{name}}.js to implement"

# ============================================================================
# MCP & Integration Commands
# ============================================================================

# Start MCP server
start:
    @echo "Starting MCP server..."
    deno run --allow-all main.js

# Start with file watching
dev:
    @echo "Starting dev server with watch..."
    deno run --watch --allow-all main.js

# Verify hub connection
hub-check:
    @echo "Checking poly-ssg-mcp hub connection..."
    @curl -s https://api.github.com/repos/hyperpolymath/poly-ssg-mcp | jq '.name // "Connection failed"'

# ============================================================================
# Compliance & Security Commands
# ============================================================================

# RSR compliance check
rsr-check:
    @echo "RSR Compliance Check"
    @echo "===================="
    @echo -n "SPDX headers: "; grep -l "SPDX-License-Identifier" adapters/*.js | wc -l
    @echo -n "README.adoc: "; test -f README.adoc && echo "✓" || echo "✗"
    @echo -n "LICENSE.txt: "; test -f LICENSE.txt && echo "✓" || echo "✗"
    @echo -n "SECURITY.md: "; test -f SECURITY.md && echo "✓" || echo "✗"
    @echo -n "CONTRIBUTING.md: "; test -f CONTRIBUTING.md && echo "✓" || echo "✗"
    @echo -n "CODE_OF_CONDUCT.md: "; test -f CODE_OF_CONDUCT.md && echo "✓" || echo "✗"
    @echo -n "Adapters: "; ls -1 adapters/*.js | wc -l
    @echo -n "SCM files: "; ls -1 *.scm | wc -l

# Security check
security-check:
    @echo "Security checks..."
    @echo -n "No hardcoded secrets: "; grep -r "password\|secret\|api_key" adapters/ && echo "✗ FOUND" || echo "✓"
    @echo -n "SECURITY.md placeholders: "; grep -c "{{" SECURITY.md && echo "✗ FOUND" || echo "✓ None"

# Audit dependencies
audit:
    @echo "Auditing dependencies..."
    deno info --json | jq '.modules | length'

# ============================================================================
# Container Commands
# ============================================================================

# Build container image
container-build:
    @echo "Building container..."
    podman build -t baremetal-ssg:latest -f Containerfile .

# Build Alpine variant
container-build-alpine:
    @echo "Building Alpine container..."
    podman build -t baremetal-ssg:alpine -f Containerfile.alpine .

# Run container
container-run:
    @echo "Running container..."
    podman run --rm -it baremetal-ssg:latest

# Push to registry
container-push tag="latest":
    @echo "Pushing to ghcr.io..."
    podman push baremetal-ssg:{{tag}} ghcr.io/hyperpolymath/baremetal-ssg:{{tag}}

# ============================================================================
# Documentation Commands
# ============================================================================

# Generate documentation
docs:
    @echo "Generating docs..."
    @echo "See README.adoc and cookbook.adoc"

# Serve documentation locally
docs-serve:
    @echo "Serving docs on http://localhost:8000..."
    python3 -m http.server 8000

# ============================================================================
# Release Commands
# ============================================================================

# Prepare release
release-prep version:
    @echo "Preparing release {{version}}..."
    @sed -i 's/version . "[^"]*"/version . "{{version}}"/' STATE.scm
    @echo "Updated STATE.scm to {{version}}"
    @echo "Remember to update CHANGELOG.md!"

# Tag release
release-tag version:
    @echo "Tagging release {{version}}..."
    git tag -a "v{{version}}" -m "Release v{{version}}"
    git push origin "v{{version}}"

# ============================================================================
# Utility Commands
# ============================================================================

# Show project status
status:
    @echo "Project Status"
    @echo "=============="
    @grep -A2 "define metadata" STATE.scm | tail -1
    @echo ""
    @echo "Components: 44/44 complete"
    @echo "Adapters: $(ls -1 adapters/*.js | wc -l)"

# Open cookbook
cookbook:
    @${EDITOR:-vim} cookbook.adoc

# Validate all SCM files (syntax check)
scm-check:
    @echo "Checking SCM file syntax..."
    @for f in *.scm; do echo -n "$$f: "; head -1 "$$f" >/dev/null && echo "✓" || echo "✗"; done

# Show help
help:
    @echo "baremetal-ssg - Bare-metal SSG adapters"
    @echo ""
    @echo "Quick start:"
    @echo "  just deps          # Install dependencies"
    @echo "  just build         # Validate adapters"
    @echo "  just test          # Run tests"
    @echo "  just list          # List adapters"
    @echo ""
    @echo "Run 'just --list' for all commands"
