# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — gungir-ssg
# Command runner for development, testing, and deployment

# ═══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

set shell := ["bash", "-euo", "pipefail", "-c"]
set dotenv-load := true

# Default recipe - show help
default:
    @just --list --unsorted

# ═══════════════════════════════════════════════════════════════════════════════
# DEVELOPMENT
# ═══════════════════════════════════════════════════════════════════════════════

# Format all JavaScript/TypeScript files
fmt *args='':
    deno fmt {{args}} adapters/*.js

# Lint all JavaScript/TypeScript files
lint *args='':
    deno lint {{args}} adapters/*.js

# Check formatting without modifying files
fmt-check:
    deno fmt --check adapters/*.js

# Run all checks (format + lint)
check: fmt-check lint
    @echo "✓ All checks passed"

# ═══════════════════════════════════════════════════════════════════════════════
# TESTING
# ═══════════════════════════════════════════════════════════════════════════════

# Run unit tests
test *args='':
    deno test --allow-run --allow-read {{args}} tests/

# Run tests with coverage
test-coverage:
    deno test --allow-run --allow-read --coverage=coverage/ tests/
    deno coverage coverage/

# Run end-to-end tests
test-e2e:
    deno test --allow-run --allow-read --allow-write tests/e2e/

# Run all tests (unit + e2e)
test-all: test test-e2e
    @echo "✓ All tests passed"

# Run tests for a specific adapter
test-adapter name:
    deno test --allow-run --allow-read --filter "{{name}}" tests/

# ═══════════════════════════════════════════════════════════════════════════════
# ADAPTER OPERATIONS
# ═══════════════════════════════════════════════════════════════════════════════

# List all available adapters
list-adapters:
    @echo "Available SSG Adapters (28):"
    @echo "─────────────────────────────"
    @ls -1 adapters/*.js | sed 's/adapters\//  /' | sed 's/\.js$//'

# Verify adapter structure and exports
verify-adapter name:
    @echo "Verifying adapter: {{name}}"
    deno eval "import * as a from './adapters/{{name}}.js'; \
        console.log('Name:', a.name); \
        console.log('Language:', a.language); \
        console.log('Tools:', a.tools.length)"

# Verify all adapters
verify-all-adapters:
    #!/usr/bin/env bash
    for adapter in adapters/*.js; do
        name=$(basename "$adapter" .js)
        echo "Checking $name..."
        just verify-adapter "$name" || exit 1
    done
    echo "✓ All adapters verified"

# Sync adapters from poly-ssg-mcp hub
sync:
    @echo "Syncing adapters from poly-ssg-mcp hub..."
    @if [ -f scripts/sync-adapters.sh ]; then \
        ./scripts/sync-adapters.sh; \
    else \
        echo "Note: sync script not found. Manual sync required."; \
    fi

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD & RELEASE
# ═══════════════════════════════════════════════════════════════════════════════

# Build/bundle (currently a no-op for pure JS)
build:
    @echo "✓ No build step required (pure JavaScript)"

# Clean build artifacts
clean:
    rm -rf coverage/
    rm -rf .deno/
    @echo "✓ Cleaned"

# Bump version (major, minor, or patch)
version type:
    #!/usr/bin/env bash
    current=$(grep -oP 'version\s*\.\s*"\K[^"]+' STATE.scm | head -1)
    IFS='.' read -ra parts <<< "$current"
    case "{{type}}" in
        major) new="$((parts[0]+1)).0.0" ;;
        minor) new="${parts[0]}.$((parts[1]+1)).0" ;;
        patch) new="${parts[0]}.${parts[1]}.$((parts[2]+1))" ;;
        *) echo "Usage: just version [major|minor|patch]"; exit 1 ;;
    esac
    echo "Bumping version: $current → $new"
    sed -i "s/version \. \"$current\"/version . \"$new\"/" STATE.scm
    sed -i "s/\"version\": \"$current\"/\"version\": \"$new\"/" deno.json
    echo "✓ Version bumped to $new"

# Pre-release checks
pre-release-check: check test-all verify-all-adapters
    @echo "✓ Pre-release checks passed"

# Create release (tag and push)
release tag:
    git tag -a "{{tag}}" -m "Release {{tag}}"
    git push origin "{{tag}}"
    @echo "✓ Released {{tag}}"

# ═══════════════════════════════════════════════════════════════════════════════
# CI/CD
# ═══════════════════════════════════════════════════════════════════════════════

# Run full CI pipeline locally
ci: check test-all
    @echo "✓ CI pipeline passed"

# Run must-pass checks (see Mustfile)
must:
    @just must-spdx
    @just must-lint
    @just must-test

# Check SPDX headers present
must-spdx:
    #!/usr/bin/env bash
    missing=0
    for f in adapters/*.js *.scm; do
        if ! grep -q "SPDX-License-Identifier" "$f"; then
            echo "Missing SPDX header: $f"
            missing=$((missing+1))
        fi
    done
    if [ $missing -gt 0 ]; then
        echo "✗ $missing files missing SPDX headers"
        exit 1
    fi
    echo "✓ All files have SPDX headers"

# Must-pass lint check
must-lint: lint
    @echo "✓ Lint passed"

# Must-pass test check
must-test: test
    @echo "✓ Tests passed"

# ═══════════════════════════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════════════════════════

# Generate documentation
docs:
    @echo "Documentation files:"
    @ls -1 *.adoc *.md 2>/dev/null || true
    @ls -1 *.scm

# Serve documentation locally (if using a docs SSG)
docs-serve:
    @echo "To serve docs, use one of the adapters!"
    @echo "Example: deno run --allow-run adapters/mdbook.js serve docs/"

# ═══════════════════════════════════════════════════════════════════════════════
# UTILITIES
# ═══════════════════════════════════════════════════════════════════════════════

# Show project status from STATE.scm
status:
    @echo "Project Status (from STATE.scm):"
    @echo "─────────────────────────────────"
    @grep -E "(version|phase|completion|updated)" STATE.scm | head -10

# Health check - verify development environment
health-check:
    #!/usr/bin/env bash
    echo "Environment Health Check"
    echo "────────────────────────"
    echo -n "Deno: "
    deno --version | head -1 || echo "NOT INSTALLED"
    echo -n "just: "
    just --version || echo "NOT INSTALLED"
    echo -n "git: "
    git --version || echo "NOT INSTALLED"
    echo "────────────────────────"
    echo "✓ Health check complete"

# Install development dependencies
setup:
    @echo "Setting up development environment..."
    @echo "Required tools: deno, just, git"
    @echo "Optional: asdf (for version management)"
    @just health-check

# Show adapter language statistics
stats:
    @echo "Adapter Language Statistics:"
    @echo "────────────────────────────"
    @grep -h "export const language" adapters/*.js | \
        sed 's/.*= "\([^"]*\)".*/\1/' | sort | uniq -c | sort -rn

# ═══════════════════════════════════════════════════════════════════════════════
# HOOKS
# ═══════════════════════════════════════════════════════════════════════════════

# Install git hooks
hooks-install:
    #!/usr/bin/env bash
    mkdir -p .git/hooks
    cat > .git/hooks/pre-commit << 'HOOK'
    #!/bin/bash
    just fmt-check && just lint
    HOOK
    chmod +x .git/hooks/pre-commit
    cat > .git/hooks/pre-push << 'HOOK'
    #!/bin/bash
    just test
    HOOK
    chmod +x .git/hooks/pre-push
    echo "✓ Git hooks installed"

# Uninstall git hooks
hooks-uninstall:
    rm -f .git/hooks/pre-commit .git/hooks/pre-push
    @echo "✓ Git hooks uninstalled"
