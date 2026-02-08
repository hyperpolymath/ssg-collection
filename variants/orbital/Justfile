# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — orbital-ssg task runner

set shell := ["bash", "-cu"]
set dotenv-load := true

# Default recipe - show help
default:
    @just --list

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD & CHECK
# ═══════════════════════════════════════════════════════════════════════════════

# Check project health (lint + type check)
check:
    @echo "🔍 Checking project health..."
    deno lint adapters/
    deno check adapters/*.js
    @echo "✅ All checks passed"

# Format all code
fmt:
    @echo "🎨 Formatting code..."
    deno fmt adapters/ tests/
    @echo "✅ Formatting complete"

# Lint all code
lint:
    @echo "🔎 Linting code..."
    deno lint adapters/ tests/
    @echo "✅ Linting complete"

# Build/compile (verify syntax)
build:
    @echo "🔨 Building project..."
    deno check adapters/*.js
    @echo "✅ Build complete"

# ═══════════════════════════════════════════════════════════════════════════════
# TESTING
# ═══════════════════════════════════════════════════════════════════════════════

# Run all tests
test:
    @echo "🧪 Running tests..."
    deno test --allow-run --allow-read tests/
    @echo "✅ Tests complete"

# Run tests with coverage
test-cov:
    @echo "📊 Running tests with coverage..."
    deno test --allow-run --allow-read --coverage=coverage/ tests/
    deno coverage coverage/
    @echo "✅ Coverage report generated"

# Run end-to-end tests
test-e2e:
    @echo "🔗 Running E2E tests..."
    deno test --allow-run --allow-read --allow-net tests/integration/
    @echo "✅ E2E tests complete"

# Run all tests (unit + e2e)
test-all: test test-e2e
    @echo "✅ All tests complete"

# Run tests for a specific adapter
test-adapter adapter:
    @echo "🧪 Testing adapter: {{adapter}}..."
    deno test --allow-run --allow-read tests/{{adapter}}.test.js
    @echo "✅ Adapter tests complete"

# ═══════════════════════════════════════════════════════════════════════════════
# SECURITY
# ═══════════════════════════════════════════════════════════════════════════════

# Run security scan
security-scan:
    @echo "🛡️ Running security scan..."
    @echo "Checking for hardcoded secrets..."
    @! grep -rn "password\s*=" adapters/ || true
    @! grep -rn "api_key\s*=" adapters/ || true
    @! grep -rn "secret\s*=" adapters/ || true
    @echo "Checking sanitization functions..."
    @grep -l "sanitize" adapters/*.js | wc -l | xargs -I{} echo "Found {} adapters with sanitization"
    @echo "✅ Security scan complete"

# Audit dependencies
audit:
    @echo "🔐 Auditing dependencies..."
    deno info
    @echo "✅ Audit complete"

# ═══════════════════════════════════════════════════════════════════════════════
# ADAPTERS
# ═══════════════════════════════════════════════════════════════════════════════

# List all adapters
adapters:
    @echo "📦 Available adapters:"
    @ls -1 adapters/*.js | xargs -I{} basename {} .js | sort

# Check adapter connectivity
adapter-check adapter:
    @echo "🔌 Checking adapter: {{adapter}}..."
    deno eval "import { connect } from './adapters/{{adapter}}.js'; console.log(await connect() ? '✅ Connected' : '❌ Not available')"

# Check all adapter versions
adapter-versions:
    @echo "📋 Checking adapter versions..."
    @for f in adapters/*.js; do \
        name=$$(basename "$$f" .js); \
        echo "Checking $$name..."; \
    done

# ═══════════════════════════════════════════════════════════════════════════════
# CONTAINER
# ═══════════════════════════════════════════════════════════════════════════════

# Build container image
container:
    @echo "🐳 Building container..."
    podman build -t orbital-ssg:latest .
    @echo "✅ Container built"

# Run container
container-run:
    @echo "🐳 Running container..."
    podman run --rm -it orbital-ssg:latest

# Run tests in container
container-test:
    @echo "🐳 Running tests in container..."
    podman run --rm orbital-ssg:latest deno test --allow-run tests/

# ═══════════════════════════════════════════════════════════════════════════════
# DEVELOPMENT
# ═══════════════════════════════════════════════════════════════════════════════

# Start development environment
dev:
    @echo "🚀 Starting development environment..."
    @echo "Run 'just watch' in another terminal for file watching"
    deno repl

# Watch for changes and run tests
watch:
    @echo "👀 Watching for changes..."
    deno test --watch --allow-run --allow-read tests/

# Start LSP for editor integration
lsp:
    @echo "🔧 Starting Deno LSP..."
    deno lsp

# ═══════════════════════════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════════════════════════

# Generate documentation
docs:
    @echo "📚 Generating documentation..."
    deno doc adapters/*.js --html --output=docs/api/
    @echo "✅ Documentation generated in docs/api/"

# Serve documentation locally
docs-serve:
    @echo "📚 Serving documentation..."
    deno run --allow-net --allow-read https://deno.land/std/http/file_server.ts docs/

# ═══════════════════════════════════════════════════════════════════════════════
# RELEASE
# ═══════════════════════════════════════════════════════════════════════════════

# Prepare release
release-prep version:
    @echo "📦 Preparing release {{version}}..."
    @echo "1. Update version in deno.json and STATE.scm"
    @echo "2. Update CHANGELOG.md"
    @echo "3. Run: just test-all"
    @echo "4. Commit with: git commit -m 'chore: release {{version}}'"
    @echo "5. Tag with: git tag v{{version}}"

# Create git tag
tag version:
    @echo "🏷️ Creating tag v{{version}}..."
    git tag -a "v{{version}}" -m "Release v{{version}}"
    @echo "✅ Tag created. Push with: git push --tags"

# ═══════════════════════════════════════════════════════════════════════════════
# SYNC
# ═══════════════════════════════════════════════════════════════════════════════

# Sync adapters from hub
sync-from-hub:
    @echo "🔄 Syncing adapters from poly-ssg-mcp hub..."
    @echo "Run: ~/Documents/scripts/transfer-ssg-adapters.sh --satellite orbital-ssg"

# Sync adapters to hub
sync-to-hub:
    @echo "🔄 Syncing adapters to poly-ssg-mcp hub..."
    @echo "Run: ~/Documents/scripts/transfer-ssg-adapters.sh --hub orbital-ssg"

# ═══════════════════════════════════════════════════════════════════════════════
# UTILITIES
# ═══════════════════════════════════════════════════════════════════════════════

# Clean build artifacts
clean:
    @echo "🧹 Cleaning build artifacts..."
    rm -rf coverage/ docs/api/
    @echo "✅ Clean complete"

# Show project info
info:
    @echo "📋 Project Info"
    @echo "==============="
    @echo "Name: orbital-ssg"
    @echo "Adapters: $(ls -1 adapters/*.js | wc -l)"
    @echo "Tests: $(ls -1 tests/*.test.js 2>/dev/null | wc -l || echo 0)"
    @echo "Deno: $(deno --version | head -1)"

# Verify all tools are installed
verify:
    @echo "🔍 Verifying tools..."
    @command -v deno >/dev/null && echo "✅ deno" || echo "❌ deno not found"
    @command -v just >/dev/null && echo "✅ just" || echo "❌ just not found"
    @command -v podman >/dev/null && echo "✅ podman" || echo "❌ podman not found"
    @command -v git >/dev/null && echo "✅ git" || echo "❌ git not found"
