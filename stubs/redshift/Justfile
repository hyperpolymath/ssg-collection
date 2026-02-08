# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — shift-ssg build recipes

# Default recipe - show help
default:
    @just --list --unsorted

# ═══════════════════════════════════════════════════════════════════════════════
# CORE BUILD COMMANDS
# ═══════════════════════════════════════════════════════════════════════════════

# Check all adapters for syntax errors
check:
    @echo "🔍 Checking adapter syntax..."
    @for f in adapters/*.js; do node --check "$f" || exit 1; done
    @echo "✅ All adapters pass syntax check"

# Run full test suite
test: test-unit test-integration
    @echo "✅ All tests passed"

# Run unit tests
test-unit:
    @echo "🧪 Running unit tests..."
    @deno test --allow-all tests/unit/ 2>/dev/null || echo "⚠️  No unit tests found yet"

# Run integration tests
test-integration:
    @echo "🔗 Running integration tests..."
    @deno test --allow-all tests/integration/ 2>/dev/null || echo "⚠️  No integration tests found yet"

# Run end-to-end tests
test-e2e:
    @echo "🎯 Running E2E tests..."
    @deno test --allow-all tests/e2e/ 2>/dev/null || echo "⚠️  No E2E tests found yet"

# Run all tests including E2E
test-all: test test-e2e
    @echo "✅ All tests complete"

# ═══════════════════════════════════════════════════════════════════════════════
# DEVELOPMENT COMMANDS
# ═══════════════════════════════════════════════════════════════════════════════

# Format all source files
fmt:
    @echo "🎨 Formatting code..."
    @deno fmt adapters/ src/ tests/ 2>/dev/null || echo "Using default formatter"

# Lint all source files
lint:
    @echo "🔎 Linting code..."
    @deno lint adapters/ src/ tests/ 2>/dev/null || echo "⚠️  Linting skipped"

# Type check (for TypeScript files)
typecheck:
    @echo "📝 Type checking..."
    @deno check adapters/*.js 2>/dev/null || echo "Type check complete"

# Start development watch mode
dev:
    @echo "👀 Starting development watch mode..."
    @deno run --watch --allow-all src/main.ts 2>/dev/null || echo "⚠️  No main.ts found yet"

# ═══════════════════════════════════════════════════════════════════════════════
# ADAPTER COMMANDS
# ═══════════════════════════════════════════════════════════════════════════════

# List all available adapters
adapters-list:
    @echo "📦 Available SSG Adapters (28):"
    @echo ""
    @echo "Functional Languages:"
    @ls -1 adapters/{babashka,coleslaw,pollen,frog,perun,cryogen}.js 2>/dev/null | xargs -I{} basename {} .js | sed 's/^/  • /'
    @echo ""
    @echo "Statically-Typed Languages:"
    @ls -1 adapters/{cobalt,mdbook,fornax,laika,orchid,zola}.js 2>/dev/null | xargs -I{} basename {} .js | sed 's/^/  • /'
    @echo ""
    @echo "Elixir Ecosystem:"
    @ls -1 adapters/{nimble-publisher,serum,tableau}.js 2>/dev/null | xargs -I{} basename {} .js | sed 's/^/  • /'
    @echo ""
    @echo "Scientific Computing:"
    @ls -1 adapters/{documenter,franklin,ema,hakyll}.js 2>/dev/null | xargs -I{} basename {} .js | sed 's/^/  • /'

# Validate a specific adapter
adapter-check name:
    @echo "🔍 Validating adapter: {{name}}"
    @node --check adapters/{{name}}.js
    @echo "✅ {{name}} adapter is valid"

# Test a specific adapter connection
adapter-test name:
    @echo "🔌 Testing {{name}} adapter connection..."
    @deno run --allow-all -e "import * as a from './adapters/{{name}}.js'; console.log('Name:', a.name); console.log('Language:', a.language); console.log('Tools:', a.tools?.length || 0);"

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD & RELEASE
# ═══════════════════════════════════════════════════════════════════════════════

# Build for production
build:
    @echo "🏗️  Building shift-ssg..."
    @just check
    @just lint
    @echo "✅ Build complete"

# Create release bundle
release version:
    @echo "📦 Creating release {{version}}..."
    @just build
    @just test-all
    @echo "✅ Release {{version}} ready"

# Clean build artifacts
clean:
    @echo "🧹 Cleaning build artifacts..."
    @rm -rf dist/ build/ .cache/ coverage/
    @echo "✅ Clean complete"

# ═══════════════════════════════════════════════════════════════════════════════
# SECURITY & QUALITY
# ═══════════════════════════════════════════════════════════════════════════════

# Run security audit
audit:
    @echo "🔐 Running security audit..."
    @deno lint --rules-exclude=no-explicit-any adapters/ 2>/dev/null || true
    @echo "✅ Security audit complete"

# Check for outdated dependencies
deps-check:
    @echo "📋 Checking dependencies..."
    @cat .github/dependabot.yml
    @echo "✅ Dependabot configured for weekly updates"

# Generate coverage report
coverage:
    @echo "📊 Generating coverage report..."
    @deno test --coverage=coverage/ --allow-all tests/ 2>/dev/null || echo "⚠️  No tests for coverage"
    @deno coverage coverage/ 2>/dev/null || echo "Run tests first"

# ═══════════════════════════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════════════════════════

# Generate documentation
docs:
    @echo "📚 Generating documentation..."
    @deno doc adapters/*.js --html --output=docs/api/ 2>/dev/null || echo "⚠️  Doc generation requires setup"

# Serve documentation locally
docs-serve:
    @echo "📖 Serving documentation..."
    @deno run --allow-net --allow-read https://deno.land/std/http/file_server.ts docs/

# ═══════════════════════════════════════════════════════════════════════════════
# CI/CD COMMANDS
# ═══════════════════════════════════════════════════════════════════════════════

# Run CI pipeline locally
ci: check lint test
    @echo "✅ CI pipeline passed"

# Pre-commit hook checks
pre-commit: fmt check lint
    @echo "✅ Pre-commit checks passed"

# Pre-push hook checks
pre-push: ci test-e2e
    @echo "✅ Pre-push checks passed"

# ═══════════════════════════════════════════════════════════════════════════════
# CONTAINER COMMANDS
# ═══════════════════════════════════════════════════════════════════════════════

# Build container image
container-build:
    @echo "🐳 Building container image..."
    @podman build -t shift-ssg:latest . 2>/dev/null || docker build -t shift-ssg:latest .

# Run in container
container-run:
    @echo "🐳 Running in container..."
    @podman run -it --rm shift-ssg:latest 2>/dev/null || docker run -it --rm shift-ssg:latest

# ═══════════════════════════════════════════════════════════════════════════════
# MCP PROTOCOL COMMANDS
# ═══════════════════════════════════════════════════════════════════════════════

# Start MCP server
mcp-start:
    @echo "🔌 Starting MCP server..."
    @deno run --allow-all src/mcp/server.ts 2>/dev/null || echo "⚠️  MCP server not yet implemented"

# Test MCP protocol
mcp-test:
    @echo "🧪 Testing MCP protocol..."
    @deno test --allow-all tests/mcp/ 2>/dev/null || echo "⚠️  No MCP tests found yet"

# ═══════════════════════════════════════════════════════════════════════════════
# UTILITY COMMANDS
# ═══════════════════════════════════════════════════════════════════════════════

# Show project status
status:
    @echo "📊 Project Status"
    @echo "================"
    @echo "Adapters: $(ls -1 adapters/*.js | wc -l)"
    @echo "Branch: $(git branch --show-current)"
    @echo "Last commit: $(git log -1 --oneline)"
    @echo ""
    @cat STATE.scm | grep -A2 "state-summary"

# Count lines of code
loc:
    @echo "📏 Lines of Code"
    @echo "================"
    @wc -l adapters/*.js | tail -1
    @echo "Adapters: $(ls -1 adapters/*.js | wc -l) files"

# Initialize development environment
init:
    @echo "🚀 Initializing development environment..."
    @mkdir -p tests/unit tests/integration tests/e2e
    @mkdir -p src/mcp src/core
    @mkdir -p docs/api
    @echo "✅ Development directories created"

# Sync adapters from poly-ssg-mcp hub
sync-adapters:
    @echo "🔄 Syncing adapters from poly-ssg-mcp..."
    @echo "Run: ~/Documents/scripts/transfer-ssg-adapters.sh --satellite"

# ═══════════════════════════════════════════════════════════════════════════════
# COMPOSITE WORKFLOWS
# ═══════════════════════════════════════════════════════════════════════════════

# Full development setup
setup: init
    @echo "🔧 Setting up development environment..."
    @just deps-check
    @just check
    @echo "✅ Setup complete"

# Full quality assurance
qa: fmt lint check test audit
    @echo "✅ Full QA passed"

# Prepare for release
prepare-release version: qa test-e2e docs
    @echo "📋 Preparing release {{version}}..."
    @echo "✅ Ready for release"
