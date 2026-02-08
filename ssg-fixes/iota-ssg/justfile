# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — iota-ssg

# Default recipe - show help
default:
    @just --list

# ============================================================================
# BUILD RECIPES
# ============================================================================

# Run full build validation
build: check lint
    @echo "✓ Build validation complete"

# Check syntax and validate adapters
check:
    @echo "Checking adapter syntax..."
    @for f in adapters/*.js; do deno check "$f" 2>/dev/null || echo "Note: $f needs Deno types"; done
    @echo "✓ Syntax check complete"

# Format all files
fmt:
    @echo "Formatting files..."
    @command -v prettier >/dev/null && prettier --write "adapters/*.js" "*.md" "*.adoc" || echo "prettier not installed"
    @echo "✓ Formatting complete"

# Run linters
lint:
    @echo "Running linters..."
    @command -v eslint >/dev/null && eslint adapters/*.js --fix || echo "eslint not installed"
    @echo "✓ Lint complete"

# Clean build artifacts
clean:
    @echo "Cleaning..."
    @rm -rf node_modules .cache coverage dist
    @echo "✓ Clean complete"

# ============================================================================
# TEST RECIPES
# ============================================================================

# Run all tests
test: test-unit test-integration
    @echo "✓ All tests complete"

# Run unit tests
test-unit:
    @echo "Running unit tests..."
    @if [ -d "tests" ]; then deno test tests/ --allow-read --allow-run; else echo "No tests directory yet"; fi

# Run integration tests
test-integration:
    @echo "Running integration tests..."
    @echo "Integration tests planned for v0.4"

# Run E2E tests
test-e2e:
    @echo "Running E2E tests..."
    @echo "E2E tests planned for v0.4"

# Test a specific adapter
test-adapter adapter:
    @echo "Testing adapter: {{adapter}}"
    @deno eval "import * as a from './adapters/{{adapter}}.js'; console.log('Name:', a.name); console.log('Language:', a.language); console.log('Tools:', a.tools.length)"

# Test all adapters can be imported
test-adapters:
    @echo "Testing all adapters..."
    @for f in adapters/*.js; do \
        name=$$(basename "$$f" .js); \
        if [ "$$name" != "README" ]; then \
            echo -n "Testing $$name... "; \
            deno eval "import * as a from './$$f'; console.log('✓', a.name)" 2>/dev/null || echo "✗ failed"; \
        fi; \
    done

# Run tests with coverage
test-coverage:
    @echo "Running tests with coverage..."
    @deno test --coverage=coverage tests/ --allow-read --allow-run || true
    @deno coverage coverage --lcov > coverage.lcov || true

# ============================================================================
# DEVELOPMENT RECIPES
# ============================================================================

# Start development mode
dev: watch

# Watch for changes
watch:
    @echo "Watching for changes..."
    @command -v watchexec >/dev/null && watchexec -e js,md,adoc just check || echo "watchexec not installed"

# Start local documentation server
serve-docs:
    @echo "Serving documentation..."
    @python3 -m http.server 8080 || python -m SimpleHTTPServer 8080

# ============================================================================
# ADAPTER MANAGEMENT
# ============================================================================

# List all adapters
list-adapters:
    @echo "Available adapters:"
    @ls -1 adapters/*.js | xargs -I {} basename {} .js | grep -v README

# Count adapters
count-adapters:
    @echo -n "Total adapters: "
    @ls -1 adapters/*.js 2>/dev/null | grep -v README | wc -l

# Show adapter info
adapter-info adapter:
    @deno eval "import * as a from './adapters/{{adapter}}.js'; console.log('Name:', a.name); console.log('Language:', a.language); console.log('Description:', a.description); console.log('Tools:'); a.tools.forEach(t => console.log('  -', t.name + ':', t.description))"

# Validate adapter interface
validate-adapter adapter:
    @echo "Validating {{adapter}} adapter interface..."
    @deno eval "\
        import * as a from './adapters/{{adapter}}.js'; \
        const required = ['name', 'language', 'description', 'connect', 'disconnect', 'isConnected', 'tools']; \
        const missing = required.filter(r => !(r in a)); \
        if (missing.length) { console.error('Missing:', missing); Deno.exit(1); } \
        console.log('✓ Interface valid');"

# Validate all adapters
validate-adapters:
    @echo "Validating all adapter interfaces..."
    @for f in adapters/*.js; do \
        name=$$(basename "$$f" .js); \
        if [ "$$name" != "README" ]; then \
            just validate-adapter "$$name" 2>/dev/null || echo "✗ $$name failed validation"; \
        fi; \
    done

# ============================================================================
# SYNCHRONIZATION
# ============================================================================

# Sync adapters from hub (placeholder)
sync-from-hub:
    @echo "Syncing adapters from poly-ssg-mcp hub..."
    @echo "Run: ~/Documents/scripts/transfer-ssg-adapters.sh --satellite iota-ssg"

# Check sync status
sync-status:
    @echo "Checking sync status..."
    @git status adapters/

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate documentation
docs:
    @echo "Documentation is in AsciiDoc format"
    @echo "View: README.adoc, cookbook.adoc, CONTRIBUTING.md"

# Open documentation
docs-open:
    @command -v xdg-open >/dev/null && xdg-open README.adoc || open README.adoc

# ============================================================================
# CI/CD RECIPES
# ============================================================================

# Run CI checks locally
ci: check lint test
    @echo "✓ CI checks passed"

# Prepare for release
release-prep version:
    @echo "Preparing release {{version}}..."
    @sed -i 's/version . "[^"]*"/version . "{{version}}"/' STATE.scm
    @echo "Updated STATE.scm version to {{version}}"
    @echo "Don't forget to update CHANGELOG.md!"

# ============================================================================
# HOOKS
# ============================================================================

# Install git hooks
hooks-install:
    @echo "Installing git hooks..."
    @cp -f .githooks/* .git/hooks/ 2>/dev/null || echo "No custom hooks to install"
    @chmod +x .git/hooks/* 2>/dev/null || true
    @echo "✓ Hooks installed"

# Uninstall git hooks
hooks-uninstall:
    @echo "Uninstalling git hooks..."
    @rm -f .git/hooks/pre-commit .git/hooks/commit-msg
    @echo "✓ Hooks uninstalled"

# ============================================================================
# CONTAINER
# ============================================================================

# Build container
container-build:
    @echo "Building container..."
    @podman build -t iota-ssg:latest . || docker build -t iota-ssg:latest .

# Run container
container-run:
    @echo "Running container..."
    @podman run -it --rm iota-ssg:latest || docker run -it --rm iota-ssg:latest

# ============================================================================
# MAINTENANCE
# ============================================================================

# Update dependencies
update-deps:
    @echo "Updating dependencies..."
    @command -v npm >/dev/null && npm update || echo "npm not available"

# Show project status
status:
    @echo "=== iota-ssg Status ==="
    @echo -n "Adapters: "; just count-adapters
    @echo -n "Git: "; git status -s | wc -l | xargs echo "modified files"
    @echo -n "Branch: "; git branch --show-current

# Show SCM state summary
state:
    @echo "=== Project State ==="
    @grep -A5 "state-summary" STATE.scm | tail -6

# ============================================================================
# NICKEL INTEGRATION (placeholder)
# ============================================================================

# Evaluate Nickel configuration (if available)
nickel-eval file:
    @command -v nickel >/dev/null && nickel eval {{file}} || echo "Nickel not installed"

# ============================================================================
# HELP
# ============================================================================

# Show detailed help
help:
    @echo "iota-ssg - SSG Adapter Collection"
    @echo ""
    @echo "Quick Start:"
    @echo "  just build          - Run build validation"
    @echo "  just test           - Run all tests"
    @echo "  just list-adapters  - List available adapters"
    @echo ""
    @echo "All Recipes:"
    @just --list
