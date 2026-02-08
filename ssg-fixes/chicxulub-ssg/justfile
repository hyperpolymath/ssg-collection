# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — chicxulub--ssg
# Comprehensive build recipes for SSG satellite

# Default recipe - show help
default:
    @just --list

# ============================================================================
# SETUP & INSTALLATION
# ============================================================================

# Initial project setup
setup:
    @echo "Setting up chicxulub--ssg development environment..."
    deno cache --reload adapters/*.js
    @echo "Setup complete!"

# Install development dependencies
install-deps:
    @echo "Installing development tools..."
    @command -v deno >/dev/null || (echo "Please install Deno: https://deno.land" && exit 1)
    @echo "Dependencies ready!"

# Verify all tools are available
doctor:
    @echo "Checking development environment..."
    @echo -n "Deno: " && deno --version | head -1
    @echo -n "Git: " && git --version
    @echo -n "Just: " && just --version
    @echo "Environment OK!"

# ============================================================================
# BUILD & COMPILE
# ============================================================================

# Build/check all adapters
build:
    @echo "Checking all adapters..."
    deno check adapters/*.js
    @echo "Build complete!"

# Type-check all TypeScript/JavaScript
check:
    deno check adapters/*.js
    deno lint adapters/*.js
    deno fmt --check adapters/*.js

# Compile a specific adapter (verify syntax)
compile adapter:
    deno check adapters/{{adapter}}.js

# ============================================================================
# TESTING
# ============================================================================

# Run all tests
test:
    deno test --allow-all tests/

# Run unit tests only
test-unit:
    deno test --allow-all tests/unit/

# Run end-to-end tests
test-e2e:
    deno test --allow-all --timeout=600000 tests/e2e/

# Run tests for a specific adapter
test-adapter adapter:
    deno test --allow-all tests/adapters/{{adapter}}_test.js

# Run tests with coverage
test-coverage:
    deno test --allow-all --coverage=coverage/ tests/
    deno coverage coverage/

# Run tests and generate HTML coverage report
test-coverage-html:
    deno test --allow-all --coverage=coverage/ tests/
    deno coverage --html coverage/

# ============================================================================
# LINTING & FORMATTING
# ============================================================================

# Lint all code
lint:
    deno lint adapters/*.js
    @echo "Lint complete!"

# Format all code
fmt:
    deno fmt adapters/*.js
    @echo "Format complete!"

# Check formatting without changes
fmt-check:
    deno fmt --check adapters/*.js

# Run all quality checks
qa: lint fmt-check check
    @echo "All quality checks passed!"

# ============================================================================
# ADAPTER MANAGEMENT
# ============================================================================

# List all available adapters
list-adapters:
    @echo "Available SSG Adapters:"
    @ls -1 adapters/*.js | sed 's|adapters/||' | sed 's|\.js||' | sort

# Count adapters
count-adapters:
    @echo -n "Total adapters: "
    @ls -1 adapters/*.js | wc -l

# Sync adapters from poly-ssg-mcp hub
sync-adapters:
    @echo "Syncing adapters from poly-ssg-mcp hub..."
    @if [ -f ~/Documents/scripts/transfer-ssg-adapters.sh ]; then \
        ~/Documents/scripts/transfer-ssg-adapters.sh --satellite chicxulub--ssg; \
    else \
        echo "Sync script not found. Manual sync required."; \
    fi

# Verify an adapter connects successfully
verify-adapter adapter:
    @echo "Verifying {{adapter}} adapter..."
    deno eval "import * as a from './adapters/{{adapter}}.js'; console.log('Name:', a.name); console.log('Language:', a.language); console.log('Tools:', a.tools.length);"

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate documentation
docs:
    @echo "Generating documentation..."
    @if command -v asciidoctor >/dev/null; then \
        asciidoctor README.adoc -o docs/index.html; \
        asciidoctor cookbook.adoc -o docs/cookbook.html; \
    else \
        echo "asciidoctor not found, skipping HTML generation"; \
    fi
    @echo "Documentation generated!"

# Serve documentation locally
docs-serve: docs
    @echo "Serving docs at http://localhost:8000"
    deno run --allow-net --allow-read https://deno.land/std/http/file_server.ts docs/

# ============================================================================
# LANGUAGE SERVER
# ============================================================================

# Start LSP server (placeholder for future)
lsp:
    @echo "LSP server not yet implemented"
    @echo "See ROADMAP.scm Phase 5 for plans"

# ============================================================================
# DEVELOPMENT UTILITIES
# ============================================================================

# Watch for changes and run tests
watch:
    deno test --allow-all --watch tests/

# Clean build artifacts
clean:
    rm -rf coverage/
    rm -rf docs/*.html
    @echo "Cleaned!"

# Deep clean including caches
clean-all: clean
    deno cache --reload adapters/*.js
    @echo "Deep clean complete!"

# ============================================================================
# GIT HELPERS
# ============================================================================

# Show git status
status:
    git status -sb

# Create a feature branch
feature name:
    git checkout -b feat/{{name}}

# Create a fix branch
fix name:
    git checkout -b fix/{{name}}

# Commit with conventional message
commit type scope message:
    git commit -m "{{type}}({{scope}}): {{message}}"

# ============================================================================
# CI/CD HELPERS
# ============================================================================

# Run full CI pipeline locally
ci: qa test
    @echo "CI pipeline passed!"

# Pre-commit checks
pre-commit: fmt lint check
    @echo "Pre-commit checks passed!"

# Prepare for release
release-prep version:
    @echo "Preparing release {{version}}..."
    @echo "1. Update STATE.scm version"
    @echo "2. Update CHANGELOG.md"
    @echo "3. Run: just ci"
    @echo "4. Tag: git tag -a v{{version}} -m 'Release {{version}}'"

# ============================================================================
# SECURITY
# ============================================================================

# Run security audit
audit:
    @echo "Running security checks..."
    deno lint --rules-exclude=no-explicit-any adapters/*.js
    @echo "Security audit complete!"

# Check for secrets in code
secrets-scan:
    @echo "Scanning for secrets..."
    @if command -v gitleaks >/dev/null; then \
        gitleaks detect --source . --verbose; \
    else \
        echo "gitleaks not found, using basic grep scan"; \
        grep -rn "password\|secret\|api.key\|token" --include="*.js" adapters/ || echo "No obvious secrets found"; \
    fi

# ============================================================================
# CONTAINER SUPPORT
# ============================================================================

# Build container image
container-build:
    podman build -t chicxulub-ssg:latest .

# Run in container
container-run:
    podman run --rm -it chicxulub-ssg:latest

# ============================================================================
# BENCHMARKING
# ============================================================================

# Benchmark adapter loading
bench-load:
    @echo "Benchmarking adapter load times..."
    deno bench --allow-all benchmarks/load_test.js

# ============================================================================
# ALL-IN-ONE COMMANDS
# ============================================================================

# Run everything for new contributors
bootstrap: install-deps setup doctor
    @echo "Bootstrap complete! Run 'just test' to verify."

# Full test suite with coverage
test-all: test test-coverage
    @echo "All tests complete!"

# Pre-push verification
pre-push: qa test
    @echo "Ready to push!"
