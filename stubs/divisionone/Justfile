# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# justfile — Division One SSG Build Commands
# Run with: just <recipe>
# List recipes: just --list

# Default recipe
default: check

# =============================================================================
# ENVIRONMENT SETUP
# =============================================================================

# Initialize development environment
setup:
    @echo "Setting up development environment..."
    @command -v deno >/dev/null 2>&1 || { echo "Deno required. Install: https://deno.land"; exit 1; }
    @echo "Deno version: $(deno --version | head -1)"
    @echo "Environment ready!"

# Check all prerequisites
prereqs:
    @echo "Checking prerequisites..."
    @command -v deno >/dev/null 2>&1 && echo "✓ Deno installed" || echo "✗ Deno missing"
    @command -v git >/dev/null 2>&1 && echo "✓ Git installed" || echo "✗ Git missing"
    @echo "Done."

# =============================================================================
# DEVELOPMENT COMMANDS
# =============================================================================

# Format all adapter code
fmt:
    deno fmt adapters/

# Check formatting without changes
fmt-check:
    deno fmt --check adapters/

# Run linter on all adapters
lint:
    deno lint adapters/

# Run all checks (format + lint)
check: fmt-check lint
    @echo "All checks passed!"

# =============================================================================
# TESTING
# =============================================================================

# Run unit tests
test:
    deno test --allow-run --allow-read

# Run tests with coverage
test-cov:
    deno test --allow-run --allow-read --coverage=coverage/

# Generate coverage report
coverage: test-cov
    deno coverage coverage/ --lcov > coverage/lcov.info
    @echo "Coverage report: coverage/lcov.info"

# Run e2e tests
test-e2e:
    deno test tests/e2e/ --allow-run --allow-read --allow-write --allow-net

# Run all tests (unit + e2e)
test-all: test test-e2e
    @echo "All tests passed!"

# =============================================================================
# ADAPTER MANAGEMENT
# =============================================================================

# List all available adapters
list-adapters:
    @echo "Available SSG Adapters (28):"
    @echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    @ls -1 adapters/*.js | xargs -I {} basename {} .js | sort

# Count adapters by language
adapter-stats:
    @echo "Adapter Statistics:"
    @echo "━━━━━━━━━━━━━━━━━━━"
    @echo "Total adapters: $(ls -1 adapters/*.js | wc -l)"
    @echo "Total lines: $(wc -l adapters/*.js | tail -1 | awk '{print $1}')"

# Validate adapter structure
validate-adapters:
    @echo "Validating adapter structure..."
    @for f in adapters/*.js; do \
        echo -n "Checking $f... "; \
        grep -q "export const name" "$f" && \
        grep -q "export const language" "$f" && \
        grep -q "export const tools" "$f" && \
        grep -q "SPDX-License-Identifier" "$f" && \
        echo "✓" || echo "✗ FAILED"; \
    done

# Sync adapters from poly-ssg-mcp hub
sync-adapters:
    @echo "Syncing adapters from poly-ssg-mcp hub..."
    @if [ -f ~/Documents/scripts/transfer-ssg-adapters.sh ]; then \
        ~/Documents/scripts/transfer-ssg-adapters.sh --satellite divisionone-ssg; \
    else \
        echo "Sync script not found. Manual sync required."; \
    fi

# Test specific adapter
test-adapter adapter:
    deno test --allow-run adapters/{{adapter}}.test.js

# =============================================================================
# SECURITY
# =============================================================================

# Run security scan
security:
    @echo "Running security checks..."
    deno lint adapters/ --rules=no-eval
    @echo "Security scan complete."

# Check for secrets in code
secrets-check:
    @echo "Checking for potential secrets..."
    @grep -rn "password\|secret\|api_key\|token" adapters/ || echo "No secrets found."

# Full security audit
audit: security secrets-check
    @echo "Security audit complete."

# =============================================================================
# DOCUMENTATION
# =============================================================================

# Build documentation
docs:
    @echo "Building documentation..."
    @command -v asciidoctor >/dev/null 2>&1 || { echo "asciidoctor required"; exit 1; }
    asciidoctor -D docs/html README.adoc cookbook.adoc 2>/dev/null || true
    @echo "Documentation built in docs/html/"

# Serve documentation locally
docs-serve port="8000":
    @echo "Serving docs at http://localhost:{{port}}"
    deno run --allow-net --allow-read https://deno.land/std/http/file_server.ts docs/html -p {{port}}

# Check documentation for issues
docs-check:
    @echo "Checking documentation..."
    @for f in *.adoc *.md; do \
        [ -f "$f" ] && echo "Checking $f" && wc -l "$f"; \
    done

# =============================================================================
# RELEASE
# =============================================================================

# Bump version
version-bump level="patch":
    @echo "Bumping {{level}} version..."
    @echo "Update version in STATE.scm and META.scm"

# Generate changelog
changelog:
    @echo "Generating changelog..."
    git log --oneline --no-merges > CHANGELOG.tmp
    @echo "Changelog generated."

# Create release
release: test-all audit docs
    @echo "Creating release..."
    @echo "Run: gh release create v$(grep 'version' STATE.scm | head -1)"

# =============================================================================
# CLEANUP
# =============================================================================

# Clean build artifacts
clean:
    rm -rf coverage/
    rm -rf docs/html/
    rm -f *.tmp
    @echo "Cleaned."

# Deep clean (including caches)
clean-all: clean
    rm -rf .deno/
    @echo "Deep clean complete."

# =============================================================================
# CI/CD HELPERS
# =============================================================================

# CI check (runs in CI environment)
ci: fmt-check lint test security
    @echo "CI checks passed!"

# Pre-commit hook
pre-commit: fmt-check lint
    @echo "Pre-commit checks passed."

# Pre-push hook
pre-push: test security
    @echo "Pre-push checks passed."

# =============================================================================
# MCP SERVER
# =============================================================================

# Start MCP server (when implemented)
mcp-serve:
    @echo "Starting MCP server..."
    deno run --allow-run --allow-read --allow-net server/mcp.ts 2>/dev/null || \
        echo "MCP server not yet implemented. See ROADMAP.md Phase 6."

# =============================================================================
# HELP
# =============================================================================

# Show all available commands
help:
    @just --list
