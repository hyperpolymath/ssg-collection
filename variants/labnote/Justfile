# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile — labnote-ssg
# Run 'just --list' to see all available commands

# Default recipe: show help
default:
    @just --list

# ─────────────────────────────────────────────────────────────────────────────
# Build & Development
# ─────────────────────────────────────────────────────────────────────────────

# Check all adapters for syntax errors
check:
    deno check adapters/*.js

# Format all code
fmt:
    deno fmt adapters/

# Format and check (no changes)
fmt-check:
    deno fmt --check adapters/

# Lint all code
lint:
    deno lint adapters/

# Run all checks (fmt + lint + check)
verify: fmt-check lint check
    @echo "✓ All checks passed"

# ─────────────────────────────────────────────────────────────────────────────
# Testing
# ─────────────────────────────────────────────────────────────────────────────

# Run unit tests
test:
    deno test --allow-run --allow-read tests/

# Run tests with coverage
test-cov:
    deno test --allow-run --allow-read --coverage=coverage/ tests/
    deno coverage coverage/

# Run end-to-end tests
test-e2e:
    deno test --allow-run --allow-read --allow-write tests/e2e/

# Run all tests (unit + e2e)
test-all: test test-e2e
    @echo "✓ All tests passed"

# ─────────────────────────────────────────────────────────────────────────────
# Adapter Operations
# ─────────────────────────────────────────────────────────────────────────────

# List all adapters
adapters:
    @echo "Available adapters (28):"
    @ls -1 adapters/*.js | xargs -n1 basename | sed 's/.js$//'

# Test specific adapter connection
adapter-test name:
    deno eval --allow-run "import * as a from './adapters/{{name}}.js'; console.log(await a.connect() ? '✓ Connected' : '✗ Failed');"

# Show adapter info
adapter-info name:
    deno eval "import * as a from './adapters/{{name}}.js'; console.log('Name:', a.name); console.log('Language:', a.language); console.log('Description:', a.description); console.log('Tools:', a.tools.map(t => t.name).join(', '));"

# ─────────────────────────────────────────────────────────────────────────────
# Security
# ─────────────────────────────────────────────────────────────────────────────

# Run security checks
security: lint
    @echo "Checking for security issues..."
    @grep -r "eval\|exec\|shell" adapters/ && echo "⚠ Potential security issues found" || echo "✓ No obvious security issues"
    @echo "✓ Security check complete"

# Audit dependencies (placeholder - no deps currently)
audit:
    @echo "No external dependencies to audit"
    @echo "✓ Audit complete"

# ─────────────────────────────────────────────────────────────────────────────
# Documentation
# ─────────────────────────────────────────────────────────────────────────────

# Generate docs
docs:
    @echo "Documentation is in AsciiDoc format"
    @echo "View: README.adoc, cookbook.adoc, CONTRIBUTING.md"

# Serve docs locally (requires asciidoctor)
docs-serve:
    asciidoctor README.adoc -o docs/index.html
    asciidoctor cookbook.adoc -o docs/cookbook.html
    @echo "Docs generated in docs/"

# ─────────────────────────────────────────────────────────────────────────────
# CI/CD
# ─────────────────────────────────────────────────────────────────────────────

# Run CI checks (what CI runs)
ci: verify test
    @echo "✓ CI checks passed"

# Pre-commit hook
pre-commit: fmt lint
    @echo "✓ Pre-commit checks passed"

# Pre-push hook
pre-push: verify test
    @echo "✓ Pre-push checks passed"

# ─────────────────────────────────────────────────────────────────────────────
# Maintenance
# ─────────────────────────────────────────────────────────────────────────────

# Clean generated files
clean:
    rm -rf coverage/
    rm -rf docs/*.html
    @echo "✓ Cleaned"

# Update SCM files timestamp
scm-update:
    @sed -i 's/updated . "[^"]*"/updated . "'"$(date +%Y-%m-%d)"'"/' STATE.scm
    @echo "✓ STATE.scm updated"

# Show project status
status:
    @echo "=== labnote-ssg Status ==="
    @echo "Adapters: 28"
    @echo "Branch: $(git branch --show-current)"
    @echo "Last commit: $(git log -1 --format='%h %s')"
    @grep -o 'overall-completion . [0-9]*' STATE.scm | head -1

# ─────────────────────────────────────────────────────────────────────────────
# Hub Synchronization
# ─────────────────────────────────────────────────────────────────────────────

# Sync adapters from hub (requires hub clone)
hub-sync hub_path:
    @echo "Syncing from {{hub_path}}/adapters/..."
    cp {{hub_path}}/adapters/*.js adapters/
    @echo "✓ Synced"

# Compare with hub
hub-diff hub_path:
    diff -rq adapters/ {{hub_path}}/adapters/ || true

# ─────────────────────────────────────────────────────────────────────────────
# Release
# ─────────────────────────────────────────────────────────────────────────────

# Prepare release
release-prep version:
    @echo "Preparing release {{version}}..."
    @sed -i 's/version . "[^"]*"/version . "{{version}}"/' STATE.scm
    @echo "✓ Updated STATE.scm to {{version}}"
    @echo "Don't forget to:"
    @echo "  1. Update CHANGELOG.md"
    @echo "  2. Commit: git commit -am 'chore(release): {{version}}'"
    @echo "  3. Tag: git tag v{{version}}"
    @echo "  4. Push: git push --tags"
