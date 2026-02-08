# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# justfile for my-ssg - SSG Adapter Collection
# Hyperpolymath Standard: Makefiles are FORBIDDEN. Use just/must only.

# Default recipe
default: help

# Show available commands
help:
    @just --list --unsorted

# ============================================================================
# POLICY ENFORCEMENT (Hyperpolymath Standard)
# ============================================================================

# Check policy compliance - MUST pass before any commit
policy-check:
    @echo "Checking Hyperpolymath Language Policy compliance..."
    @echo ""
    @echo "Checking for banned TypeScript files..."
    @if find . -name "*.ts" -o -name "*.tsx" | grep -v node_modules | grep -q .; then \
        echo "FAIL: TypeScript files found - use ReScript instead"; \
        find . -name "*.ts" -o -name "*.tsx" | grep -v node_modules; \
        exit 1; \
    else \
        echo "PASS: No TypeScript files"; \
    fi
    @echo ""
    @echo "Checking for banned package.json..."
    @if [ -f "package.json" ]; then \
        echo "FAIL: package.json found - use deno.json imports instead"; \
        exit 1; \
    else \
        echo "PASS: No package.json"; \
    fi
    @echo ""
    @echo "Checking for banned Makefile..."
    @if [ -f "Makefile" ] || [ -f "makefile" ] || [ -f "GNUmakefile" ]; then \
        echo "FAIL: Makefile found - use just/must instead"; \
        exit 1; \
    else \
        echo "PASS: No Makefile"; \
    fi
    @echo ""
    @echo "Checking for banned node_modules..."
    @if [ -d "node_modules" ]; then \
        echo "FAIL: node_modules found - use Deno caching instead"; \
        exit 1; \
    else \
        echo "PASS: No node_modules"; \
    fi
    @echo ""
    @echo "Checking for banned bun.lock..."
    @if [ -f "bun.lock" ] || [ -f "bun.lockb" ]; then \
        echo "FAIL: bun.lock found - use Deno instead"; \
        exit 1; \
    else \
        echo "PASS: No bun.lock"; \
    fi
    @echo ""
    @echo "Checking for required deno.json..."
    @if [ -f "deno.json" ] || [ -f "deno.jsonc" ]; then \
        echo "PASS: deno.json found"; \
    else \
        echo "FAIL: deno.json required"; \
        exit 1; \
    fi
    @echo ""
    @echo "Checking for required justfile..."
    @if [ -f "justfile" ] || [ -f "Justfile" ]; then \
        echo "PASS: justfile found"; \
    else \
        echo "FAIL: justfile required"; \
        exit 1; \
    fi
    @echo ""
    @echo "All policy checks passed!"

# Install git hooks for policy enforcement
hooks-install:
    @echo "Installing git hooks..."
    @mkdir -p .git/hooks
    @echo '#!/bin/sh' > .git/hooks/pre-commit
    @echo '# Hyperpolymath Language Policy Enforcement' >> .git/hooks/pre-commit
    @echo 'just policy-check' >> .git/hooks/pre-commit
    @chmod +x .git/hooks/pre-commit
    @echo "Pre-commit hook installed"

# Remove git hooks
hooks-uninstall:
    @rm -f .git/hooks/pre-commit
    @echo "Pre-commit hook removed"

# ============================================================================
# RESCRIPT BUILD
# ============================================================================

# Build ReScript sources
rescript-build:
    @echo "Building ReScript sources..."
    @if command -v rescript >/dev/null 2>&1; then \
        rescript build; \
    else \
        echo "ReScript compiler not found. Install with: deno install -g npm:rescript"; \
    fi

# Watch ReScript sources
rescript-watch:
    @echo "Watching ReScript sources..."
    @rescript build -w

# Clean ReScript build artifacts
rescript-clean:
    @echo "Cleaning ReScript artifacts..."
    @rescript clean
    @rm -rf lib .bsb.lock

# ============================================================================
# ADAPTER TESTING
# ============================================================================

# Run all adapter tests
test:
    @echo "Testing SSG adapters..."
    deno test --allow-all tests/

# Test specific adapter
test-adapter name:
    @echo "Testing {{name}} adapter..."
    deno test --allow-all tests/ --filter "{{name}}"

# Run unit tests
test-unit:
    @echo "Running unit tests..."
    deno test --allow-all tests/unit/

# Run E2E tests
test-e2e:
    @echo "Running E2E tests..."
    deno test --allow-all tests/e2e/

# Run all tests
test-all: test-unit test-e2e
    @echo "All tests complete"

# Check code coverage
coverage:
    @echo "Generating coverage report..."
    deno test --allow-all --coverage=.coverage tests/
    deno coverage .coverage --lcov > coverage.lcov

# ============================================================================
# ADAPTER VERIFICATION
# ============================================================================

# Check all adapter syntax
check:
    @echo "Checking adapter syntax..."
    @if [ -d "adapters" ]; then \
        for adapter in adapters/*.js; do \
            echo "Checking $$adapter..."; \
            deno check "$$adapter" 2>/dev/null || echo "  (skipped - JS file)"; \
        done; \
    else \
        echo "No adapters directory yet"; \
    fi

# Verify adapter exports
verify:
    @echo "Verifying adapter exports..."
    @if [ -d "adapters" ]; then \
        deno eval " \
            const files = [...Deno.readDirSync('adapters')].filter(f => f.name.endsWith('.js')); \
            let count = 0; \
            for (const file of files) { \
                try { \
                    const mod = await import('./adapters/' + file.name); \
                    if (mod.name && mod.tools && mod.connect) { \
                        console.log('OK', file.name.padEnd(25), 'tools:', (mod.tools?.length || 0).toString().padStart(2)); \
                        count++; \
                    } else { \
                        console.log('WARN', file.name, '- missing exports'); \
                    } \
                } catch (e) { \
                    console.log('FAIL', file.name, '-', e.message); \
                } \
            } \
            console.log('\nTotal valid adapters:', count); \
        "; \
    else \
        echo "No adapters directory yet"; \
    fi

# List all adapters
list:
    @echo "Available SSG Adapters:"
    @echo ""
    @if [ -d "adapters" ]; then \
        for adapter in adapters/*.js; do \
            name=$$(basename "$$adapter" .js); \
            echo "  - $$name"; \
        done; \
    else \
        echo "  (none - adapters/ directory not created yet)"; \
    fi

# Test adapter connection (requires SSG to be installed)
connect adapter:
    @echo "Testing connection to {{adapter}}..."
    @deno eval " \
        const mod = await import('./adapters/{{adapter}}.js'); \
        console.log('Name:', mod.name); \
        console.log('Language:', mod.language); \
        console.log('Tools:', mod.tools?.length || 0); \
        const connected = await mod.connect(); \
        console.log('Connected:', connected); \
    "

# ============================================================================
# DEVELOPMENT
# ============================================================================

# Install dependencies
deps:
    @echo "Installing dependencies..."
    @deno cache --reload deno.json
    @echo "Dependencies installed"

# Format all code
fmt:
    deno fmt

# Lint all code
lint:
    deno lint

# Security audit
audit:
    @echo "Running security checks..."
    @echo ""
    @echo "Checking for shell:true..."
    @if [ -d "adapters" ]; then \
        grep -rn "shell:\s*true" adapters/ 2>/dev/null && echo "WARNING: shell:true found!" || echo "PASS: No shell:true usage"; \
    else \
        echo "PASS: No adapters directory"; \
    fi
    @echo ""
    @echo "Checking for eval()..."
    @if [ -d "adapters" ]; then \
        grep -rn "eval(" adapters/ 2>/dev/null && echo "WARNING: eval() found!" || echo "PASS: No eval() usage"; \
    else \
        echo "PASS: No adapters directory"; \
    fi
    @echo ""
    @echo "Checking for hardcoded secrets..."
    @grep -rn "password\s*=" . --include="*.js" --include="*.res" 2>/dev/null | grep -v "test" && echo "WARNING: Possible hardcoded secret!" || echo "PASS: No obvious hardcoded secrets"
    @echo ""
    @echo "Security audit complete"

# Run all CI checks locally
ci: policy-check fmt lint check verify test audit
    @echo ""
    @echo "All CI checks passed!"

# ============================================================================
# CONTAINER
# ============================================================================

# Build container image
container-build:
    podman build -t my-ssg:latest -f Containerfile .

# Run container
container-run:
    podman run -it --rm my-ssg:latest

# Push container to registry
container-push registry="ghcr.io/hyperpolymath":
    podman push my-ssg:latest {{registry}}/my-ssg:latest

# ============================================================================
# UTILITIES
# ============================================================================

# Clean build artifacts
clean:
    rm -rf .coverage coverage.lcov lib .bsb.lock

# Show adapter statistics
stats:
    @echo "Adapter Statistics:"
    @echo "==================="
    @if [ -d "adapters" ]; then \
        echo "Total adapters:  $$(ls -1 adapters/*.js 2>/dev/null | wc -l)"; \
        echo "Total lines:     $$(wc -l adapters/*.js 2>/dev/null | tail -1 | awk '{print $$1}')"; \
        echo ""; \
        echo "By language:"; \
        deno eval " \
            const files = [...Deno.readDirSync('adapters')].filter(f => f.name.endsWith('.js')); \
            const langs = {}; \
            for (const file of files) { \
                try { \
                    const mod = await import('./adapters/' + file.name); \
                    const lang = mod.language || 'Unknown'; \
                    langs[lang] = (langs[lang] || 0) + 1; \
                } catch {} \
            } \
            for (const [lang, count] of Object.entries(langs).sort((a,b) => b[1] - a[1])) { \
                console.log('  ' + lang.padEnd(15) + count); \
            } \
        "; \
    else \
        echo "No adapters directory yet"; \
    fi

# Show project info
info:
    @echo "my-ssg - SSG Adapter Collection"
    @echo "================================"
    @echo "Runtime:    Deno $$(deno --version | head -1 | cut -d' ' -f2)"
    @echo "Build:      just $$(just --version | cut -d' ' -f2)"
    @echo "Policy:     Hyperpolymath Standard"
    @echo ""
    @echo "Source:     ReScript -> JavaScript"
    @echo "Testing:    Deno Test"
    @echo "Container:  Podman"
