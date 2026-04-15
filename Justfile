# SPDX-License-Identifier: PMPL-1.0-or-later
# SSG Collection - consolidated monorepo justfile
# Copyright (c) 2024-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Default recipe: list all available recipes
import? "contractile.just"

default:
    @just --list

# List all implementations
list-implementations:
    @ls implementations/

# List all variants
list-variants:
    @ls variants/

# List all stubs
list-stubs:
    @ls stubs/

# Count files in each implementation
count-implementations:
    @for dir in implementations/*/; do printf "%4d %s\n" "$$(find $$dir -type f | wc -l)" "$$dir"; done | sort -rn

# Count files in each variant
count-variants:
    @for dir in variants/*/; do printf "%4d %s\n" "$$(find $$dir -type f | wc -l)" "$$dir"; done | sort -rn

# Show overall statistics
stats:
    @echo "Implementations: $$(ls implementations/ | wc -l)"
    @echo "Variants: $$(ls variants/ | wc -l)"
    @echo "Stubs: $$(ls stubs/ | wc -l)"
    @echo "Total files: $$(find implementations/ variants/ stubs/ -type f | wc -l)"

# Build a specific implementation (if it has a justfile or Cargo.toml)
build name:
    @if [ -f "implementations/{{name}}/justfile" ]; then cd implementations/{{name}} && just build; elif [ -f "implementations/{{name}}/Cargo.toml" ]; then cd implementations/{{name}} && cargo build --release; else echo "No build system found for {{name}}"; fi

# Clean build artifacts across all implementations
clean:
    @find implementations/ -name target -type d -exec rm -rf {} + 2>/dev/null; echo "Cleaned build artifacts"

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"

# Self-diagnostic — checks dependencies, permissions, paths
doctor:
    @echo "Running diagnostics for ssg-collection..."
    @echo "Checking required tools..."
    @command -v just >/dev/null 2>&1 && echo "  [OK] just" || echo "  [FAIL] just not found"
    @command -v git >/dev/null 2>&1 && echo "  [OK] git" || echo "  [FAIL] git not found"
    @echo "Checking for hardcoded paths..."
    @grep -rn '$HOME\|$ECLIPSE_DIR' --include='*.rs' --include='*.ex' --include='*.res' --include='*.gleam' --include='*.sh' . 2>/dev/null | head -5 || echo "  [OK] No hardcoded paths"
    @echo "Diagnostics complete."

# Auto-repair common issues
heal:
    @echo "Attempting auto-repair for ssg-collection..."
    @echo "Fixing permissions..."
    @find . -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
    @echo "Cleaning stale caches..."
    @rm -rf .cache/stale 2>/dev/null || true
    @echo "Repair complete."

# Guided tour of key features
tour:
    @echo "=== ssg-collection Tour ==="
    @echo ""
    @echo "1. Project structure:"
    @ls -la
    @echo ""
    @echo "2. Available commands: just --list"
    @echo ""
    @echo "3. Read README.adoc for full overview"
    @echo "4. Read EXPLAINME.adoc for architecture decisions"
    @echo "5. Run 'just doctor' to check your setup"
    @echo ""
    @echo "Tour complete! Try 'just --list' to see all available commands."

# Open feedback channel with diagnostic context
help-me:
    @echo "=== ssg-collection Help ==="
    @echo "Platform: $(uname -s) $(uname -m)"
    @echo "Shell: $SHELL"
    @echo ""
    @echo "To report an issue:"
    @echo "  https://github.com/hyperpolymath/ssg-collection/issues/new"
    @echo ""
    @echo "Include the output of 'just doctor' in your report."


# Print the current CRG grade (reads from READINESS.md '**Current Grade:** X' line)
crg-grade:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    echo "$$grade"

# Generate a shields.io badge markdown for the current CRG grade
# Looks for '**Current Grade:** X' in READINESS.md; falls back to X
crg-badge:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    case "$$grade" in \
      A) color="brightgreen" ;; B) color="green" ;; C) color="yellow" ;; \
      D) color="orange" ;; E) color="red" ;; F) color="critical" ;; \
      *) color="lightgrey" ;; esac; \
    echo "[![CRG $$grade](https://img.shields.io/badge/CRG-$$grade-$$color?style=flat-square)](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)"
