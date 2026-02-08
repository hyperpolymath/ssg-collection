# SPDX-License-Identifier: PMPL-1.0-or-later
# SSG Collection - consolidated monorepo justfile
# Copyright (c) 2024-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

# Default recipe: list all available recipes
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
