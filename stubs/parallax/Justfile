# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
# parallax-ssg Justfile - Chapel Static Site Generator

# Default recipe
default: help

# ============================================================================
# CORE BUILD COMMANDS
# ============================================================================

# Build the SSG engine
build:
    @echo "Building parallax-ssg..."
    chpl src/parallel-press.chpl -o parallax-ssg --fast
    @echo "Build complete: ./parallax-ssg"

# Build with debug symbols
build-debug:
    @echo "Building parallax-ssg (debug)..."
    chpl src/parallel-press.chpl -o parallax-ssg-debug -g --checks
    @echo "Debug build complete: ./parallax-ssg-debug"

# Build with optimizations
build-release:
    @echo "Building parallax-ssg (release)..."
    chpl src/parallel-press.chpl -o parallax-ssg --fast --optimize --no-checks
    @echo "Release build complete: ./parallax-ssg"

# Clean build artifacts
clean:
    @echo "Cleaning build artifacts..."
    rm -f parallax-ssg parallax-ssg-debug
    rm -rf _site/
    rm -rf adapters/lib adapters/node_modules adapters/.bsb.lock
    @echo "Clean complete."

# ============================================================================
# TEST COMMANDS
# ============================================================================

# Run all tests
test: test-unit test-markdown test-frontmatter
    @echo "All tests passed!"

# Run unit tests
test-unit:
    @echo "Running unit tests..."
    ./parallax-ssg test-full || (just build && ./parallax-ssg test-full)

# Run markdown parser tests
test-markdown:
    @echo "Running markdown tests..."
    ./parallax-ssg test-markdown || (just build && ./parallax-ssg test-markdown)

# Run frontmatter parser tests
test-frontmatter:
    @echo "Running frontmatter tests..."
    ./parallax-ssg test-frontmatter || (just build && ./parallax-ssg test-frontmatter)

# Run end-to-end tests
test-e2e:
    @echo "Running E2E tests..."
    @mkdir -p test-site/content
    @echo "---\ntitle: Test Post\ndate: 2025-01-01\n---\n# Hello World\nThis is a test." > test-site/content/test.md
    ./parallax-ssg build --source=test-site/content --output=test-site/_site || (just build && ./parallax-ssg build --source=test-site/content --output=test-site/_site)
    @test -f test-site/_site/test.html && echo "E2E test passed!" || (echo "E2E test failed!" && exit 1)
    @rm -rf test-site

# Run all tests including E2E
test-all: test test-e2e
    @echo "All tests including E2E passed!"

# ============================================================================
# DEVELOPMENT COMMANDS
# ============================================================================

# Start development mode with file watching
dev:
    @echo "Starting development mode..."
    @echo "Note: Watch mode not yet implemented in Chapel engine"
    just build
    ./parallax-ssg build --verbose

# Generate site from content
generate:
    @echo "Generating site..."
    ./parallax-ssg build || (just build && ./parallax-ssg build)
    @echo "Site generated in _site/"

# Serve generated site locally (requires Python or simple HTTP server)
serve:
    @echo "Starting local server at http://localhost:8080..."
    @cd _site && python3 -m http.server 8080 2>/dev/null || (echo "Python not available. Install Python or use another HTTP server." && exit 1)

# ============================================================================
# ADAPTER COMMANDS
# ============================================================================

# Build ReScript MCP adapter
adapter-build:
    @echo "Building MCP adapter..."
    cd adapters && npm install && npm run build
    @echo "Adapter built successfully."

# Clean adapter build
adapter-clean:
    @echo "Cleaning adapter build..."
    cd adapters && npm run clean 2>/dev/null || rm -rf lib .bsb.lock
    @echo "Adapter cleaned."

# Watch adapter for changes
adapter-watch:
    @echo "Watching adapter for changes..."
    cd adapters && npm run watch

# ============================================================================
# LSP & TOOLING
# ============================================================================

# Start language server (placeholder - Chapel LSP)
lsp:
    @echo "Starting Chapel LSP..."
    @echo "Note: Chapel Language Server integration pending"
    @echo "Use chpl-language-server if installed"

# Compile a single Chapel file
compile file:
    @echo "Compiling {{file}}..."
    chpl {{file}} -o "$(basename {{file}} .chpl)"

# Check Chapel syntax without building
check:
    @echo "Checking Chapel syntax..."
    chpl --syntax-only src/parallel-press.chpl
    @echo "Syntax check passed."

# Format Chapel code (if chapel-fmt available)
fmt:
    @echo "Formatting Chapel code..."
    @chapel-fmt src/*.chpl 2>/dev/null || echo "chapel-fmt not installed. Install via: pip install chapel-fmt"

# ============================================================================
# CI/CD COMMANDS
# ============================================================================

# Run CI pipeline locally
ci: check build test
    @echo "CI pipeline passed!"

# Run security checks
security:
    @echo "Running security checks..."
    @echo "Checking for forbidden languages in src/..."
    @find src/ -type f \( -name "*.py" -o -name "*.js" -o -name "*.ts" -o -name "*.cpp" \) 2>/dev/null && (echo "ERROR: Forbidden languages found!" && exit 1) || echo "OK: Chapel only in src/"
    @echo "Checking for secrets..."
    @grep -r "password\|secret\|api_key\|token" src/ 2>/dev/null && echo "WARNING: Possible secrets in code" || echo "OK: No obvious secrets"
    @echo "Security checks complete."

# Lint Chapel code
lint:
    @echo "Linting Chapel code..."
    @chpl --warn-unstable src/parallel-press.chpl -o /dev/null 2>&1 || true
    @echo "Lint complete."

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate documentation
docs:
    @echo "Generating documentation..."
    @echo "Note: Chapel doc generation via chpldoc"
    chpldoc src/parallel-press.chpl -o docs/api/ 2>/dev/null || echo "chpldoc not available in PATH"

# Serve documentation locally
docs-serve:
    @echo "Serving documentation..."
    @cd docs && python3 -m http.server 8081 2>/dev/null || echo "Python not available"

# ============================================================================
# CONTAINER COMMANDS
# ============================================================================

# Build container image
container-build:
    @echo "Building container image..."
    podman build -t parallax-ssg:latest . 2>/dev/null || docker build -t parallax-ssg:latest .

# Run in container
container-run:
    @echo "Running in container..."
    podman run --rm -v $(pwd):/workspace:Z parallax-ssg:latest build 2>/dev/null || docker run --rm -v $(pwd):/workspace parallax-ssg:latest build

# ============================================================================
# RELEASE COMMANDS
# ============================================================================

# Create a release build
release version:
    @echo "Creating release {{version}}..."
    just clean
    just build-release
    just test-all
    @echo "Release {{version}} ready."

# Tag release in git
tag version:
    @echo "Tagging release {{version}}..."
    git tag -a "v{{version}}" -m "Release v{{version}}"
    @echo "Tagged v{{version}}. Push with: git push origin v{{version}}"

# ============================================================================
# UTILITY COMMANDS
# ============================================================================

# Show help
help:
    @echo "parallax-ssg - Chapel Static Site Generator"
    @echo ""
    @echo "Core Commands:"
    @echo "  just build          Build the SSG engine"
    @echo "  just build-debug    Build with debug symbols"
    @echo "  just build-release  Build optimized release"
    @echo "  just clean          Clean build artifacts"
    @echo ""
    @echo "Test Commands:"
    @echo "  just test           Run all tests"
    @echo "  just test-unit      Run unit tests"
    @echo "  just test-markdown  Run markdown parser tests"
    @echo "  just test-e2e       Run end-to-end tests"
    @echo "  just test-all       Run all tests including E2E"
    @echo ""
    @echo "Development Commands:"
    @echo "  just dev            Start development mode"
    @echo "  just generate       Generate site from content"
    @echo "  just serve          Serve generated site locally"
    @echo ""
    @echo "Adapter Commands:"
    @echo "  just adapter-build  Build ReScript MCP adapter"
    @echo "  just adapter-clean  Clean adapter build"
    @echo "  just adapter-watch  Watch adapter for changes"
    @echo ""
    @echo "Tooling Commands:"
    @echo "  just lsp            Start language server"
    @echo "  just compile FILE   Compile a Chapel file"
    @echo "  just check          Check syntax without building"
    @echo "  just fmt            Format Chapel code"
    @echo ""
    @echo "CI/CD Commands:"
    @echo "  just ci             Run CI pipeline locally"
    @echo "  just security       Run security checks"
    @echo "  just lint           Lint Chapel code"
    @echo ""
    @echo "Documentation:"
    @echo "  just docs           Generate documentation"
    @echo "  just docs-serve     Serve documentation locally"
    @echo ""
    @echo "Container Commands:"
    @echo "  just container-build  Build container image"
    @echo "  just container-run    Run in container"
    @echo ""
    @echo "Release Commands:"
    @echo "  just release VERSION  Create release build"
    @echo "  just tag VERSION      Tag release in git"

# Show version
version:
    @echo "parallax-ssg v1.0.1"
    @chpl --version 2>/dev/null || echo "Chapel not installed"

# Show environment info
info:
    @echo "parallax-ssg Environment Info"
    @echo "=============================="
    @echo "Chapel:"
    @chpl --version 2>/dev/null || echo "  Not installed"
    @echo ""
    @echo "Node.js:"
    @node --version 2>/dev/null || echo "  Not installed"
    @echo ""
    @echo "ReScript:"
    @cd adapters && npx rescript -version 2>/dev/null || echo "  Not installed"
    @echo ""
    @echo "Container Runtime:"
    @podman --version 2>/dev/null || docker --version 2>/dev/null || echo "  None installed"
