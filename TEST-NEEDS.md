# TEST-NEEDS: ssg-collection

## Current State

| Category | Count | Details |
|----------|-------|---------|
| **Source modules** | 70+ | ReScript across 3 SSG implementations (rats, odd, eclipse) -- adapters, MCP, parsers, engines, a11y |
| **Unit tests** | ~6 | Engine_test.res (~32 assertions), Lexer_test.res (~42), Parser_test.res (~45), Adapter_test.res (~16), Bernoulli_test.res (~34), test_utils.js |
| **Integration tests** | 0 | None |
| **E2E tests** | 1 | Adapter_test.res in eclipse/tests/e2e/ (~16 assertions) |
| **Benchmarks** | 0 | None |
| **Fuzz tests** | 0 | placeholder.txt only |

## What's Missing

### P2P Tests (CRITICAL)
- [ ] No tests for rats MCP protocol handler with actual MCP clients
- [ ] No tests for adapter registry loading and dispatch
- [ ] No tests for cross-implementation compatibility

### E2E Tests
- [ ] Only 1 e2e test file for 3 entire SSG implementations
- [ ] No test that actually builds a static site end-to-end
- [ ] No test for the noteg-lang LSP server
- [ ] No test for the MCP server endpoints

### Aspect Tests
- [ ] **Security**: No XSS tests for HTML output, no path traversal tests for file operations
- [ ] **Performance**: No SSG build throughput benchmarks (10/100/1000 pages)
- [ ] **Concurrency**: No parallel build tests
- [ ] **Error handling**: No tests for malformed templates, missing assets, broken links

### Benchmarks Needed
- [ ] Static site build time (small/medium/large sites)
- [ ] Parser throughput for noteg-lang
- [ ] Adapter loading time

### Self-Tests
- [ ] No self-diagnostic mode for any SSG

## FLAGGED ISSUES
- **70+ source files across 3 implementations with ~6 test files** = grossly undertested
- **rats (MCP server) has 0 tests** -- entire MCP implementation untested
- **a11y validation module has 0 tests** -- accessibility claims unverified
- **fuzz/placeholder.txt** -- fake fuzz testing claim

## Priority: P0 (CRITICAL)
