# wagasm-ssg Tests

Test suite for the pure WebAssembly Text SSG.

## Test Categories

### Compilation Tests
- `test-compile`: Verify WAT compiles without errors
- `test-validate`: Verify WASM binary is valid

### Compliance Tests
- `test-language`: No forbidden languages in src/
- `test-host-io`: Host runtime is I/O only

### Integration Tests
- `test-build`: Verify site generation works

### E2E Tests
- Located in `tests/e2e/`
- Run with `just test-e2e`

## Running Tests

```bash
# All tests
just test

# Specific test
just test-compile

# E2E tests
just test-e2e

# Everything
just test-all
```

## Writing E2E Tests

Create shell scripts in `tests/e2e/`:

```bash
#!/bin/bash
# tests/e2e/test-example.sh
set -e

# Your test logic here
just build
grep -q "<!DOCTYPE html>" _site/index.html
echo "PASS: Example test"
```
