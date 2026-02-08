# Tests

Test suite for doit-ssg adapters and integration.

## Structure

```
tests/
├── adapters/           # Adapter interface tests
│   └── adapter_test.js # Tests all adapter exports
├── e2e/                # End-to-end tests
│   └── integration_test.js # Integration tests
└── README.md           # This file
```

## Running Tests

```bash
# Run all tests
just test-all

# Run unit tests only
just test-unit

# Run E2E tests only
just test-e2e

# Run with Deno directly
deno test tests/ --allow-read
```

## Test Coverage

Target: 70% code coverage

```bash
# Run with coverage
deno test --coverage=coverage tests/
deno coverage coverage/
```

## Writing Tests

Use Deno's built-in test framework:

```javascript
import { assertEquals, assertExists } from "https://deno.land/std@0.208.0/assert/mod.ts";

Deno.test("Test name", async () => {
  // Test code
});
```
