# DevTools CLI - Ephapax Implementation

A command-line developer utilities tool written in Ephapax.

## Status

**Specification Only** - Waiting for Ephapax compiler completion.

## Features Demonstrated

### Linear Type System
- Linear ownership for file handles and strings
- Explicit region-based memory management
- Safe resource handling without garbage collection

### Region-Based Memory
- Scoped allocations with bulk deallocation
- `@r` annotations for region-local allocations
- Automatic cleanup when regions exit

### Pattern Matching
- Exhaustive command parsing
- Option types for nullable values
- Result types for error handling

## Commands

Same functionality as Python version:
- JSON operations (format, validate, minify)
- File operations (count, size, lines)
- Text operations (encode, decode, upper, lower)
- Hash operations (md5, sha1, sha256, sha512)
- Search with regex patterns

## Build (Future)

```bash
# When Ephapax compiler is ready:
ephapax build --target wasm32 devtools.epx

# Run with Wasmtime/Wasmer:
wasmtime devtools.wasm json format input.json
```

## License

SPDX-License-Identifier: MPL-2.0-or-later
