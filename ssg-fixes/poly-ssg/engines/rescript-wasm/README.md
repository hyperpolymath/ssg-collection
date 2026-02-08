# ReScript WASM-GC Backend

[![Build Status](https://github.com/hyperpolymath/poly-ssg/actions/workflows/ci.yml/badge.svg)](https://github.com/hyperpolymath/poly-ssg/actions)
image:https://img.shields.io/badge/License-MPL_2.0-blue.svg[MPL-2.0-or-later,link="https://opensource.org/licenses/MPL-2.0"]
[![OCaml](https://img.shields.io/badge/OCaml-%3E%3D4.14-orange)](https://ocaml.org/)
[![WebAssembly](https://img.shields.io/badge/WebAssembly-GC-654FF0)](https://webassembly.org/)

A WebAssembly-GC compilation backend for ReScript, targeting the WASM-GC proposal supported in all major browsers since late 2023.

## Overview

This project implements **Phase 1 MVP** of a Lambda IR to WebAssembly-GC compiler. It compiles a subset of ReScript's intermediate representation directly to WASM-GC, leveraging the browser's native garbage collector instead of embedding a GC runtime.

### Key Features

- **Native GC Integration** - Uses WASM-GC's built-in garbage collection (i31ref, structs, arrays)
- **Type-Safe Compilation** - Preserves ReScript's type safety through WASM-GC's type system
- **Browser Ready** - Output runs in Chrome 119+, Firefox 120+, Edge 119+, Safari 18+
- **Standalone Testing** - Includes minimal Lambda IR for independent development

## Installation

### Prerequisites

- OCaml >= 4.14
- Dune >= 3.0
- (Optional) wasm-tools for validation

### Build

```bash
cd engines/rescript-wasm
dune build
```

### Test

```bash
dune test
```

## Usage

### Generate WAT Output

```bash
# Simple addition function
dune exec rescript_wasm -- add

# Max function with conditionals
dune exec rescript_wasm -- max

# Multiple functions
dune exec rescript_wasm -- combined

# List all examples
dune exec rescript_wasm -- --list
```

### Example Output

Input (conceptual ReScript):
```rescript
let add = (a, b) => a + b
```

Output (WAT):
```wat
(module
  (type $string (array (mut i32)))
  (type $unit (struct ))

  (func $add (export "add")
    (param (ref i31)) (param (ref i31)) (result (ref i31))
    (ref.i31 (i32.add
      (i31.get_s (local.get 0))
      (i31.get_s (local.get 1))))
  )
)
```

## Architecture

```
Lambda IR  ──►  wasm_compile  ──►  WASM Instructions  ──►  wasm_emit  ──►  WAT
    │                │                     │
    ▼                ▼                     ▼
lambda_ir.ml    wasm_env.ml          wasm_types.ml
```

### Modules

| Module | Description |
|--------|-------------|
| `lambda_ir` | Minimal Lambda IR definition with free variable analysis |
| `wasm_types` | WASM-GC type system (values, instructions, modules) |
| `wasm_env` | Compilation environment (locals, globals, scoping) |
| `wasm_compile` | Core compilation from Lambda IR to WASM |
| `wasm_emit` | WAT text format pretty-printer |

## Type Mapping

| ReScript | WASM-GC | Notes |
|----------|---------|-------|
| `int` | `(ref i31)` | 31-bit signed integer |
| `float` | `f64` | 64-bit IEEE float |
| `bool` | `(ref i31)` | 0 = false, 1 = true |
| `string` | `(ref (array i32))` | UTF-8 byte array |
| `unit` | `(ref $unit)` | Empty struct singleton |

## Phase 1 MVP Capabilities

- [x] Integer and float constants
- [x] Arithmetic operations (+, -, *, /, mod)
- [x] Comparison operations (=, <>, <, <=, >, >=)
- [x] Simple functions with parameters
- [x] Let bindings and local variables
- [x] If-then-else conditionals
- [x] While and for loops
- [x] Function exports

### Future Phases

- **Phase 2**: Records, tuples, arrays (WASM-GC structs/arrays)
- **Phase 3**: Closures (captured variables, closure structs)
- **Phase 4**: Variants and pattern matching (tagged unions)
- **Phase 5**: JavaScript interop (imports, exports, glue code)
- **Phase 6**: Binary emission, optimization, source maps

## Browser Support

| Browser | Version | Release Date |
|---------|---------|--------------|
| Chrome | 119+ | November 2023 |
| Firefox | 120+ | November 2023 |
| Edge | 119+ | November 2023 |
| Safari | 18+ | September 2024 |

## Development

### Project Structure

```
engines/rescript-wasm/
├── dune-project          # Dune build configuration
├── justfile              # Task runner commands
├── lib/
│   ├── lambda_ir.ml      # Lambda IR types and analysis
│   ├── wasm_types.ml     # WASM-GC type definitions
│   ├── wasm_env.ml       # Compilation environment
│   ├── wasm_compile.ml   # Core compiler
│   └── wasm_emit.ml      # WAT emitter
├── bin/
│   └── main.ml           # CLI entry point
└── test/
    ├── test_compile.ml   # Unit tests (21 tests)
    └── fixtures/         # Expected WAT outputs
```

### Running Tests

```bash
# All tests
dune test

# With verbose output
dune test --force

# Specific test
dune exec ./test/test_compile.exe
```

### Validating Output

```bash
# Generate WAT
dune exec rescript_wasm -- add > add.wat

# Validate with wasm-tools
wasm-tools validate add.wat

# Run with wasmtime (requires GC support)
wasmtime --wasm-features gc add.wat
```

## Contributing

Contributions are welcome! Please ensure:

1. All tests pass (`dune test`)
2. Code follows OCaml conventions
3. New features include tests
4. WAT output validates with wasm-tools

## References

- [WebAssembly GC Proposal](https://github.com/WebAssembly/gc)
- [ReScript Compiler](https://github.com/rescript-lang/rescript-compiler)
- [WASM-GC Browser Support](https://webassembly.org/roadmap/)

## License

MIT License - see [LICENSE](../../LICENSE) for details.

## Keywords

WebAssembly, WASM-GC, ReScript, OCaml, compiler, functional programming, garbage collection, browser, JavaScript interop, Lambda IR, type-safe compilation
