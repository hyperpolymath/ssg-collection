# GitHub Copilot Instructions for poly-ssg

## Project Overview

poly-ssg is a polyglot static site generator framework implementing the same SSG contract in multiple programming languages. Each engine in `engines/` is a complete SSG implementation.

## Code Style Guidelines

### OCaml (rescript-wasm)

- Use descriptive variable names (no single letters except loop indices)
- Add type annotations to all public functions in `.mli` files
- Follow OCaml conventions: snake_case for functions/values, PascalCase for modules/types
- Prefer pattern matching over if-else chains
- Use `option` and `result` types for error handling, not exceptions
- Document functions with ocamldoc comments: `(** Description *)`

### Forth (forth-estate)

- Use stack comments for all words: `( inputs -- outputs )`
- Keep stack effects shallow (prefer < 4 items)
- Use descriptive word names
- Factor complex operations into smaller words

## Security Considerations

### DO NOT generate code that:
- Uses `Obj.magic` (breaks type safety)
- Uses `Marshal` for untrusted input (code execution risk)
- Executes shell commands with user input
- Disables compiler warnings

### ALWAYS:
- Validate input bounds before array access
- Handle all pattern match cases (no `_` catch-all unless intentional)
- Use `[@warning "-X"]` sparingly and document why

## WASM-GC Specific

When generating WASM-GC code:
- Integers use `(ref i31)` for ReScript compatibility
- Floats use `f64` (unboxed)
- Strings are `(array (mut i32))` UTF-8 byte arrays
- Use `struct.new`, `struct.get`, `array.new_fixed` for GC types
- Always wrap/unwrap i31refs for arithmetic: `(ref.i31 (i32.add (i31.get_s a) (i31.get_s b)))`

## File Locations

- OCaml source: `engines/rescript-wasm/lib/`
- OCaml interfaces: `engines/rescript-wasm/lib/*.mli`
- Tests: `engines/rescript-wasm/test/`
- WAT fixtures: `engines/rescript-wasm/test/fixtures/`

## Testing

- All new features need tests in `test_compile.ml`
- Use the existing test helpers: `test`, `assert_eq`, `assert_contains`
- Run tests with `dune test`

## Commit Messages

Follow conventional commits:
- `feat:` new features
- `fix:` bug fixes
- `chore:` maintenance
- `docs:` documentation
- `test:` test changes
