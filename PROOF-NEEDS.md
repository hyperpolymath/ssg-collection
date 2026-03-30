# PROOF-NEEDS.md
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

## Current State

- **LOC**: ~108,600
- **Languages**: ReScript, Ada, Idris2, Zig, Nickel
- **Existing ABI proofs**: `implementations/eclipse/src/abi/*.idr` (template-level)
- **Dangerous patterns**:
  - `implementations/rats/src/Main.res`: `Obj.magic(err)` for error stringification
  - `implementations/eclipse/tests/`: `Obj.magic` in test assertions (Lexer_test, Parser_test)
  - `implementations/pharos/noteg-lang/src/lsp/Server.res`: `Obj.magic` for Dict operations

## What Needs Proving

### Noteg-Lang Compiler (implementations/eclipse/noteg-lang/)
- `Compiler.res`, `Interpreter.res`, `Lexer.res`, `Parser.res`
- Prove: parser produces well-formed ASTs
- Prove: interpreter evaluation matches compiler output

### Noteg-Lang LSP (implementations/pharos/noteg-lang/src/lsp/Server.res)
- `Obj.magic` for document store operations — should use typed Dict
- Prove: LSP responses conform to LSP protocol specification

### Ada Engine (implementations/eclipse/engine/src/)
- `engine.adb`, `engine.ads` — static site generation engine in Ada
- Should have SPARK annotations for pre/post conditions on file generation

### Build System Correctness (implementations/eclipse/ssg/)
- `build.res`, `env.res`, `types.res` — build configuration and execution
- Prove: build produces deterministic output for the same input

## Recommended Prover

- **Idris2** for noteg-lang compiler/parser correctness
- **SPARK** for Ada engine components

## Priority

**LOW** — SSG collection is a build tool, not security-critical. The `Obj.magic` uses are minor (tests, error logging, Dict operations). Noteg-lang proofs would be nice but are not urgent.

## Template ABI Cleanup (2026-03-29)

Template ABI removed -- was creating false impression of formal verification.
The removed files (Types.idr, Layout.idr, Foreign.idr) contained only RSR template
scaffolding with unresolved {{PROJECT}}/{{AUTHOR}} placeholders and no domain-specific proofs.
