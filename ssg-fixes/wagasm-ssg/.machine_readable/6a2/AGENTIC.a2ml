;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; AGENTIC.scm — wagasm-ssg

(define-module (wagasm-ssg agentic)
  #:export (agent-capabilities tool-definitions mcp-integration collaboration-patterns))

(define agent-capabilities
  '((project-type . "pure-wat-ssg")
    (status . "EXPERIMENTAL")

    (core-capabilities
     ((wat-compilation . "Compile WAT to WASM binary")
      (wasm-validation . "Validate WASM module structure")
      (site-generation . "Generate static HTML sites")
      (host-execution . "Run WASM with Deno host")))

    (language-constraints
     ((allowed . ("WAT" "ReScript for adapter"))
      (forbidden . ("TypeScript" "JavaScript" "AssemblyScript" "Rust" "Python"))
      (enforcement . "ABSOLUTE - violations result in deletion")))

    (agent-instructions
     ((never-suggest . "Rewriting in TypeScript or high-level language")
      (never-add . "SSG logic to host runtime")
      (always-verify . "Language compliance before commits")
      (always-use . "Pure WAT for all SSG logic")))))

(define tool-definitions
  '((wagasm-compile
     ((description . "Compile WAT to WASM binary")
      (command . "wat2wasm src/wagasm-ssg.wat -o src/wagasm-ssg.wasm")
      (requires . "WABT (WebAssembly Binary Toolkit)")
      (output . "src/wagasm-ssg.wasm")))

    (wagasm-validate
     ((description . "Validate WASM module")
      (command . "wasm-validate src/wagasm-ssg.wasm")
      (requires . "WABT")
      (success . "Exit code 0")))

    (wagasm-build
     ((description . "Build and generate site")
      (command . "deno run --allow-read --allow-write runtime/host.ts")
      (requires . "Deno, compiled WASM")
      (output . "_site/")))

    (wagasm-test
     ((description . "Run full test suite")
      (command . "just test")
      (stages . ("compile" "validate" "run" "verify"))))

    (wagasm-clean
     ((description . "Clean build artifacts")
      (command . "rm -rf src/*.wasm _site/")
      (safe . #t)))))

(define mcp-integration
  '((adapter-location . "adapters/src/WagasmAdapter.res")
    (adapter-language . "ReScript")
    (hub . "poly-ssg-mcp")

    (exposed-tools
     (("wagasm_compile" . "Compile WAT to WASM")
      ("wagasm_build" . "Build site with Deno host")
      ("wagasm_validate" . "Validate WAT syntax")
      ("wagasm_version" . "Get version information")))

    (connection-requirements
     ((wat2wasm . "WABT must be installed")
      (deno . "Deno runtime required")
      (wasm-file . "Compiled WASM must exist for build")))

    (integration-status . "pending-hub-connection")))

(define collaboration-patterns
  '((human-agent-workflow
     ((human . "Defines content structure and templates")
      (agent . "Implements WAT logic for processing")
      (review . "Human verifies WAT correctness")
      (iterate . "Refine based on output")))

    (multi-agent-patterns
     ((code-agent . "Writes WAT implementation")
      (review-agent . "Verifies language compliance")
      (test-agent . "Validates WASM output")
      (doc-agent . "Updates documentation")))

    (guardrails
     ((pre-commit . "Language compliance check")
      (pre-push . "Full test suite")
      (ci-gate . "All checks must pass")
      (review-required . "WAT changes need careful review")))

    (escalation
     ((language-violation . "STOP - do not proceed")
      (host-logic-creep . "WARN - review host.ts changes")
      (test-failure . "BLOCK - fix before merge")))))
