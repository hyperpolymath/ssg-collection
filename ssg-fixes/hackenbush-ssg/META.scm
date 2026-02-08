;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; META.scm — hackenbush-ssg

(define-module (hackenbush-ssg meta)
  #:export (architecture-decisions development-practices design-rationale language-rules))

(define language-rules
  '((mandatory-language . "Conway's Game of Life")
    (enforcement-level . "absolute")
    (rationale . "Each SSG satellite is the DEFINITIVE implementation for its language. hackenbush-ssg IS the Game of Life SSG - proving Turing-completeness of cellular automata.")
    (violations
     ("TypeScript" . "FORBIDDEN - use ReScript instead (Hyperpolymath Language Policy)")
     ("Python implementation" . "FORBIDDEN - unless SaltStack")
     ("Go implementation" . "FORBIDDEN - use Rust instead")
     ("Node.js/npm" . "FORBIDDEN - use Deno instead")
     ("JavaScript/TypeScript SSG logic" . "FORBIDDEN")
     ("Any conventional language for SSG logic" . "FORBIDDEN - defeats the purpose of this satellite"))
    (allowed-exceptions
     ("ReScript in runtime/" . "ALLOWED - host simulation and I/O (compiles to JS)")
     ("ReScript in adapters/" . "ALLOWED - MCP hub integration only")
     ("JavaScript (Deno glue)" . "ALLOWED ONLY where ReScript bindings cannot work"))
    (policy-enforcement
     (ci-workflow . ".github/workflows/language-policy.yml")
     (check-script . "scripts/check-language-policy.js")
     (pre-commit . "hooks/pre-commit (check banned extensions)"))
    (correct-implementation
     (core-logic . "RLE pattern files in src/")
     (simulator . "ReScript host in runtime/ (compiled to JS for Deno)")
     (mcp-adapter . "ReScript in adapters/"))))

(define architecture-decisions
  '((adr-001
     (title . "Game of Life as Programming Language")
     (status . "accepted")
     (date . "2025-12-17")
     (context . "Prove Turing-completeness of cellular automata for practical computation")
     (decision . "SSG logic encoded in Conway's Game of Life patterns (RLE format)")
     (consequences . ("Astronomical pattern complexity" "Proof-of-concept focus" "Educational value")))
    (adr-002
     (title . "Minimal Host Runtime")
     (status . "accepted")
     (date . "2025-12-17")
     (context . "Host must not contain SSG logic - only simulation + I/O")
     (decision . "ReScript host provides RLE parsing, simulation, and output only (runs via Deno)")
     (consequences . ("Clean separation" "Life patterns ARE the program" "Testable simulator" "Type-safe runtime")))
    (adr-004
     (title . "TypeScript to ReScript Migration")
     (status . "accepted")
     (date . "2025-12-30")
     (context . "Hyperpolymath Language Policy bans TypeScript in favor of ReScript")
     (decision . "Migrate runtime/host.ts to runtime/Host.res with Deno bindings")
     (consequences . ("Policy compliance" "Type-safe with OCaml-like inference" "Compiles to clean ES6 JS")))
    (adr-003
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "Part of hyperpolymath poly-ssg constellation")
     (decision . "Follow Rhodium Standard Repository guidelines")
     (consequences . ("RSR Gold target" "SHA-pinned actions" "SPDX headers")))))

(define development-practices
  '((code-style (languages . ("RLE patterns" "ReScript (host)" "ReScript (adapter)")))
    (security (sast . "CodeQL for workflow scanning") (credentials . "env vars only") (patterns . "RLE files are data - minimal attack surface"))
    (versioning (scheme . "SemVer 2.0.0"))
    (testing (pattern-validation . "CI validates .rle structure") (simulator-tests . "Deno test suite"))
    (policy-enforcement
     (banned . ("TypeScript" "Go" "Python (non-SaltStack)" "Node.js/npm"))
     (required . ("ReScript for app code" "Deno for JS runtime" "Rust for systems"))
     (ci-check . "language-policy.yml"))))

(define design-rationale
  '((why-game-of-life . "This is THE Game of Life SSG. It proves cellular automata can encode computation.")
    (why-not-conventional . "Conventional languages defeat the experimental purpose.")
    (philosophical-goal . "Demonstrate that Turing-completeness is sufficient for practical tasks.")))
