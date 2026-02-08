;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; META.scm — wagasm-ssg

(define-module (wagasm-ssg meta)
  #:export (architecture-decisions development-practices design-rationale language-rules strict-enforcement))

(define language-rules
  '((mandatory-language . "WebAssembly Text (WAT)")
    (enforcement-level . "ABSOLUTE")
    (status . "EXPERIMENTAL")
    (rationale . "wagasm-ssg exists to PROVE that an SSG can be written in pure WAT.")
    (violations
     ("TypeScript" . "ABSOLUTELY FORBIDDEN - was incorrectly used before")
     ("JavaScript" . "FORBIDDEN except for minimal host runtime I/O")
     ("AssemblyScript" . "FORBIDDEN - that's TypeScript in disguise")
     ("Rust compiled to WASM" . "FORBIDDEN - defeats the purpose")
     ("Any high-level language" . "FORBIDDEN for SSG logic"))
    (correct-implementation
     (core-ssg . "src/wagasm-ssg.wat - ALL SSG logic here")
     (host-runtime . "runtime/Host.res - ONLY I/O, no SSG logic (compiles to JS for Deno)")
     (mcp-adapter . "adapters/ in ReScript"))
    (policy-enforcement
     (ci-workflow . ".github/workflows/language-policy.yml")
     (check-script . "scripts/check-language-policy.js")
     (pre-commit . "hooks/pre-commit"))))

(define strict-enforcement
  '((what-host-can-do
     "ONLY provide these imports to WASM:
      - write_file(filename_ptr, filename_len, content_ptr, content_len)
      - read_file(filename_ptr, filename_len, buffer_ptr, buffer_len) -> bytes_read
      - log(msg_ptr, msg_len)
      That's IT. No templating. No markdown parsing. No routing logic.")
    (what-host-cannot-do
     "- Generate HTML
      - Parse markdown
      - Make routing decisions
      - Template processing
      - Any SSG logic whatsoever")
    (penalty
     "Any violation will result in the entire host runtime being deleted
      and rewritten to be compliant. This is not negotiable.")))

(define architecture-decisions
  '((adr-001
     (title . "Pure WAT Implementation")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "SSG satellites must be in their target language. WASM needs I/O from host.")
     (decision . "Core SSG in WAT. Host provides ONLY file I/O imports.")
     (consequences . ("Extreme challenge" "Educational value" "Proves WAT viability")))
    (adr-002
     (title . "No AssemblyScript")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "AssemblyScript is TypeScript that compiles to WASM")
     (decision . "AssemblyScript is FORBIDDEN - must use raw WAT")
     (consequences . ("Harder development" "True low-level WASM" "No TypeScript disguises")))
    (adr-003
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "Part of hyperpolymath ecosystem")
     (decision . "Follow Rhodium Standard Repository guidelines")
     (consequences . ("RSR Gold target" "SHA-pinned actions" "SPDX headers")))
    (adr-004
     (title . "Just + Must Build System")
     (status . "accepted")
     (date . "2025-12-22")
     (context . "Need portable, documented build commands")
     (decision . "Use Justfile for tasks, Mustfile for required checks")
     (consequences . ("Cross-platform" "Self-documenting" "CI integration")))
    (adr-005
     (title . "Git Hooks for Compliance")
     (status . "accepted")
     (date . "2025-12-22")
     (context . "Language compliance must be enforced pre-commit")
     (decision . "Pre-commit hook checks for forbidden languages in src/")
     (consequences . ("Catches violations early" "Prevents bad commits")))
    (adr-006
     (title . "SCM File Architecture")
     (status . "accepted")
     (date . "2025-12-22")
     (context . "Project metadata needs structured representation")
     (decision . "Use Scheme files: STATE, META, ECOSYSTEM, PLAYBOOK, AGENTIC, NEUROSYM")
     (consequences . ("Machine-readable" "S-expression consistency" "Documentation as code")))))

(define development-practices
  '((code-style
     (languages . ("WAT"))
     (format . "S-expression based")
     (comments . "Use ;; for comments")
     (memory . "Explicit memory management required"))
    (compilation
     (tool . "wat2wasm from WABT (WebAssembly Binary Toolkit)")
     (command . "wat2wasm src/wagasm-ssg.wat -o src/wagasm-ssg.wasm"))
    (security
     (sast . "CodeQL for workflow scanning")
     (credentials . "env vars only"))
    (versioning
     (scheme . "SemVer 2.0.0"))))

(define design-rationale
  '((why-wat
     "WAT is the human-readable form of WebAssembly.
      It's S-expression syntax (like Lisp/Scheme).
      wagasm-ssg proves that even a compilation target can be a source language.")
    (why-not-typescript
     "TypeScript was INCORRECTLY used before (wasm-ssg.ts and host.ts).
      This violated both core principles: the SSG must BE in its target language,
      AND the Hyperpolymath Language Policy bans TypeScript in favor of ReScript.
      Host runtime has been converted to ReScript (Host.res).
      Even AssemblyScript (TS→WASM) would be cheating.")
    (adr-007-ts-to-rescript
     (title . "TypeScript to ReScript Migration")
     (status . "accepted")
     (date . "2025-12-30")
     (context . "Hyperpolymath Language Policy bans TypeScript")
     (decision . "Migrate runtime/host.ts to runtime/Host.res")
     (consequences . ("Policy compliance" "Type-safe Deno host" "Consistent ecosystem")))))
