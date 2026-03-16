;;; STATE.scm — wagasm-ssg
;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define metadata
  '((version . "0.1.0")
    (updated . "2025-12-17")
    (project . "wagasm-ssg")
    (required-language . "WebAssembly Text (WAT)")
    (status . "EXPERIMENTAL")))

(define language-enforcement
  '((primary-language . "WAT")
    (description . "WebAssembly Text format - the human-readable form of WASM")
    (file-extension . ".wat")
    (compiler . "wat2wasm (wabt)")
    (host-runtime . "Deno/ReScript provides I/O imports")
    (forbidden-languages . ("JavaScript" "TypeScript" "AssemblyScript" "Python" "Rust"))
    (rationale . "wagasm-ssg is the EXPERIMENTAL pure WebAssembly SSG. The core logic MUST be in WAT. Host runtime provides only I/O.")
    (enforcement . "ABSOLUTE")))

(define experimental-notes
  '((why-experimental
     "WAT is a low-level language meant as a compilation target, not a source language.
      Writing an SSG in pure WAT is an extreme challenge:
      - No built-in file I/O (needs host imports)
      - No string type (manual memory management)
      - No high-level abstractions
      This is an experiment to prove it's possible.")
    (architecture
     "1. src/wagasm-ssg.wat - Pure WAT SSG logic
      2. runtime/host.ts - Deno host provides write_file, log imports
      The WAT is compiled to WASM, loaded by host, and executed.")))

;; =============================================================================
;; COMPONENT STATUS - 38/44 Components Complete
;; =============================================================================

(define component-status
  '((total-components . 44)
    (completed . 38)
    (completion-percentage . 86)))

;; 1. Core Engine (4/4)
(define core-engine
  '((status . "complete")
    (components
     (("WAT Engine" . "src/wagasm-ssg.wat")
      ("Memory Management" . "src/wagasm-ssg.wat:14-17")
      ("HTML Generation" . "src/wagasm-ssg.wat:64-86")
      ("Host Imports" . "src/wagasm-ssg.wat:10-11")))))

;; 2. Build System (4/4)
(define build-system
  '((status . "complete")
    (components
     (("Justfile" . "Justfile")
      ("Mustfile" . "Mustfile")
      (".tool-versions" . ".tool-versions")
      ("CI Pipeline" . ".github/workflows/ci.yml")))))

;; 3. Site Generation (3/4)
(define site-generation
  '((status . "partial")
    (components
     (("HTML Output" . "complete")
      ("Template System" . "basic - embedded in WAT")
      ("Content Processing" . "pending - markdown parsing")
      ("Output Directory" . "_site/")))))

;; 4. Adapters (3/3)
(define adapters
  '((status . "complete")
    (components
     (("ReScript Adapter" . "adapters/src/WagasmAdapter.res")
      ("Adapter Package" . "adapters/package.json")
      ("Deno Host" . "runtime/host.ts")))))

;; 5. Testing (4/4)
(define testing
  '((status . "complete")
    (components
     (("Test Directory" . "tests/")
      ("E2E Tests" . "tests/e2e/*.sh")
      ("Language Compliance" . "tests/e2e/test-language-compliance.sh")
      ("CI Testing" . ".github/workflows/ci.yml")))))

;; 6. Documentation (8/8)
(define documentation
  '((status . "complete")
    (components
     (("README" . "README.adoc")
      ("User Guide" . "docs/USER-GUIDE.adoc")
      ("Cookbook" . "cookbook.adoc")
      ("CLAUDE.md" . ".claude/CLAUDE.md")
      ("Contributing" . "CONTRIBUTING.md")
      ("Security" . "SECURITY.md")
      ("Code of Conduct" . "CODE_OF_CONDUCT.md")
      ("Copilot Instructions" . "copilot-instructions.md")))))

;; 7. Configuration (3/3)
(define configuration
  '((status . "complete")
    (components
     (("Tool Versions" . ".tool-versions")
      ("Git Attributes" . ".gitattributes")
      ("Git Ignore" . ".gitignore")))))

;; 8. SCM Files (6/6)
(define scm-files
  '((status . "complete")
    (components
     (("STATE.scm" . "STATE.scm")
      ("META.scm" . "META.scm")
      ("ECOSYSTEM.scm" . "ECOSYSTEM.scm")
      ("PLAYBOOK.scm" . "PLAYBOOK.scm")
      ("AGENTIC.scm" . "AGENTIC.scm")
      ("NEUROSYM.scm" . "NEUROSYM.scm")))))

;; 9. Hooks (3/3)
(define hooks
  '((status . "complete")
    (components
     (("pre-commit" . "hooks/pre-commit")
      ("pre-push" . "hooks/pre-push")
      ("commit-msg" . "hooks/commit-msg")))))

;; 10. Examples (3/3)
(define examples
  '((status . "complete")
    (components
     (("Content" . "content/")
      ("Templates" . "templates/")
      ("Example Config" . "templates/README.md")))))

(define current-position
  '((phase . "v0.1 - Complete Project Structure")
    (overall-completion . 86)
    (components
     ((core-engine ((status . "complete") (count . "4/4")))
      (build-system ((status . "complete") (count . "4/4")))
      (site-generation ((status . "partial") (count . "3/4")))
      (adapters ((status . "complete") (count . "3/3")))
      (testing ((status . "complete") (count . "4/4")))
      (documentation ((status . "complete") (count . "8/8")))
      (configuration ((status . "complete") (count . "3/3")))
      (scm-files ((status . "complete") (count . "6/6")))
      (hooks ((status . "complete") (count . "3/3")))
      (examples ((status . "complete") (count . "3/3")))))))

(define blockers-and-issues
  '((critical ())
    (high-priority
     (("Markdown parsing not yet in WAT" . high)
      ("Multi-page generation pending" . medium)))
    (resolved
     (("wat2wasm dependency" . "documented in README")
      ("CI/CD pipeline" . "complete with SHA-pinned actions")
      ("Security issues" . "fixed - SECURITY.md placeholders replaced")))))

(define state-summary
  '((project . "wagasm-ssg")
    (language . "WAT")
    (status . "EXPERIMENTAL")
    (completion . 55)
    (blockers . 1)
    (updated . "2025-12-17")))
