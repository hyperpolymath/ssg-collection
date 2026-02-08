;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; META.scm — zigzag-ssg
;;; Architecture Decisions, Development Practices, and Design Rationale

(define-module (zigzag-ssg meta)
  #:export (architecture-decisions development-practices design-rationale
            language-rules component-registry security-posture))

;; ============================================================================
;; LANGUAGE RULES - Zig is MANDATORY
;; ============================================================================

(define language-rules
  '((mandatory-language . "Zig")
    (enforcement-level . "absolute")
    (rationale . "zigzag-ssg is THE DEFINITIVE Zig static site generator. It MUST be written entirely in Zig.")
    (violations
     ("Python implementation" . "FORBIDDEN")
     ("JavaScript/TypeScript" . "FORBIDDEN")
     ("Ruby/Go/Rust" . "FORBIDDEN")
     ("Any non-Zig language" . "FORBIDDEN - defeats the purpose of this satellite"))
    (exceptions
     (mcp-adapter . "ReScript in adapters/ directory ONLY")
     (build-tooling . "Just, Make, Nickel for task orchestration")
     (documentation . "AsciiDoc, Markdown"))
    (correct-implementation
     (engine . "src/*.zig")
     (build . "build.zig")
     (tests . "src/*.zig with test blocks")
     (adapter . "adapters/src/*.res"))))

;; ============================================================================
;; ARCHITECTURE DECISIONS (ADRs)
;; ============================================================================

(define architecture-decisions
  '((adr-001
     (title . "Zig-Only Implementation")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "SSG satellites must be in their target language to be definitive")
     (decision . "zigzag-ssg is written entirely in Zig with no other languages in core")
     (consequences . ("Language-specific optimizations"
                      "Comptime template validation"
                      "Zero hidden allocations"
                      "Cross-compilation support")))

    (adr-002
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "Part of hyperpolymath ecosystem requiring standards compliance")
     (decision . "Follow Rhodium Standard Repository guidelines")
     (consequences . ("RSR Gold target"
                      "SHA-pinned actions"
                      "SPDX headers"
                      "Well-known metadata")))

    (adr-003
     (title . "MCP Hub Integration via ReScript")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "poly-ssg satellites need hub connectivity")
     (decision . "ReScript adapter wraps Zig binary for MCP protocol")
     (consequences . ("Type-safe JS interop"
                      "Clean ES6 module output"
                      "Consistent with other satellites")))

    (adr-004
     (title . "Comptime Template Engine")
     (status . "proposed")
     (date . "2025-12-17")
     (context . "Leverage Zig's compile-time capabilities")
     (decision . "Template validation at compile time, not runtime")
     (consequences . ("Invalid templates don't compile"
                      "Zero runtime overhead"
                      "Compile errors for missing variables")))

    (adr-005
     (title . "Memory Safety via Explicit Allocation")
     (status . "accepted")
     (date . "2025-12-17")
     (context . "SSG processing large content needs predictable memory")
     (decision . "Use GeneralPurposeAllocator with explicit deallocation")
     (consequences . ("No hidden allocations"
                      "Predictable memory usage"
                      "Suitable for embedded/constrained environments")))

    (adr-006
     (title . "Task Orchestration via Just/Must")
     (status . "accepted")
     (date . "2025-12-17")
     (context . "Need cross-platform task running without shell scripts")
     (decision . "Use Justfile for development, Mustfile for must-pass tasks")
     (consequences . ("Portable recipes"
                      "Clear separation of optional vs mandatory"
                      "Integration with CI/CD")))))

;; ============================================================================
;; COMPONENT REGISTRY (44 Components Mapped to Zig)
;; ============================================================================

(define component-registry
  '((core-engine
     (count . 4)
     (components
      ((zig-parser . "src/zigzag.zig:frontmatter")
       (markdown-processor . "src/zigzag.zig:parseMarkdown")
       (template-engine . "src/zigzag.zig:applyTemplate")
       (html-generator . "src/zigzag.zig:output"))))

    (build-system
     (count . 4)
     (components
      ((justfile . "Justfile")
       (mustfile . "Mustfile")
       (build-zig . "build.zig")
       (containerfile . "Containerfile"))))

    (site-generation
     (count . 4)
     (components
      ((content-processing . "YAML frontmatter + Markdown")
       (template-substitution . "{{ variable }} patterns")
       (output-generation . "HTML files")
       (content-schema . "src/types.zig"))))

    (adapters
     (count . 3)
     (components
      ((mcp-server . "adapters/src/ZigzagAdapter.res")
       (rescript-bindings . "adapters/rescript.json")
       (deno-runtime . "adapters/package.json"))))

    (testing
     (count . 4)
     (components
      ((unit-tests . "src/zigzag.zig:test blocks")
       (e2e-tests . "tests/e2e/")
       (ci-pipeline . ".github/workflows/ci.yml")
       (coverage . "zig build test with coverage"))))

    (documentation
     (count . 8)
     (components
      ((readme . "README.adoc")
       (cookbook . "cookbook.adoc")
       (handover . "HANDOVER.adoc")
       (user-guide . "docs/USER-GUIDE.adoc")
       (module-readmes . "Each directory")
       (security-policy . "SECURITY.md")
       (contributing . "CONTRIBUTING.md")
       (code-of-conduct . "CODE_OF_CONDUCT.md"))))

    (configuration
     (count . 3)
     (components
      ((site-config . "zigzag.config.json")
       (env-handling . ".env.example")
       (tool-versions . ".tool-versions"))))

    (scm-files
     (count . 6)
     (components
      ((meta . "META.scm")
       (state . "STATE.scm")
       (ecosystem . "ECOSYSTEM.scm")
       (playbook . "PLAYBOOK.scm")
       (agentic . "AGENTIC.scm")
       (neurosym . "NEUROSYM.scm"))))

    (hooks-system
     (count . 4)
     (components
      ((pre-build . "hooks/pre-build.zig")
       (post-build . "hooks/post-build.zig")
       (watch-handler . "hooks/watch.zig")
       (deploy-hook . "hooks/deploy.zig"))))

    (examples
     (count . 4)
     (components
      ((example-content . "content/")
       (example-templates . "templates/")
       (example-config . "zigzag.config.json")
       (example-site . "examples/"))))))

;; ============================================================================
;; DEVELOPMENT PRACTICES
;; ============================================================================

(define development-practices
  '((code-style
     (languages . ("Zig"))
     (formatter . "zig fmt")
     (linter . "zig build with warnings")
     (line-length . 100)
     (indentation . "4 spaces"))

    (security
     (sast . "CodeQL for JS adapter, manual review for Zig")
     (credentials . "Environment variables only")
     (secrets-scanning . "GitHub secret scanning enabled")
     (dependency-review . "Dependabot weekly"))

    (testing
     (minimum-coverage . 70)
     (test-framework . "Zig builtin test")
     (e2e-framework . "Custom Zig test harness")
     (ci-required . #t))

    (versioning
     (scheme . "SemVer 2.0.0")
     (changelog . "CHANGELOG.md")
     (release-tags . "vX.Y.Z"))

    (commits
     (style . "Conventional Commits")
     (prefixes . ("feat" "fix" "docs" "style" "refactor" "test" "chore" "ci"))
     (gpg-signing . "recommended"))))

;; ============================================================================
;; SECURITY POSTURE
;; ============================================================================

(define security-posture
  '((threat-model
     (input-validation . "All user content sanitized")
     (path-traversal . "Strict path validation")
     (command-injection . "No shell execution in core")
     (memory-safety . "Zig's safety features enabled"))

    (supply-chain
     (sha-pinned-actions . #t)
     (dependency-audit . "Weekly via Dependabot")
     (provenance . ".well-known/provenance.json")
     (sbom . "Generated on release"))

    (disclosure
     (method . "GitHub Security Advisories")
     (timeline . "90-day coordinated disclosure")
     (safe-harbour . #t))))

;; ============================================================================
;; DESIGN RATIONALE
;; ============================================================================

(define design-rationale
  '((why-zig
     "Zig provides compile-time execution, explicit memory management, and zero hidden allocations - ideal for SSG workloads")
    (why-rsr
     "RSR ensures consistency, security, and maintainability across the hyperpolymath ecosystem")
    (why-mcp
     "MCP enables unified tooling across 28+ SSGs via the poly-ssg hub")
    (why-comptime
     "Compile-time template validation catches errors before runtime")
    (why-cross-compile
     "Zig's cross-compilation means the SSG runs anywhere from a single build")))
