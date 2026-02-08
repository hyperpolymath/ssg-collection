;;; STATE.scm — zigzag-ssg
;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; Project State Tracking - 44 Component Status

(define-module (zigzag-ssg state)
  #:export (metadata language-enforcement current-position
            component-status blockers-and-issues
            critical-next-actions roadmap session-history))

;; ============================================================================
;; METADATA
;; ============================================================================

(define metadata
  '((version . "1.0.0")
    (updated . "2025-12-17")
    (project . "zigzag-ssg")
    (required-language . "Zig")
    (total-components . 44)
    (components-complete . 20)
    (overall-completion . 45)))

;; ============================================================================
;; LANGUAGE ENFORCEMENT
;; ============================================================================

(define language-enforcement
  '((primary-language . "Zig")
    (file-extension . ".zig")
    (toolchain . "zig build")
    (minimum-version . "0.11.0")
    (forbidden-languages . ("Python" "JavaScript" "TypeScript" "Ruby" "Go" "Rust"))
    (rationale . "zigzag-ssg is the DEFINITIVE Zig static site generator")
    (enforcement . "strict - CI blocks forbidden languages")))

;; ============================================================================
;; COMPONENT STATUS (44 Total)
;; ============================================================================

(define component-status
  '(;; 1. Core Engine (4/4 Complete)
    (core-engine
     (total . 4) (complete . 4) (percentage . 100)
     (components
      ((zig-parser . #t)
       (markdown-processor . #t)
       (template-engine . #t)
       (html-generator . #t))))

    ;; 2. Build System (4/4 Complete)
    (build-system
     (total . 4) (complete . 4) (percentage . 100)
     (components
      ((justfile . #t)
       (mustfile . #t)
       (build-zig . #t)
       (containerfile . #t))))

    ;; 3. Site Generation (4/4 Complete)
    (site-generation
     (total . 4) (complete . 4) (percentage . 100)
     (components
      ((content-processing . #t)
       (template-substitution . #t)
       (output-generation . #t)
       (content-schema . #t))))

    ;; 4. Adapters (3/3 Complete)
    (adapters
     (total . 3) (complete . 3) (percentage . 100)
     (components
      ((mcp-server . #t)
       (rescript-bindings . #t)
       (deno-runtime . #t))))

    ;; 5. Testing (2/4 In Progress)
    (testing
     (total . 4) (complete . 2) (percentage . 50)
     (components
      ((unit-tests . #t)
       (e2e-tests . #f)
       (ci-pipeline . #t)
       (coverage . #f))))

    ;; 6. Documentation (4/8 In Progress)
    (documentation
     (total . 8) (complete . 4) (percentage . 50)
     (components
      ((readme . #t)
       (cookbook . #t)
       (handover . #f)
       (user-guide . #f)
       (module-readmes . #f)
       (security-policy . #t)
       (contributing . #t)
       (code-of-conduct . #t))))

    ;; 7. Configuration (2/3 In Progress)
    (configuration
     (total . 3) (complete . 2) (percentage . 66)
     (components
      ((site-config . #f)
       (env-handling . #t)
       (tool-versions . #t))))

    ;; 8. SCM Files (6/6 Complete)
    (scm-files
     (total . 6) (complete . 6) (percentage . 100)
     (components
      ((meta . #t)
       (state . #t)
       (ecosystem . #t)
       (playbook . #t)
       (agentic . #t)
       (neurosym . #t))))

    ;; 9. Hooks System (0/4 Pending)
    (hooks-system
     (total . 4) (complete . 0) (percentage . 0)
     (components
      ((pre-build . #f)
       (post-build . #f)
       (watch-handler . #f)
       (deploy-hook . #f))))

    ;; 10. Examples (0/4 Pending)
    (examples
     (total . 4) (complete . 0) (percentage . 0)
     (components
      ((example-content . #f)
       (example-templates . #f)
       (example-config . #f)
       (example-site . #f))))))

;; ============================================================================
;; CURRENT POSITION
;; ============================================================================

(define current-position
  '((phase . "v1.0 - Core Implementation Complete")
    (overall-completion . 100)
    (components ((Zig-engine ((status . "complete") (completion . 100)))
                 (mcp-adapter ((status . "complete") (language . "ReScript") (completion . 100)))
                 (build-system ((status . "complete") (completion . 100)))))))

(define blockers-and-issues
  '((critical ())
    (high-priority
     (("E2E test infrastructure" . "Need test content fixtures")
      ("Example site" . "Need representative content")))
    (medium-priority
     (("Watch mode" . "Requires inotify/FSEvents integration")
      ("Incremental builds" . "Need content hash tracking")))))

;; ============================================================================
;; CRITICAL NEXT ACTIONS
;; ============================================================================

(define critical-next-actions
  '((immediate (("Add comprehensive test suite" . medium)
                ("Implement watch mode" . medium)
                ("Add incremental builds" . low)))))

(define state-summary
  '((project . "zigzag-ssg")
    (language . "Zig")
    (total-components . 44)
    (components-complete . 20)
    (completion . "45%")
    (blockers . 0)
    (updated . "2025-12-17")))
