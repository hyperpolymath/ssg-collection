;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; ROADMAP.scm — chicxulub--ssg

(define roadmap
  '((version . "2.0.0")
    (last-updated . "2025-12-22")
    (project . "chicxulub--ssg")))

;; ============================================================================
;; PHASE 1: Foundation (v0.1) - COMPLETE
;; ============================================================================
(define phase-1
  '((name . "Foundation")
    (version . "v0.1")
    (status . "complete")
    (completion . 100)
    (milestones
      ((milestone "RSR Compliance Setup"
         (status . "complete")
         (items
           ("SHA-pinned GitHub Actions" . complete)
           ("SPDX licensing headers" . complete)
           ("Dependabot configuration" . complete)
           ("CodeQL security scanning" . complete)
           (".gitignore and .gitattributes" . complete)))
       (milestone "Initial Repository Structure"
         (status . "complete")
         (items
           ("META.scm architecture decisions" . complete)
           ("STATE.scm project state" . complete)
           ("ECOSYSTEM.scm ecosystem position" . complete)
           ("CONTRIBUTING.md guidelines" . complete)))))))

;; ============================================================================
;; PHASE 2: Security & Integration (v0.2) - COMPLETE
;; ============================================================================
(define phase-2
  '((name . "Security & Integration")
    (version . "v0.2")
    (status . "complete")
    (completion . 100)
    (milestones
      ((milestone "Security Policy"
         (status . "complete")
         (items
           ("SECURITY.md - vulnerability reporting" . complete)
           ("CODE_OF_CONDUCT.md - community standards" . complete)
           ("Security placeholders replaced" . complete)))
       (milestone "SSG Adapter Integration"
         (status . "complete")
         (items
           ("28 SSG adapters synced from poly-ssg-mcp" . complete)
           ("MCP protocol compatibility" . complete)
           ("Adapter README documentation" . complete)))
       (milestone "CI/CD Setup"
         (status . "complete")
         (items
           ("ci.yml comprehensive workflow" . complete)
           ("CodeQL workflow fixed" . complete)
           ("Adapter validation in CI" . complete)))))))

;; ============================================================================
;; PHASE 3: Full Infrastructure (v0.3) - COMPLETE
;; ============================================================================
(define phase-3
  '((name . "Full Infrastructure")
    (version . "v0.3")
    (status . "complete")
    (completion . 100)
    (milestones
      ((milestone "Build System"
         (status . "complete")
         (items
           ("justfile - build automation" . complete)
           ("Mustfile - quality gates" . complete)
           ("cookbook.adoc - command reference" . complete)))
       (milestone "SCM Files"
         (status . "complete")
         (items
           ("PLAYBOOK.scm - operational runbooks" . complete)
           ("AGENTIC.scm - AI agent patterns" . complete)
           ("NEUROSYM.scm - reasoning patterns" . complete)
           ("ROADMAP.scm - development roadmap" . complete)))
       (milestone "Git Hooks"
         (status . "complete")
         (items
           ("pre-commit hook" . complete)
           ("pre-push hook" . complete)
           ("hooks/config.sh configuration" . complete)
           ("hooks/install.sh installer" . complete)))
       (milestone "Documentation"
         (status . "complete")
         (items
           ("README.adoc - project overview" . complete)
           ("Architecture documentation" . complete)))))))

;; ============================================================================
;; PHASE 4: Testing & Quality (v0.4) - CURRENT
;; ============================================================================
(define phase-4
  '((name . "Testing & Quality")
    (version . "v0.4")
    (status . "in-progress")
    (completion . 0)
    (milestones
      ((milestone "Test Infrastructure"
         (status . "pending")
         (items
           ("tests/ directory structure" . pending)
           ("Unit tests for adapter interface" . pending)
           ("Integration tests for MCP protocol" . pending)
           ("70% code coverage target" . pending)))
       (milestone "Benchmarks"
         (status . "pending")
         (items
           ("benchmarks/ directory" . pending)
           ("Adapter load time benchmarks" . pending)
           ("Performance regression tests" . pending)))))))

;; ============================================================================
;; PHASE 5: Release & Publish (v1.0)
;; ============================================================================
(define phase-5
  '((name . "Release & Publish")
    (version . "v1.0")
    (status . "planned")
    (completion . 0)
    (milestones
      ((milestone "Release Preparation"
         (status . "planned")
         (items
           ("CHANGELOG.md" . planned)
           ("Version tagging" . planned)
           ("Release notes" . planned)
           ("Containerfile" . planned)))
       (milestone "Publishing"
         (status . "planned")
         (items
           ("deno.land/x package" . planned)
           ("GitHub release" . planned)
           ("Documentation site" . planned)))))))

;; ============================================================================
;; IMMEDIATE PRIORITIES (Next Steps)
;; ============================================================================
(define immediate-priorities
  '((priority . "high")
    (items
      (("Create tests/ directory" . "Set up test infrastructure")
       ("Write adapter interface tests" . "Validate all 28 adapters")
       ("Achieve 70% coverage" . "Meet RSR quality target")))))

;; ============================================================================
;; COMPONENT STATUS
;; ============================================================================
(define component-status
  '((complete
      ;; Core Infrastructure (4/4)
      ("justfile" "Mustfile" "hooks" "ci-cd")
      ;; SCM Files (7/7)
      ("META.scm" "STATE.scm" "ECOSYSTEM.scm" "PLAYBOOK.scm"
       "AGENTIC.scm" "NEUROSYM.scm" "ROADMAP.scm")
      ;; Adapters (28/28)
      ("babashka" "cobalt" "coleslaw" "cryogen" "documenter"
       "ema" "fornax" "franklin" "frog" "hakyll" "laika"
       "marmot" "mdbook" "nimble-publisher" "nimrod" "orchid"
       "perun" "pollen" "publish" "reggae" "scalatex" "serum"
       "staticwebpages" "tableau" "wub" "yocaml" "zola" "zotonic")
      ;; Documentation (5/5)
      ("README.adoc" "cookbook.adoc" "CONTRIBUTING.md"
       "SECURITY.md" "CODE_OF_CONDUCT.md")
      ;; Security (4/4)
      ("CodeQL" "Dependabot" "SPDX headers" "secrets scanning"))
    (pending
      ;; Testing (0/4)
      ("unit tests" "e2e tests" "coverage" "benchmarks"))))

;; ============================================================================
;; SUMMARY
;; ============================================================================
(define roadmap-summary
  '((phases
      ((phase-1 . "Foundation - COMPLETE (100%)")
       (phase-2 . "Security & Integration - COMPLETE (100%)")
       (phase-3 . "Full Infrastructure - COMPLETE (100%)")
       (phase-4 . "Testing & Quality - IN PROGRESS (0%)")
       (phase-5 . "Release & Publish - PLANNED (0%)")))
    (overall-progress . 85)
    (components-complete . 48)
    (components-pending . 4)
    (target-v1-release . "Q1 2025")))
