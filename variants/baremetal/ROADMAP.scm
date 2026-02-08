;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; ROADMAP.scm — baremetal-ssg

(define-module (baremetal-ssg roadmap)
  #:export (project-vision milestones priorities dependencies))

(define project-vision
  '((mission . "Provide bare-metal, high-performance SSG adapters for the hyperpolymath ecosystem")
    (scope . "28 adapters across 18 programming languages")
    (principles . ("Performance first" "Minimal abstraction" "RSR compliance" "Polyglot support"))))

;;; Completed and planned milestones
(define milestones
  '((v1.0-production
     ((status . "complete")
      (date . "2025-12-22")
      (deliverables
       . ("28 SSG adapters"
          "RSR Gold compliance"
          "Security policy"
          "Full documentation"
          "Justfile with 20+ commands"
          "Mustfile with recipes"
          "CI/CD pipeline"
          "Cookbook with CLI/Nickel/Just sections"))))

    (v1.1-optimization
     ((status . "planned")
      (target . "Q1 2025")
      (deliverables
       . ("Performance benchmarks for all adapters"
          "Caching layer for repeated builds"
          "Parallel build execution"
          "Memory usage optimization"))))

    (v1.2-ecosystem
     ((status . "planned")
      (target . "Q2 2025")
      (deliverables
       . ("Container registry publishing (ghcr.io)"
          "deno.land/x publishing"
          "Cross-satellite integration tests"
          "Compatibility matrix"))))

    (v2.0-advanced
     ((status . "planned")
      (target . "Q3 2025")
      (deliverables
       . ("Watch mode with incremental builds"
          "Multi-site orchestration"
          "Plugin system for custom adapters"
          "GUI companion app"))))))

;;; Current priorities
(define priorities
  '((high
     . ("Maintain adapter compatibility"
        "Security patch responsiveness"
        "Documentation accuracy"))
    (medium
     . ("Performance optimization"
        "Container image size reduction"
        "Additional language support"))
    (low
     . ("GUI development"
        "IDE plugins"
        "Cloud deployment integrations"))))

;;; Dependencies and blockers
(define dependencies
  '((external
     ((poly-ssg-mcp . "Hub coordination for adapter protocols")
      (rhodium-standard-repositories . "RSR compliance templates")
      (deno . "Runtime >= 1.40")))
    (internal
     ((adapters . "All 28 must pass tests before release")
      (documentation . "Must be complete for each adapter")
      (ci-cd . "Must pass all checks before merge")))))
