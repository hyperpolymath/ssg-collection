;; SPDX-License-Identifier: MPL-2.0-or-later
;; STATE.scm - Project state for anvil-ssg
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.0.1")
    (schema-version "1.0")
    (created "2026-01-03")
    (updated "2026-01-09")
    (project "anvil-ssg")
    (repo "github.com/hyperpolymath/anvil-ssg"))

  (project-context
    (name "anvil-ssg")
    (tagline "High-integrity, safety-critical site generation in Ada/SPARK")
    (tech-stack
      ("Ada 2022" "SPARK" "Just" "Podman" "GNATprove")))

  (current-position
    (phase "initial")
    (overall-completion 5)
    (components
      (("core" . "planned")
       ("prover" . "planned")
       ("templates" . "planned")
       ("cli" . "planned")))
    (working-features
      ("repository-structure"
       "ci-workflows"
       "documentation-templates")))

  (route-to-mvp
    (milestones
      (("m1" . "Core SPARK verified types")
       ("m2" . "Template engine with contracts")
       ("m3" . "CLI interface")
       ("m4" . "Static site output"))))

  (blockers-and-issues
    (critical)
    (high)
    (medium
      ("ada-toolchain-setup" . "GNAT/SPARK toolchain configuration"))
    (low))

  (critical-next-actions
    (immediate
      ("define-core-types" . "Define Ada types for content and templates"))
    (this-week
      ("setup-gnatprove" . "Configure GNATprove for verification"))
    (this-month
      ("implement-template-engine" . "Build contract-verified template engine")))

  (session-history
    (("2026-01-09" . "Resolved all template placeholders and stubs"))))
