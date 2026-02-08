;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state for 60-ssg
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.1.0")
    (schema-version "1.0")
    (created "2026-01-03")
    (updated "2026-01-09")
    (project "60-ssg")
    (repo "github.com/hyperpolymath/60-ssg"))

  (project-context
    (name "60-ssg")
    (tagline "Block-structured static site generation in ALGOL 60")
    (tech-stack
      ("ALGOL 60" "Just" "Scheme" "POSIX Shell")))

  (current-position
    (phase "alpha")
    (overall-completion 25)
    (components
      (("engine" . "ALGOL 60 core generator")
       ("scripts" . "Policy enforcement")
       ("workflows" . "CI/CD automation")))
    (working-features
      ("Block-scoped variable handling")
      ("HTML escaping")
      ("Page rendering")
      ("Site building")))

  (route-to-mvp
    (milestones
      (("v0.1" . "Core engine implementation")
       ("v0.2" . "Template system")
       ("v0.3" . "Content processing")
       ("v1.0" . "Production-ready MVP"))))

  (blockers-and-issues
    (critical)
    (high)
    (medium
      ("ALGOL 60 interpreter availability"))
    (low
      ("Additional template features")))

  (critical-next-actions
    (immediate
      ("Complete engine validation tests")
      ("Document ALGOL 60 syntax requirements"))
    (this-week
      ("Add template inheritance support")
      ("Implement content directory scanning"))
    (this-month
      ("Build example site")
      ("Create user documentation")))

  (session-history
    (("2026-01-09" . "Resolved all stubs and TODOs"))))
