;;; STATE.scm — befunge-ssg
;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define metadata
  '((version . "0.1.0")
    (updated . "2026-01-03")
    (project . "befunge-ssg")
    (required-language . "Befunge-93")
    (status . "EXPERIMENTAL")))

(define language-enforcement
  '((primary-language . "Befunge-93")
    (description . "2D stack-based esoteric language from 1993")
    (file-extension . ".bf")
    (interpreter . "Custom ReScript/JS interpreter")
    (host-runtime . "Deno with ReScript adapter for rendering")
    (forbidden-languages . ("TypeScript" "Python" "Go"))
    (rationale . "befunge-ssg is the Befunge-focused SSG. Programs are displayed as 2D grids with optional execution.")
    (enforcement . "STRICT")))

(define experimental-notes
  '((why-experimental
     "Befunge-93 is an esoteric language not designed for practical use.
      This SSG displays Befunge programs as:
      - 2D grids with stable coordinates
      - Optional execution output capture
      - Literate documentation extraction
      This is for galleries, teaching, and code preservation.")
    (architecture
     "1. rs/src/*.res - ReScript adapter for parsing/rendering
      2. src/*.mjs - Deno host for I/O and serving
      3. site/programs/*.bf - Befunge programs to display")))

;; =============================================================================
;; COMPONENT STATUS
;; =============================================================================

(define component-status
  '((total-components . 24)
    (completed . 18)
    (completion-percentage . 75)))

;; 1. Core Engine (3/4)
(define core-engine
  '((status . "partial")
    (components
     (("Grid Parser" . "rs/src/Adapter.res")
      ("HTML Renderer" . "rs/src/Render.res")
      ("Slug Generator" . "rs/src/Slug.res")
      ("Interpreter" . "pending - optional execution")))))

;; 2. Build System (4/4)
(define build-system
  '((status . "complete")
    (components
     (("Justfile" . "justfile")
      ("Mustfile" . "Mustfile")
      ("deno.json" . "deno.json")
      ("rescript.json" . "rs/rescript.json")))))

;; 3. Site Generation (3/4)
(define site-generation
  '((status . "partial")
    (components
     (("SSG Core" . "src/ssg.mjs")
      ("Dev Server" . "src/serve.mjs")
      ("Watch Mode" . "src/dev.mjs")
      ("Multi-page Index" . "pending")))))

;; 4. Documentation (4/4)
(define documentation
  '((status . "complete")
    (components
     (("README" . "README.adoc")
      ("Contributing" . "CONTRIBUTING.adoc")
      ("Security" . "SECURITY.md")
      ("Code of Conduct" . "CODE_OF_CONDUCT.md")))))

;; 5. SCM Files (4/6)
(define scm-files
  '((status . "partial")
    (components
     (("STATE.scm" . "STATE.scm")
      ("ECOSYSTEM.scm" . "ECOSYSTEM.scm")
      ("META.scm" . "pending")
      ("PLAYBOOK.scm" . "pending")
      ("AGENTIC.scm" . "pending")
      ("NEUROSYM.scm" . "pending")))))

(define current-position
  '((phase . "v0.1 - Core Structure Complete")
    (overall-completion . 75)
    (components
     ((core-engine ((status . "partial") (count . "3/4")))
      (build-system ((status . "complete") (count . "4/4")))
      (site-generation ((status . "partial") (count . "3/4")))
      (documentation ((status . "complete") (count . "4/4")))
      (scm-files ((status . "partial") (count . "4/6")))))))

(define blockers-and-issues
  '((critical ())
    (high-priority
     (("Befunge interpreter not yet implemented" . high)
      ("Multi-page gallery index pending" . medium)))
    (resolved
     (("Directory structure" . "reorganized per REPO-TREE.adoc")
      ("ReScript adapter" . "complete")))))

(define state-summary
  '((project . "befunge-ssg")
    (language . "Befunge-93")
    (status . "EXPERIMENTAL")
    (completion . 75)
    (blockers . 1)
    (updated . "2026-01-03")))
