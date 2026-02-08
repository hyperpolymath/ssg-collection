;;; ECOSYSTEM.scm — befunge-ssg
;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define ecosystem-position
  '((project . "befunge-ssg")
    (category . "esoteric-language-tooling")
    (subcategory . "static-site-generator")
    (role . "Display and document Befunge programs as 2D grids")))

(define poly-ssg-integration
  '((hub . "poly-ssg-mcp")
    (satellite-type . "esoteric-language-ssg")
    (adapter-location . "rs/src/Adapter.res")
    (mcp-compatible . #t)))

(define related-projects
  '((siblings
     (("wagasm-ssg" . "WebAssembly Text SSG - similar structure")
      ("milk-ssg" . "COW esoteric language SSG")
      ("easel-ssg" . "Piet visual language SSG")))
    (parent
     (("poly-ssg" . "Polyglot SSG framework")
      ("poly-ssg-mcp" . "Unified MCP server for SSGs")))
    (inspiration
     (("befunge93-vault-cracker" . "Befunge-93 parody application")
      ("esolangs-wiki" . "Esoteric language documentation")))))

(define language-context
  '((befunge-93
     (created . 1993)
     (creator . "Chris Pressey")
     (paradigm . "2D stack-based")
     (playfield . "80x25 character grid")
     (execution . "Program counter moves in 4 directions")
     (notable . "First 2D programming language"))))

(define use-cases
  '((primary
     (("esolang-galleries" . "Host collections of Befunge programs")
      ("teaching" . "Show execution traces and stack states")
      ("code-archaeology" . "Preserve programs as static pages")))
    (secondary
     (("museum-mode" . "Offline-capable static archives")
      ("literate-befunge" . "Documentation embedded in programs")))))

(define ecosystem-summary
  '((project . "befunge-ssg")
    (hub . "poly-ssg-mcp")
    (language . "Befunge-93")
    (category . "esoteric-language-tooling")
    (updated . "2026-01-03")))
