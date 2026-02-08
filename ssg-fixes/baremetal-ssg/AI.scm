;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; AI.scm — baremetal-ssg

(define-module (baremetal-ssg ai)
  #:export (project-context ai-guidelines language-coverage scope))

;;; Project context for AI assistants
(define project-context
  '((name . "baremetal-ssg")
    (type . "SSG adapter satellite")
    (hub . "poly-ssg-mcp")
    (description . "Bare-metal SSG adapters for 28 static site generators across 18 languages")
    (runtime . "Deno")
    (license . "AGPL-3.0-or-later")))

;;; Guidelines for AI-assisted development
(define ai-guidelines
  '((code-generation
     ((follow . "existing adapter patterns in adapters/")
      (require . "SPDX headers on all files")
      (format . "deno fmt for JavaScript")
      (test . "include unit tests for new code")))

    (documentation
     ((format . "AsciiDoc (.adoc) for docs")
      (exception . "GitHub-required files may use Markdown")
      (style . "British English spelling")
      (include . "examples for all public APIs")))

    (security
     ((never . "commit credentials or secrets")
      (always . "use environment variables for config")
      (scan . "run CodeQL on all changes")
      (report . "use GitHub Security Advisories")))

    (testing
     ((unit . "test all public functions")
      (integration . "test adapter connectivity")
      (e2e . "test full build pipelines")
      (coverage . "maintain >= 70%")))))

;;; Supported languages and their SSGs
(define language-coverage
  '((functional
     ((haskell . ("Hakyll" "Ema"))
      (ocaml . ("YOCaml"))
      (fsharp . ("Fornax"))
      (scala . ("Laika" "ScalaTex"))))
    (lisp-family
     ((clojure . ("Cryogen" "Perun"))
      (common-lisp . ("Coleslaw"))
      (racket . ("Frog" "Pollen"))
      (babashka . ("Babashka"))))
    (beam-vm
     ((elixir . ("Serum" "Tableau"))
      (erlang . ("Zotonic" "Wub"))))
    (systems
     ((rust . ("Zola" "Cobalt" "mdBook"))
      (nim . ("Nimrod" "Nimble-Publisher"))
      (d . ("Reggae"))
      (crystal . ("Marmot"))))
    (scientific
     ((julia . ("Franklin" "Documenter"))))
    (other
     ((swift . ("Publish"))
      (kotlin . ("Orchid"))
      (tcl . ("StaticWebPages"))))))

;;; What's in and out of scope
(define scope
  '((in-scope
     . ("All listed SSG adapters"
        "Deno-based tooling"
        "RSR compliance"
        "MCP protocol integration"
        "Container deployment"))
    (out-of-scope
     . ("Mainstream JS SSGs (Hugo, Jekyll, etc.)"
        "Python SSGs (Pelican, MkDocs, etc.)"
        "Ruby SSGs (Middleman, etc.)"
        "PHP SSGs"
        "GUI applications"))))
