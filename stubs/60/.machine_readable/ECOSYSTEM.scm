;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for 60-ssg
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "60-ssg")
  (type "static-site-generator")
  (purpose "Block-structured site generation using ALGOL 60 for historical authenticity and lexical scope rigour")

  (position-in-ecosystem
    (category "web-tooling")
    (subcategory "static-site-generators")
    (unique-value
      ("Historical language implementation - ALGOL 60 as the 'Latin' of programming")
      ("Strict lexical scoping prevents global state corruption")
      ("Educational value for computer science history")
      ("Part of the poly-ssg family of generators")))

  (related-projects
    (("poly-ssg" . "Parent family of polyglot static site generators")
     ("palimpsest-license" . "Hyperpolymath licensing framework")
     ("marst" . "ALGOL 60 to C translator")
     ("a60" . "ALGOL 60 interpreter")))

  (what-this-is
    ("A static site generator written in ALGOL 60")
    ("An educational tool for understanding lexical scope origins")
    ("A demonstration of block-structured programming principles")
    ("Part of the hyperpolymath ecosystem"))

  (what-this-is-not
    ("A production-grade high-performance SSG")
    ("A replacement for modern static site generators")
    ("A general-purpose programming environment")
    ("Backwards-compatible with any other SSG")))
