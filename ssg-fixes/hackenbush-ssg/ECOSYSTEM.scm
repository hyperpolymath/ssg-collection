;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — hackenbush-ssg

(ecosystem
  (version "0.1.0")
  (name "hackenbush-ssg")
  (type "satellite-ssg")
  (purpose "The DEFINITIVE Conway's Game of Life static site generator")

  (language-identity
    (primary "Conway's Game of Life")
    (rationale "hackenbush-ssg exists to PROVE that SSG logic can be encoded in Game of Life patterns. Conway's Game of Life is Turing-complete, meaning any computation can theoretically be performed by a Life pattern.")
    (forbidden ("Python" "JavaScript" "TypeScript" "Ruby" "Go"))
    (allowed-for-host ("TypeScript/Deno" "ReScript"))
    (enforcement "The Life pattern IS the program. The host runtime provides ONLY simulation and I/O."))

  (position-in-ecosystem
    "Satellite SSG in the poly-ssg constellation. Each satellite is the definitive SSG for its language. hackenbush-ssg proves cellular automata can be a programming paradigm.")

  (related-projects
    (project
      (name "poly-ssg-mcp")
      (url "https://github.com/hyperpolymath/poly-ssg-mcp")
      (relationship "hub")
      (description "Unified MCP server for 28+ SSGs - provides adapter interface"))
    (project
      (name "rhodium-standard-repositories")
      (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
      (relationship "standard")))

  (what-this-is
    "- The DEFINITIVE static site generator using Conway's Game of Life patterns
     - Proof that Turing-complete cellular automata can encode SSG logic
     - Part of the poly-ssg satellite constellation
     - An extreme experimental project")

  (what-this-is-not
    "- NOT a JavaScript/Python SSG with Life visualization
     - NOT a template that can be reimplemented in conventional languages
     - NOT practical for production use (yet)
     - The host runtime is NOT the SSG - the Life patterns ARE"))
