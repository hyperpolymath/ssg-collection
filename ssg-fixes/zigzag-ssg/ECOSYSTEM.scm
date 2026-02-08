;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; ECOSYSTEM.scm — zigzag-ssg
;;; Ecosystem Positioning and Integration Points

(define-module (zigzag-ssg ecosystem)
  #:export (ecosystem-definition related-projects integration-points))

;; ============================================================================
;; ECOSYSTEM DEFINITION
;; ============================================================================

(define ecosystem-definition
  '((version . "1.0.0")
    (name . "zigzag-ssg")
    (type . "satellite-ssg")
    (purpose . "The DEFINITIVE Zig static site generator")

    (language-identity
     (primary . "Zig")
     (version-minimum . "0.11.0")
     (rationale . "zigzag-ssg exists to be THE Zig SSG. The entire engine is written in Zig.")
     (forbidden . ("Python" "JavaScript" "TypeScript" "Ruby" "Go" "Rust"))
     (enforcement . "Absolute - non-Zig implementations rejected"))

    (position-in-ecosystem
     "Satellite SSG in the poly-ssg constellation. Each satellite is the definitive SSG for its language.
      zigzag-ssg occupies the Zig position, providing:
      - Compile-time template validation
      - Zero hidden allocations
      - Cross-compilation support
      - Memory-safe content processing")

    (what-this-is
     ("The DEFINITIVE static site generator written in Zig"
      "Part of the poly-ssg satellite constellation"
      "RSR-compliant repository structure"
      "MCP-integrated for unified tooling"))

    (what-this-is-not
     ("NOT a template reimplemented in other languages"
      "NOT optional about being in Zig"
      "NOT a wrapper around another SSG"
      "NOT a port of Hugo/Jekyll/etc"))))

;; ============================================================================
;; RELATED PROJECTS
;; ============================================================================

(define related-projects
  '((project
     (name . "poly-ssg-mcp")
     (url . "https://github.com/hyperpolymath/poly-ssg-mcp")
     (relationship . "hub")
     (description . "Unified MCP server for 28+ SSGs")
     (integration . "ReScript adapter in adapters/"))

    (project
     (name . "rhodium-standard-repositories")
     (url . "https://github.com/hyperpolymath/rhodium-standard-repositories")
     (relationship . "standard")
     (description . "Repository quality standards")
     (compliance . "RSR Gold target"))

    (project
     (name . "noteg-ssg")
     (url . "https://github.com/hyperpolymath/noteg-ssg")
     (relationship . "sibling-satellite")
     (description . "Ada/SPARK SSG - Note G implementation")
     (differentiation . "Ada for safety-critical, Zig for systems programming"))

    (project
     (name . "pyroclast-ssg")
     (url . "https://github.com/hyperpolymath/pyroclast-ssg")
     (relationship . "sibling-satellite")
     (description . "Elixir SSG")
     (differentiation . "Elixir for concurrent/distributed, Zig for low-level control"))

    (project
     (name . "flotsam-ssg")
     (url . "https://github.com/hyperpolymath/flotsam-ssg")
     (relationship . "sibling-satellite")
     (description . "Julia SSG")
     (differentiation . "Julia for scientific computing, Zig for embedded/systems"))))

;; ============================================================================
;; INTEGRATION POINTS
;; ============================================================================

(define integration-points
  '((mcp-protocol
     (adapter . "adapters/src/ZigzagAdapter.res")
     (tools . ("zigzag_build" "zigzag_run" "zigzag_test" "zigzag_version"))
     (connection . "child_process execution of Zig binary"))

    (ci-cd
     (provider . "GitHub Actions")
     (workflows . ("ci.yml" "codeql.yml"))
     (quality-gates . ("zig build" "zig build test" "language check")))

    (package-registries
     (zig . "zig-pkg (future)")
     (npm . "adapters/package.json"))

    (documentation
     (format . "AsciiDoc")
     (readme . "README.adoc")
     (api . "Generated from source"))

    (security
     (advisories . "GitHub Security Advisories")
     (scanning . "CodeQL for adapter, Dependabot for deps")
     (provenance . ".well-known/provenance.json"))))

;; ============================================================================
;; CONSTELLATION MAP
;; ============================================================================

(define constellation-map
  '((tier-1-systems
     ("zigzag-ssg" . "Zig - systems programming")
     ("noteg-ssg" . "Ada/SPARK - safety-critical")
     ("ferric-ssg" . "Rust - memory-safe systems"))

    (tier-2-functional
     ("pyroclast-ssg" . "Elixir - distributed systems")
     ("flotsam-ssg" . "Julia - scientific computing")
     ("argon-ssg" . "Haskell - pure functional"))

    (tier-3-dynamic
     ("cascade-ssg" . "TypeScript - web ecosystem")
     ("surge-ssg" . "Python - ML/data science")
     ("tempest-ssg" . "Ruby - rapid development"))))
