;; SPDX-License-Identifier: MPL-2.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for anvil-ssg
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "anvil-ssg")
  (type "static-site-generator")
  (purpose "High-integrity, formally-verified static site generation for safety-critical systems")

  (position-in-ecosystem
    (category "static-site-generators")
    (subcategory "safety-critical")
    (unique-value
      ("formal-verification" . "SPARK proofs eliminate runtime errors")
      ("design-by-contract" . "Mathematical guarantees on output validity")
      ("deterministic-resources" . "Predictable memory for embedded targets")
      ("multi-arch" . "Verified for RISC-V, x86_64, ARM")))

  (related-projects
    (("poly-ssg" . "Parent family of polyglot static site generators")
     ("gnat" . "Ada compiler toolchain")
     ("gnatprove" . "SPARK formal verification tool")))

  (what-this-is
    ("Formally-verified static site generator"
     "Safety-critical documentation tool"
     "DO-178C/ISO-26262 compliant generator"
     "High-integrity alternative to mainstream SSGs"))

  (what-this-is-not
    ("General-purpose SSG for casual websites"
     "JavaScript-based build tool"
     "CMS or content management system"
     "Dynamic server-side renderer")))
