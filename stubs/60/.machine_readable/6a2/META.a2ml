;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Meta-level information for 60-ssg
;; Media-Type: application/meta+scheme

(meta
  (architecture-decisions
    (("ADR-001" . "Use ALGOL 60 as core language for historical authenticity")
     ("ADR-002" . "Block-scoped variables prevent global state pollution")
     ("ADR-003" . "Just as task runner for cross-platform compatibility")
     ("ADR-004" . "Scheme for machine-readable configuration files")))

  (development-practices
    (code-style
      (algol60
        (blocks "begin/end pairs must be balanced")
        (comments "Use 'comment' keyword for documentation")
        (procedures "Named procedures with explicit parameter types"))
      (scheme
        (format "2-space indentation")
        (naming "kebab-case for identifiers")))
    (security
      (principle "Defense in depth")
      (spdx-headers "Required on all source files")
      (sha-pinning "Dependencies must be SHA-pinned")
      (scorecard "OpenSSF Scorecard compliance"))
    (testing
      (unit "Validate engine procedures individually")
      (integration "Test full site generation pipeline")
      (policy "Automated language policy enforcement"))
    (versioning "SemVer")
    (documentation "AsciiDoc")
    (branching "main for stable"))

  (design-rationale
    (("historical-focus" . "ALGOL 60 chosen as the 'Latin' of programming languages")
     ("block-structure" . "Lexical scoping pioneered by ALGOL 60 prevents variable pollution")
     ("minimal-deps" . "Core engine has zero external dependencies")
     ("portability" . "POSIX-compliant tooling for maximum compatibility"))))
