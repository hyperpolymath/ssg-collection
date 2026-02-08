;; SPDX-License-Identifier: MPL-2.0-or-later
;; META.scm - Meta-level information for anvil-ssg
;; Media-Type: application/meta+scheme

(meta
  (architecture-decisions
    (("adr-001" . "Use SPARK subset for formal verification")
     ("adr-002" . "Design by Contract for all interfaces")
     ("adr-003" . "Deterministic resource management")
     ("adr-004" . "Multi-architecture support (RISC-V, x86_64, ARM)")
     ("adr-005" . "Podman-first containerisation for builds")))

  (development-practices
    (code-style
      ("Ada 2022 standard"
       "SPARK annotations for verification"
       "Strong typing with no implicit conversions"
       "Contract-based preconditions and postconditions"))
    (security
      (principle "Defense in depth")
      (verification "GNATprove formal verification")
      (memory "No dynamic allocation in critical paths"))
    (testing
      ("GNATprove proofs as primary verification"
       "Unit tests for non-SPARK components"
       "Integration tests via Just recipes"))
    (versioning "SemVer")
    (documentation "AsciiDoc")
    (branching "main for stable"))

  (design-rationale
    (("spark-subset" . "Enables mathematical proof of absence of runtime errors")
     ("ada-tasking" . "Provides deterministic real-time build monitoring")
     ("contracts" . "Guarantees valid output if input satisfies preconditions")
     ("no-gc" . "Predictable memory footprint for embedded/ASIC targets"))))
