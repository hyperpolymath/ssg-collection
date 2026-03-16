;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — wagasm-ssg

(ecosystem
  (version "1.0.0")
  (name "wagasm-ssg")
  (type "satellite-ssg")
  (purpose "The EXPERIMENTAL pure WebAssembly Text (WAT) static site generator")
  (status "EXPERIMENTAL")
  (completion "86%")
  (components "38/44")
  (updated "2025-12-22")

  (language-identity
    (primary "WebAssembly Text (WAT)")
    (description "WAT is the human-readable text format for WebAssembly")
    (rationale "wagasm-ssg exists to PROVE that an SSG can be written in pure WAT")
    (host-runtime "Deno/ReScript provides ONLY I/O imports - no SSG logic")
    (forbidden ("JavaScript" "TypeScript" "AssemblyScript" "Python" "Rust"))
    (enforcement "ABSOLUTE - Core SSG logic MUST be in WAT. Host provides ONLY I/O."))

  (strict-rules
    (rule-1 "ALL SSG logic MUST be in src/wagasm-ssg.wat")
    (rule-2 "Host runtime MUST NOT contain SSG logic")
    (rule-3 "Host runtime provides ONLY: write_file, read_file, log")
    (rule-4 "No high-level language wrappers around the WAT")
    (rule-5 "AssemblyScript is FORBIDDEN - that defeats the purpose")
    (rule-6 "Any 'convenience' rewrites in JS/TS will be REJECTED"))

  (position-in-ecosystem
    "EXPERIMENTAL satellite SSG in the poly-ssg constellation.
     Demonstrates that even the most impractical target language can build websites.")

  (related-projects
    (project
      (name "poly-ssg-mcp")
      (url "https://github.com/hyperpolymath/poly-ssg-mcp")
      (relationship "hub")
      (description "Unified MCP server for 28+ SSGs"))
    (project
      (name "rhodium-standard-repositories")
      (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
      (relationship "standard")))

  (what-this-is
    "- EXPERIMENTAL pure WebAssembly Text SSG
     - Proves WAT can be used as a primary language
     - Demonstrates extreme low-level web development
     - Host runtime provides ONLY file I/O (not SSG logic)")

  (what-this-is-not
    "- NOT a TypeScript/JavaScript SSG compiled to WASM
     - NOT using AssemblyScript (that's TypeScript in disguise)
     - NOT a wrapper with 'real' logic in the host
     - NOT optional about being in WAT"))
