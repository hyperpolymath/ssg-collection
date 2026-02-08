;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; AGENTIC.scm — hackenbush-ssg AI Agent Configuration

(define-module (hackenbush-ssg agentic)
  #:export (agent-config capabilities constraints workflows))

(define agent-config
  '((project . "hackenbush-ssg")
    (type . "experimental-ssg")
    (language . "Conway's Game of Life")
    (agent-model . "claude-opus-4-5-20251101")
    (consent-framework . "consent-aware-http")))

(define capabilities
  '((pattern-analysis
     (description . "Analyze and validate Game of Life patterns")
     (tools . ("rle-parser" "pattern-validator" "golly-integration"))
     (permissions . ("read-patterns" "simulate" "report")))

    (simulator-development
     (description . "Develop and maintain host simulator")
     (tools . ("deno" "typescript" "test-framework"))
     (permissions . ("read-runtime" "write-runtime" "execute-tests"))
     (constraints . ("NO SSG logic in runtime" "Simulation + I/O only")))

    (adapter-development
     (description . "Develop MCP adapter in ReScript")
     (tools . ("rescript" "mcp-sdk"))
     (permissions . ("read-adapters" "write-adapters" "compile")))

    (documentation
     (description . "Generate and maintain documentation")
     (tools . ("asciidoctor" "markdown"))
     (permissions . ("read-all" "write-docs")))

    (ci-cd-management
     (description . "Manage CI/CD workflows")
     (tools . ("github-actions" "just" "deno"))
     (permissions . ("read-workflows" "write-workflows" "execute-ci")))))

(define constraints
  '((absolute
     ((rule . "Never add SSG logic to host runtime")
      (reason . "The Life pattern IS the program")
      (enforcement . "Code review + CI checks"))

     ((rule . "Never use JavaScript/TypeScript for SSG core")
      (reason . "Project proves Game of Life Turing-completeness")
      (enforcement . "rsr-antipattern.yml workflow"))

     ((rule . "All patterns must be valid RLE format")
      (reason . "Interoperability with Golly and other Life tools")
      (enforcement . "Pattern validation in CI")))

    (flexible
     ((rule . "Prefer smaller patterns when possible")
      (reason . "Performance and understandability")
      (enforcement . "Code review"))

     ((rule . "Document all pattern components")
      (reason . "Educational value of project")
      (enforcement . "Documentation check in CI")))))

(define workflows
  '((pattern-creation
     (trigger . "User requests new pattern component")
     (steps
      (1 . "Understand computation requirement")
      (2 . "Design pattern using Golly or similar")
      (3 . "Test pattern in isolation")
      (4 . "Export to RLE format")
      (5 . "Add to patterns/ with documentation")
      (6 . "Write tests for pattern behavior")
      (7 . "Create PR with description")))

    (bug-fix
     (trigger . "Issue reported or test failure")
     (steps
      (1 . "Reproduce issue locally")
      (2 . "Identify root cause")
      (3 . "Determine if pattern or simulator issue")
      (4 . "Apply fix with tests")
      (5 . "Verify all CI passes")
      (6 . "Create PR with fix description")))

    (security-fix
     (trigger . "Security vulnerability detected")
     (steps
      (1 . "Assess severity and impact")
      (2 . "Prepare fix without public disclosure")
      (3 . "Test fix thoroughly")
      (4 . "Merge with security advisory")
      (5 . "Notify users if critical")))))
