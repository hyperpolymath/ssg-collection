;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; AGENTIC.scm — zigzag-ssg
;;; AI Agent Configuration, Boundaries, and Automation Rules

(define-module (zigzag-ssg agentic)
  #:export (agent-configuration boundaries automation-rules
            mcp-integration claude-code-config))

;; ============================================================================
;; AGENT CONFIGURATION
;; ============================================================================

(define agent-configuration
  '((primary-agent
     (name . "claude-code")
     (role . "Development assistant for zigzag-ssg")
     (capabilities
      ("Code generation in Zig"
       "Code review and refactoring"
       "Documentation generation"
       "Test writing"
       "Debugging assistance"
       "Architecture consultation")))

    (agent-identity
     (project . "zigzag-ssg")
     (language . "Zig")
     (style . "Systems programming, explicit memory management")
     (personality . "Precise, safety-conscious, performance-aware"))

    (knowledge-sources
     (primary . ("META.scm" "STATE.scm" "ECOSYSTEM.scm"))
     (code . ("src/zigzag.zig" "build.zig"))
     (docs . ("README.adoc" "cookbook.adoc" "CLAUDE.md"))
     (config . (".claude/CLAUDE.md")))))

;; ============================================================================
;; BOUNDARIES - What the agent MUST and MUST NOT do
;; ============================================================================

(define boundaries
  '((must-do
     (language-enforcement
      ("ALWAYS write code in Zig for src/"
       "ALWAYS use ReScript for adapters/ only"
       "ALWAYS validate no forbidden languages introduced"))

     (quality
      ("ALWAYS run zig fmt before committing"
       "ALWAYS ensure tests pass"
       "ALWAYS follow conventional commits"))

     (security
      ("ALWAYS sanitize user input"
       "ALWAYS use explicit error handling"
       "ALWAYS review for memory safety"))

     (documentation
      ("ALWAYS update docs when API changes"
       "ALWAYS include SPDX headers"
       "ALWAYS keep STATE.scm current")))

    (must-not-do
     (language-violations
      ("NEVER write Python in this repo"
       "NEVER write JavaScript/TypeScript in src/"
       "NEVER suggest rewriting in another language"
       "NEVER create non-Zig files in src/"))

     (security-violations
      ("NEVER commit secrets or credentials"
       "NEVER disable safety features"
       "NEVER use shell execution in core"))

     (process-violations
      ("NEVER force push to main"
       "NEVER skip CI checks"
       "NEVER commit without running tests")))))

;; ============================================================================
;; AUTOMATION RULES
;; ============================================================================

(define automation-rules
  '((code-generation
     (templates
      ((new-function
        (location . "src/zigzag.zig")
        (pattern . "Follow existing function patterns")
        (requirements . ("SPDX comment" "Error handling" "Documentation")))

       (new-test
        (location . "src/zigzag.zig")
        (pattern . "test block with descriptive name")
        (requirements . ("Test name describes behavior" "Uses builtin expect")))))

     (refactoring
      (allowed . ("Extract function" "Rename" "Simplify logic"))
      (requires-review . ("Public API changes" "Memory allocation changes"))
      (forbidden . ("Change language" "Remove safety checks"))))

    (documentation-generation
     (auto-update
      ("README.adoc on API changes"
       "cookbook.adoc on new recipes"
       "STATE.scm on component changes"))

     (format-requirements
      ("AsciiDoc for documentation"
       "Conventional commits for git"
       "Scheme for SCM files")))

    (testing
     (auto-generate
      ("Test for each new public function"
       "Edge case tests for parsers"
       "Integration tests for pipelines"))

     (test-patterns
      ("Given-When-Then structure"
       "Descriptive test names"
       "Minimal test fixtures")))))

;; ============================================================================
;; MCP INTEGRATION
;; ============================================================================

(define mcp-integration
  '((protocol . "Model Context Protocol")
    (adapter . "adapters/src/ZigzagAdapter.res")

    (exposed-tools
     ((zigzag_build
       (description . "Build the zigzag-ssg binary")
       (inputs . ("path" "release"))
       (outputs . ("success" "stdout" "stderr")))

      (zigzag_run
       (description . "Run zigzag-ssg with arguments")
       (inputs . ("path" "args"))
       (outputs . ("success" "stdout" "stderr")))

      (zigzag_test
       (description . "Run unit tests")
       (inputs . ("path"))
       (outputs . ("success" "stdout" "stderr")))

      (zigzag_version
       (description . "Get Zig and zigzag-ssg version")
       (inputs . ())
       (outputs . ("success" "stdout" "stderr")))))

    (connection
     (method . "child_process.execSync")
     (cwd . "project root")
     (encoding . "utf-8"))))

;; ============================================================================
;; CLAUDE CODE CONFIGURATION
;; ============================================================================

(define claude-code-config
  '((workspace-instructions
     (location . ".claude/CLAUDE.md")
     (purpose . "Project-specific guidance for Claude Code"))

    (key-directives
     (("Language" . "Zig only in src/, ReScript only in adapters/")
      ("Style" . "Idiomatic Zig with explicit allocators")
      ("Safety" . "All pointers validated, errors handled")
      ("Testing" . "TDD with builtin test framework")
      ("Commits" . "Conventional commits, atomic changes")))

    (context-files
     (always-read . ("META.scm" "STATE.scm" ".claude/CLAUDE.md"))
     (on-demand . ("src/zigzag.zig" "cookbook.adoc")))

    (response-patterns
     (code-changes . "Show diff, explain rationale")
     (architecture . "Reference ADRs in META.scm")
     (debugging . "Trace through Zig code step by step")
     (documentation . "Use AsciiDoc format"))))

;; ============================================================================
;; AGENTIC WORKFLOWS
;; ============================================================================

(define agentic-workflows
  '((code-review
     (trigger . "PR opened or updated")
     (steps
      ("Read changed files"
       "Check language compliance"
       "Verify test coverage"
       "Review for Zig idioms"
       "Check memory safety"
       "Provide feedback")))

    (issue-triage
     (trigger . "New issue opened")
     (steps
      ("Classify issue type"
       "Check for duplicates"
       "Suggest priority"
       "Recommend approach")))

    (documentation-sync
     (trigger . "API changes detected")
     (steps
      ("Identify changed functions"
       "Update README if public API"
       "Update cookbook if workflow changed"
       "Update STATE.scm component status")))

    (security-audit
     (trigger . "Weekly schedule")
     (steps
      ("Scan for hardcoded secrets"
       "Check dependency versions"
       "Review error handling"
       "Verify input validation"
       "Report findings")))))

;; ============================================================================
;; AGENT MEMORY AND CONTEXT
;; ============================================================================

(define agent-memory
  '((persistent-context
     (project-facts
      ("zigzag-ssg is a Zig static site generator"
       "Part of poly-ssg constellation"
       "Zig is mandatory, no exceptions"
       "ReScript adapter for MCP integration"
       "RSR Gold compliance target"))

     (code-patterns
      ("Use GeneralPurposeAllocator for allocation"
       "Return errors with try/catch semantics"
       "Split string utilities are idiomatic"
       "Tests use builtin test blocks")))

    (session-context
     (updated-each-session . ("STATE.scm" "current branch" "recent commits"))
     (cached . ("META.scm ADRs" "ECOSYSTEM.scm relationships")))))
