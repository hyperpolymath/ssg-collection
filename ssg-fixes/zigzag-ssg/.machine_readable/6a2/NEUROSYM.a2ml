;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; NEUROSYM.scm — zigzag-ssg
;;; Neuro-Symbolic Patterns, Reasoning Systems, and Hybrid Intelligence

(define-module (zigzag-ssg neurosym)
  #:export (symbolic-patterns neural-integration reasoning-systems
            hybrid-architecture verification-proofs))

;; ============================================================================
;; SYMBOLIC PATTERNS
;; ============================================================================

(define symbolic-patterns
  '((language-grammar
     (description . "Formal grammar for content processing")
     (patterns
      ((frontmatter
        (rule . "--- NEWLINE (key: value NEWLINE)* ---")
        (terminals . ("---" ":" "NEWLINE"))
        (non-terminals . ("key" "value")))

       (markdown-block
        (rule . "(header | paragraph | list | code-fence)*")
        (terminals . ("#" "-" "*" "```" "NEWLINE"))
        (non-terminals . ("header" "paragraph" "list" "code-fence")))

       (template-substitution
        (rule . "{{ IDENTIFIER }}")
        (terminals . ("{{" "}}" "IDENTIFIER"))
        (semantics . "Replace with variable value from context")))))

    (type-system
     (description . "Type constraints for Zig implementation")
     (types
      ((Frontmatter
        (fields . ("title: []const u8"
                   "date: []const u8"
                   "draft: bool"
                   "template: []const u8")))

       (ParserState
        (fields . ("html: ArrayList(u8)"
                   "in_para: bool"
                   "in_code: bool"
                   "in_list: bool")))

       (ContentResult
        (fields . ("frontmatter: Frontmatter"
                   "body: []const u8"))))))

    (invariants
     (description . "Properties that must always hold")
     (properties
      ("All HTML output is properly escaped"
       "No unclosed tags in output"
       "Memory is freed before function returns"
       "Allocator errors are propagated"
       "Parser state is consistent after each line")))))

;; ============================================================================
;; NEURAL INTEGRATION
;; ============================================================================

(define neural-integration
  '((llm-assistance
     (description . "How LLMs integrate with zigzag-ssg development")
     (integration-points
      ((code-generation
        (input . "Natural language description")
        (output . "Zig code following project patterns")
        (constraints . "Must respect BOUNDARIES in AGENTIC.scm"))

       (documentation
        (input . "Code changes or API")
        (output . "AsciiDoc documentation")
        (constraints . "Must match existing style"))

       (review
        (input . "Diff or code snippet")
        (output . "Review comments and suggestions")
        (constraints . "Focus on Zig idioms and safety")))))

    (embeddings
     (description . "Semantic understanding of codebase")
     (indexed-content
      ("src/*.zig - Core implementation"
       "*.scm - Project configuration"
       "*.adoc - Documentation"
       "adapters/*.res - MCP adapter"))

     (similarity-search
      (use-cases . ("Find similar functions"
                    "Locate relevant documentation"
                    "Identify related tests"))))

    (aibdp-compliance
     (description . "AI Boundary Declaration Protocol")
     (manifest . ".well-known/aibdp.json")
     (policies
      ((training . "conditional - attribution required")
       (indexing . "allowed")
       (generation . "conditional - AGPL applies")
       (fine-tuning . "encouraged for SSG tools"))))))

;; ============================================================================
;; REASONING SYSTEMS
;; ============================================================================

(define reasoning-systems
  '((deductive-reasoning
     (description . "Deriving conclusions from rules")
     (rules
      ((language-rule
        (premise . "File is in src/")
        (conclusion . "File must be .zig"))

       (safety-rule
        (premise . "Function allocates memory")
        (conclusion . "Function must have defer cleanup or return owned slice"))

       (test-rule
        (premise . "Public function added")
        (conclusion . "Test must be added")))))

    (inductive-reasoning
     (description . "Learning patterns from examples")
     (patterns
      ((code-style
        (examples . "Existing functions in zigzag.zig")
        (learned . "4-space indent, explicit error handling, doc comments"))

       (commit-style
        (examples . "Recent git history")
        (learned . "Conventional commits, atomic changes")))))

    (abductive-reasoning
     (description . "Best explanation for observations")
     (scenarios
      ((test-failure
        (observation . "Test fails after change")
        (hypotheses . ("Change broke functionality"
                       "Test was fragile"
                       "Missing dependency")))

       (build-failure
        (observation . "Build fails in CI but passes locally")
        (hypotheses . ("Environment difference"
                       "Missing file in commit"
                       "Zig version mismatch"))))))))

;; ============================================================================
;; HYBRID ARCHITECTURE
;; ============================================================================

(define hybrid-architecture
  '((symbolic-layer
     (components
      ((parser . "Rule-based frontmatter/markdown parsing")
       (validator . "Schema validation for content")
       (type-checker . "Zig compile-time type checking")
       (formatter . "Deterministic code formatting")))
     (strengths . ("Predictable" "Verifiable" "Fast")))

    (neural-layer
     (components
      ((assistant . "Claude Code for development help")
       (reviewer . "AI-powered code review")
       (documenter . "Natural language documentation")))
     (strengths . ("Flexible" "Natural language" "Pattern recognition")))

    (integration
     (pattern . "Symbolic verifies Neural outputs")
     (workflow
      ("Neural generates code suggestion"
       "Symbolic validates against rules"
       "If valid, human reviews"
       "If invalid, Neural retries with feedback"))

     (verification-chain
      ("Neural → Type check → Test → Format → Commit")))))

;; ============================================================================
;; VERIFICATION PROOFS
;; ============================================================================

(define verification-proofs
  '((memory-safety
     (claim . "No memory leaks in normal operation")
     (proof-technique . "Allocator tracking")
     (verification
      ("All allocations paired with defer free"
       "toOwnedSlice transfers ownership clearly"
       "GeneralPurposeAllocator detects leaks in debug")))

    (html-safety
     (claim . "No XSS vulnerabilities in output")
     (proof-technique . "Escape analysis")
     (verification
      ("escapeHtml called on all user content"
       "< > & \" all replaced with entities"
       "No raw HTML passthrough")))

    (parser-correctness
     (claim . "Parser produces valid output for valid input")
     (proof-technique . "Property-based testing")
     (properties
      ("Frontmatter extraction preserves content"
       "Markdown to HTML is reversible (structurally)"
       "Empty input produces empty output")))

    (language-enforcement
     (claim . "Only Zig code exists in src/")
     (proof-technique . "CI enforcement")
     (verification
      ("find src/ with forbidden extensions"
       "Blocks merge if violations found"
       "Logged in CI artifacts")))))

;; ============================================================================
;; COGNITIVE ARCHITECTURE
;; ============================================================================

(define cognitive-architecture
  '((perception
     (inputs . ("Code changes" "Issues" "PRs" "Commands"))
     (processing . ("Parse structure" "Identify intent" "Extract context")))

    (reasoning
     (methods . ("Rule application" "Pattern matching" "Constraint solving"))
     (knowledge . ("ADRs" "Code patterns" "Best practices")))

    (action
     (outputs . ("Code" "Documentation" "Reviews" "Suggestions"))
     (verification . ("Type check" "Test" "Format" "Human review")))

    (learning
     (feedback . ("Review comments" "Test results" "User corrections"))
     (adaptation . ("Update patterns" "Refine suggestions" "Improve accuracy")))))

;; ============================================================================
;; SYMBOLIC-NEURAL BRIDGE
;; ============================================================================

(define symbolic-neural-bridge
  '((translation
     (neural-to-symbolic
      (description . "Convert LLM output to formal representation")
      (examples
       (("LLM: 'Add error handling'" .
         "Symbolic: (require error-union-return)")
        ("LLM: 'Make thread-safe'" .
         "Symbolic: (require mutex-protection)"))))

     (symbolic-to-neural
      (description . "Convert formal specs to natural language")
      (examples
       (("Symbolic: (type []const u8)" .
         "Neural: 'a slice of constant bytes (string)'")
        ("Symbolic: (error OutOfMemory)" .
         "Neural: 'may fail if memory allocation fails'")))))

    (grounding
     (description . "Ensure neural outputs respect symbolic constraints")
     (mechanisms
      ("Type system enforcement"
       "Test suite validation"
       "CI gate checks"
       "Human review")))))
