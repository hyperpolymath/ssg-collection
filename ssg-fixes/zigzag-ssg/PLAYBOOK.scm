;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; PLAYBOOK.scm — zigzag-ssg
;;; Development Workflows, Recipes, and Operational Procedures

(define-module (zigzag-ssg playbook)
  #:export (workflows recipes ci-cd-pipeline
            development-cycle release-process))

;; ============================================================================
;; DEVELOPMENT WORKFLOWS
;; ============================================================================

(define workflows
  '((daily-development
     (description . "Standard development workflow")
     (steps
      ("1. Pull latest changes: git pull origin main"
       "2. Create feature branch: git checkout -b feat/description"
       "3. Run tests: just test"
       "4. Make changes in src/*.zig"
       "5. Format code: just fmt"
       "6. Run full check: must check"
       "7. Commit with conventional commits"
       "8. Push and create PR")))

    (feature-development
     (description . "Adding new functionality")
     (steps
      ("1. Review ADRs in META.scm for architectural guidance"
       "2. Write failing tests first (TDD)"
       "3. Implement in src/zigzag.zig"
       "4. Update documentation"
       "5. Run must check before PR")))

    (bug-fixing
     (description . "Fixing issues")
     (steps
      ("1. Reproduce issue with test case"
       "2. Write failing test that demonstrates bug"
       "3. Fix the code"
       "4. Verify test passes"
       "5. Check for regressions: just test")))

    (release-workflow
     (description . "Publishing a new version")
     (steps
      ("1. Ensure all tests pass: must check"
       "2. Update CHANGELOG.md"
       "3. Bump version in STATE.scm"
       "4. Create release commit: git commit -m 'chore: release vX.Y.Z'"
       "5. Tag release: git tag vX.Y.Z"
       "6. Push with tags: git push origin main --tags"
       "7. GitHub release with notes")))))

;; ============================================================================
;; JUSTFILE RECIPES
;; ============================================================================

(define justfile-recipes
  '((build
     (command . "zig build")
     (description . "Build the zigzag-ssg binary"))

    (test
     (command . "zig build test")
     (description . "Run all unit tests"))

    (test-e2e
     (command . "zig build run -- test-full")
     (description . "Run end-to-end tests"))

    (run
     (command . "zig build run")
     (description . "Build and run zigzag-ssg"))

    (fmt
     (command . "zig fmt src/")
     (description . "Format all Zig source files"))

    (clean
     (command . "rm -rf zig-cache zig-out")
     (description . "Remove build artifacts"))

    (release
     (command . "zig build -Doptimize=ReleaseFast")
     (description . "Build optimized release binary"))

    (adapter-build
     (command . "cd adapters && npm run build")
     (description . "Build the ReScript MCP adapter"))

    (adapter-clean
     (command . "cd adapters && npm run clean")
     (description . "Clean adapter build artifacts"))

    (docs
     (command . "asciidoctor README.adoc cookbook.adoc")
     (description . "Generate HTML documentation"))

    (check
     (command . "just fmt && just test && just build")
     (description . "Run all quality checks"))

    (container
     (command . "podman build -t zigzag-ssg .")
     (description . "Build container image"))

    (ci
     (command . "just check && just test-e2e")
     (description . "Full CI pipeline locally"))))

;; ============================================================================
;; MUSTFILE RECIPES (Must-Pass Gates)
;; ============================================================================

(define mustfile-recipes
  '((check
     (description . "All quality gates must pass")
     (commands
      ("zig fmt --check src/"
       "zig build"
       "zig build test"
       "! find src/ -name '*.py' -o -name '*.js' -o -name '*.ts' | grep .")))

    (security
     (description . "Security checks must pass")
     (commands
      ("! grep -r 'TODO.*security' src/"
       "! grep -r 'password.*=' src/"
       "! grep -r 'secret.*=' src/")))

    (release
     (description . "Pre-release gates")
     (commands
      ("must check"
       "must security"
       "test -f CHANGELOG.md"
       "grep -q 'version' STATE.scm")))))

;; ============================================================================
;; CI/CD PIPELINE
;; ============================================================================

(define ci-cd-pipeline
  '((triggers
     (push . ("main"))
     (pull-request . ("main"))
     (schedule . "weekly security scan"))

    (jobs
     ((build
       (runs-on . "ubuntu-latest")
       (steps
        ("Checkout repository"
         "Setup Zig 0.11.0"
         "Run zig build"
         "Run zig build test")))

      (language-check
       (runs-on . "ubuntu-latest")
       (steps
        ("Checkout repository"
         "Verify no forbidden languages in src/")))

      (security
       (runs-on . "ubuntu-latest")
       (condition . "schedule or adapters/ changed")
       (steps
        ("Checkout repository"
         "Initialize CodeQL"
         "Run CodeQL analysis")))))

    (quality-gates
     (required . ("build" "language-check"))
     (optional . ("security")))))

;; ============================================================================
;; DEVELOPMENT CYCLE
;; ============================================================================

(define development-cycle
  '((phases
     ((explore
       (description . "Understand requirements and existing code")
       (activities . ("Read ADRs" "Review src/zigzag.zig" "Check STATE.scm")))

      (plan
       (description . "Design the solution")
       (activities . ("Update META.scm if new ADR needed" "Write test cases")))

      (implement
       (description . "Write the code")
       (activities . ("Write Zig code" "Keep tests passing")))

      (verify
       (description . "Ensure quality")
       (activities . ("Run just check" "Run must check" "Review changes")))

      (document
       (description . "Update documentation")
       (activities . ("Update README if API changed" "Update cookbook")))

      (integrate
       (description . "Merge and deploy")
       (activities . ("Create PR" "Address review comments" "Merge")))))))

;; ============================================================================
;; RELEASE PROCESS
;; ============================================================================

(define release-process
  '((pre-release
     (checklist
      ("All tests passing"
       "No critical issues in STATE.scm blockers"
       "CHANGELOG.md updated"
       "Documentation current"
       "Version bumped")))

    (release
     (steps
      ("Create release branch from main"
       "Final must check"
       "Tag with vX.Y.Z"
       "Push tag to origin"
       "Create GitHub release"
       "Attach binaries if applicable")))

    (post-release
     (steps
      ("Update STATE.scm with release info"
       "Announce in discussions if major"
       "Close related milestones"
       "Plan next iteration")))))

;; ============================================================================
;; HOOKS INTEGRATION
;; ============================================================================

(define hooks-integration
  '((pre-commit
     (triggers . "git commit")
     (actions . ("just fmt" "zig build")))

    (pre-push
     (triggers . "git push")
     (actions . ("must check")))

    (post-checkout
     (triggers . "git checkout")
     (actions . ("zig build")))

    (ci-hooks
     (pre-build . "Validate content schema")
     (post-build . "Deploy to staging")
     (on-success . "Notify success")
     (on-failure . "Notify failure and open issue"))))
