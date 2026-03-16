;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; PLAYBOOK.scm — hackenbush-ssg Operations Playbook

(define-module (hackenbush-ssg playbook)
  #:export (runbooks incident-response maintenance-procedures))

(define runbooks
  '((deploy
     (name . "Deploy hackenbush-ssg")
     (steps
      (1 . "Validate Life patterns: just validate-patterns")
      (2 . "Run simulator tests: just test")
      (3 . "Build host runtime: just build")
      (4 . "Generate site: deno task run")
      (5 . "Deploy to target: just deploy")))

    (pattern-development
     (name . "Develop New Life Pattern")
     (steps
      (1 . "Design pattern in Golly or similar tool")
      (2 . "Export to RLE format")
      (3 . "Place in patterns/ directory")
      (4 . "Document in patterns/README.adoc")
      (5 . "Add test cases: just test-patterns")
      (6 . "Submit PR with pattern + tests")))

    (simulator-update
     (name . "Update Host Simulator")
     (steps
      (1 . "Modify runtime/host.ts")
      (2 . "Ensure NO SSG logic added (CRITICAL)")
      (3 . "Run type check: deno check runtime/host.ts")
      (4 . "Run tests: just test")
      (5 . "Update documentation if API changed")))))

(define incident-response
  '((pattern-corruption
     (severity . "high")
     (symptoms . ("Invalid RLE output" "Simulation hangs" "Unexpected cell states"))
     (response
      (1 . "Validate RLE syntax with pattern linter")
      (2 . "Compare against known-good pattern backup")
      (3 . "Run in Golly to visualize")
      (4 . "Restore from git if needed")))

    (simulator-crash
     (severity . "medium")
     (symptoms . ("Deno process exit" "OOM errors" "Infinite loop"))
     (response
      (1 . "Check pattern size (may exceed memory)")
      (2 . "Review recent host.ts changes")
      (3 . "Run with --inspect for debugging")
      (4 . "Limit generations with --max-gen flag")))

    (security-incident
     (severity . "critical")
     (symptoms . ("Unauthorized access" "Secret exposure" "Dependency vulnerability"))
     (response
      (1 . "Revoke compromised credentials immediately")
      (2 . "Run security audit: just security")
      (3 . "Check dependabot alerts")
      (4 . "Report to security@hyperpolymath.org")))))

(define maintenance-procedures
  '((weekly
     ((task . "Review dependabot PRs")
      (command . "gh pr list --label dependencies"))
     ((task . "Check CI status")
      (command . "gh run list --limit 10"))
     ((task . "Update pattern documentation")
      (command . "just docs")))

    (monthly
     ((task . "Security audit")
      (command . "just security"))
     ((task . "Performance benchmark")
      (command . "just bench"))
     ((task . "Dependency major updates")
      (command . "just deps-update")))

    (quarterly
     ((task . "RSR compliance review")
      (command . "just validate-rsr"))
     ((task . "Pattern library expansion review")
      (command . "Review patterns/ for new additions"))
     ((task . "Documentation refresh")
      (command . "Review all .adoc files for accuracy")))))
