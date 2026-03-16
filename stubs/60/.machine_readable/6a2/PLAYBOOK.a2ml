;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for 60-ssg

(define playbook
  `((version . "1.0.0")
    (procedures
      ((deploy
         (("step-1" . "just clean")
          ("step-2" . "just build")
          ("step-3" . "just test")
          ("step-4" . "just validate")
          ("step-5" . "Copy _site/ to deployment target")))
       (rollback
         (("step-1" . "Identify previous known-good commit")
          ("step-2" . "git checkout <commit-sha>")
          ("step-3" . "just clean && just build")
          ("step-4" . "Verify site generation succeeds")
          ("step-5" . "Redeploy from clean build")))
       (debug
         (("check-engine" . "Verify engine/src/sixty.alg exists and is valid ALGOL 60")
          ("check-output" . "Inspect _site/ directory for generated files")
          ("check-policy" . "just lint to verify language policy compliance")
          ("check-blocks" . "just fmt to verify begin/end block balance")
          ("verbose-build" . "Run build steps manually with output inspection")))))
    (alerts
      (("build-failure" . "Site generation failed - check ALGOL 60 syntax")
       ("policy-violation" . "Banned language detected in codebase")
       ("test-failure" . "Validation tests did not pass")))
    (contacts
      (("maintainer" . "See MAINTAINERS.adoc")
       ("security" . "See SECURITY.md")
       ("issues" . "https://github.com/hyperpolymath/60-ssg/issues")))))
