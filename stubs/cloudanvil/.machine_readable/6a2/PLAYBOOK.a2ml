;; SPDX-License-Identifier: MPL-2.0-or-later
;; PLAYBOOK.scm - Operational runbook for anvil-ssg

(define playbook
  `((version . "1.0.0")
    (procedures
      ((deploy . (("build" . "just build")
                  ("test" . "just test")
                  ("prove" . "just prove")
                  ("release" . "just release")))
       (rollback . (("revert-last" . "git revert HEAD")
                    ("reset-to-tag" . "git checkout <tag>")
                    ("rebuild-clean" . "just clean && just build")))
       (debug . (("run-gnatprove" . "gnatprove -P project.gpr --level=2")
                 ("check-contracts" . "gnatprove -P project.gpr --mode=check")
                 ("verbose-build" . "just build --verbose")
                 ("inspect-ali" . "View .ali files for dependency info")))))
    (alerts
      ((proof-failure . "GNATprove failed to prove a contract")
       (build-error . "Compilation failed")
       (contract-violation . "Runtime contract check failed")))
    (contacts
      ((maintainer . "@hyperpolymath")
       (issues . "github.com/hyperpolymath/anvil-ssg/issues")))))
