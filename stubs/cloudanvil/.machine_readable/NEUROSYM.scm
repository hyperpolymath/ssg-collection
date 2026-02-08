;; SPDX-License-Identifier: MPL-2.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for anvil-ssg

(define neurosym-config
  `((version . "1.0.0")
    (symbolic-layer
      ((type . "scheme")
       (reasoning . "deductive")
       (verification . "formal")
       (contracts . "spark-annotations")))
    (neural-layer
      ((embeddings . false)
       (fine-tuning . false)
       (assistance . "code-generation")))
    (integration
      ((ai-assisted-proofs . "suggest proof strategies for GNATprove")
       (contract-generation . "infer preconditions/postconditions from usage")
       (documentation . "generate AsciiDoc from code comments")))))
