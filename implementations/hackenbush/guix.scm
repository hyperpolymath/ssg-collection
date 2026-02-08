;; hackenbush-ssg - Guix Package Definition
;; Run: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages base))

(define-public hackenbush-ssg
  (package
    (name "hackenbush-ssg")
    (version "0.1.0")
    (source (local-file "." "hackenbush-ssg-checkout"
                        #:recursive? #t
                        #:select? (git-predicate ".")))
    (build-system gnu-build-system)
    (synopsis "Game of Life Static Site Generator")
    (description "Static site generator implemented in Conway's Game of Life patterns - proving Turing-completeness through cellular automata. Part of the poly-ssg satellite constellation.")
    (home-page "https://github.com/hyperpolymath/hackenbush-ssg")
    (license license:agpl3+)))

;; Return package for guix shell
hackenbush-ssg
