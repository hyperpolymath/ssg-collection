;;; STATE.scm — hackenbush-ssg
;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define metadata
  '((version . "0.1.0")
    (updated . "2025-12-17")
    (project . "hackenbush-ssg")
    (required-language . "Conway's Game of Life")))

(define language-enforcement
  '((primary-language . "Conway's Game of Life")
    (file-extension . ".rle")
    (interpreter . "Deno (host runtime)")
    (forbidden-languages . ("Python" "JavaScript" "TypeScript" "Ruby" "Go"))
    (rationale . "hackenbush-ssg is the DEFINITIVE Game of Life static site generator. The SSG logic MUST be encoded in Life patterns. The host runtime (Deno/TypeScript) provides ONLY simulation and I/O - no SSG logic.")
    (enforcement . "strict")))

(define current-position
  '((phase . "v0.1.0 - Game of Life Implementation (Experimental)")
    (overall-completion . 30)
    (components ((life-patterns ((status . "initial") (completion . 20)))
                 (host-runtime ((status . "complete") (language . "TypeScript/Deno") (completion . 100)))
                 (mcp-adapter ((status . "skeleton") (language . "ReScript") (completion . 10)))))))

(define blockers-and-issues
  '((critical ())
    (high-priority (("Design larger Life patterns for HTML generation" . research)
                    ("Implement input encoding system" . design)))))

(define critical-next-actions
  '((immediate (("Design pattern components for basic HTML tags" . high)
                ("Create pattern library documentation" . medium)
                ("Complete MCP adapter integration" . high)))))

(define state-summary
  '((project . "hackenbush-ssg")
    (language . "Conway's Game of Life")
    (completion . 30)
    (blockers . 0)
    (status . "experimental")
    (updated . "2025-12-17")))
