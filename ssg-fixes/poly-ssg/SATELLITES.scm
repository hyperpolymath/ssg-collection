;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; SATELLITES.scm — poly-ssg satellite registry

(define-module (poly-ssg satellites)
  #:export (satellite-registry satellite-languages satellite-status))

;; Registry of all SSG satellites connected to the poly-ssg-mcp hub
;; Each satellite is THE definitive SSG for its language
(define satellite-registry
  '((implemented
     ;; These satellites have complete implementations in their native language
     ((name . "casket-ssg")
      (language . "Haskell")
      (repo . "https://github.com/hyperpolymath/casket-ssg")
      (adapter . "adapters/src/CasketAdapter.res")
      (description . "Pure functional SSG with strong type guarantees")
      (status . "active"))

     ((name . "ddraig-ssg")
      (language . "Idris 2")
      (repo . "https://github.com/hyperpolymath/ddraig-ssg")
      (adapter . "adapters/src/DdraigAdapter.res")
      (description . "Dependently-typed SSG with compile-time correctness")
      (status . "active"))

     ((name . "estate-ssg")
      (language . "Forth")
      (repo . "https://github.com/hyperpolymath/estate-ssg")
      (adapter . "adapters/src/EstateAdapter.res")
      (description . "Stack-based SSG with minimal dependencies")
      (status . "active"))

     ((name . "parallax-ssg")
      (language . "Chapel")
      (repo . "https://github.com/hyperpolymath/parallax-ssg")
      (adapter . "adapters/src/ParallaxAdapter.res")
      (description . "Parallel-first SSG for high-performance builds")
      (status . "active"))

     ((name . "prodigy-ssg")
      (language . "Prolog")
      (repo . "https://github.com/hyperpolymath/prodigy-ssg")
      (adapter . "adapters/src/ProdigyAdapter.res")
      (description . "Logic-based SSG with declarative site definitions")
      (status . "active"))

     ((name . "rescribe-ssg")
      (language . "ReScript")
      (repo . "https://github.com/hyperpolymath/rescribe-ssg")
      (adapter . "adapters/src/RescribeAdapter.res")
      (description . "Type-safe SSG compiling to JavaScript")
      (status . "active"))

     ((name . "zigzag-ssg")
      (language . "Zig")
      (repo . "https://github.com/hyperpolymath/zigzag-ssg")
      (adapter . "adapters/src/ZigzagAdapter.res")
      (description . "High-performance SSG with zero-overhead abstractions")
      (status . "active"))

     ((name . "hackenbush-ssg")
      (language . "Conway's Game of Life")
      (repo . "https://github.com/hyperpolymath/hackenbush-ssg")
      (adapter . "adapters/src/HackenbushAdapter.res")
      (description . "Turing-complete cellular automaton SSG")
      (status . "needs-rewrite")
      (note . "Needs pure Game of Life implementation"))

     ((name . "milk-ssg")
      (language . "COW")
      (repo . "https://github.com/hyperpolymath/milk-ssg")
      (adapter . "adapters/src/MilkAdapter.res")
      (description . "Esoteric SSG proving universality")
      (status . "active"))

     ((name . "terrapin-ssg")
      (language . "Logo")
      (repo . "https://github.com/hyperpolymath/terrapin-ssg")
      (adapter . "adapters/src/TerrapinAdapter.res")
      (description . "Educational SSG with turtle graphics")
      (status . "active"))

     ((name . "wagasm-ssg")
      (language . "WebAssembly Text (WAT)")
      (repo . "https://github.com/hyperpolymath/wagasm-ssg")
      (adapter . "adapters/src/WagasmAdapter.res")
      (description . "EXPERIMENTAL: Pure WAT proving source viability")
      (status . "experimental")))

    (todo
     ;; These satellites need implementation
     ;; Listed in SSG-LANGUAGE-MAP.md
     "anvil-ssg"      ; Anvil (visual Python)
     "baremetal-ssg"  ; Bare Metal (C/Assembly)
     "chicxulub-ssg"  ; Meteor impact theme
     "divisionone-ssg"; D1
     "doit-ssg"       ; DoIt
     "eclipse-ssg"    ; Eclipse IDE
     "gungir-ssg"     ; Odin
     "iota-ssg"       ; Iota
     "labnote-ssg"    ; Lab notebook
     "macrauchenia-ssg" ; Extinct mammals theme
     "my-ssg"         ; Customizable
     "noteg-ssg"      ; Note G (Ada Lovelace)
     "obli-ssg"       ; Oblique strategies
     "odd-ssg"        ; Odd numbers
     "orbital-ssg"    ; Orbital mechanics
     "pharos-ssg"     ; Lighthouse
     "qed-ssg"        ; Proof assistant
     "rats-ssg"       ; RATS
     "saur-ssg"       ; Dinosaur
     "shift-ssg"      ; Shift
     "sparkle-ssg"    ; Sparkle
     "yocaml-ssg")))  ; YOCaml (OCaml)

;; Language to satellite mapping
(define satellite-languages
  '(("Haskell" . "casket-ssg")
    ("Idris 2" . "ddraig-ssg")
    ("Forth" . "estate-ssg")
    ("Chapel" . "parallax-ssg")
    ("Prolog" . "prodigy-ssg")
    ("ReScript" . "rescribe-ssg")
    ("Zig" . "zigzag-ssg")
    ("Conway's Game of Life" . "hackenbush-ssg")
    ("COW" . "milk-ssg")
    ("Logo" . "terrapin-ssg")
    ("WebAssembly Text" . "wagasm-ssg")))

;; Status counts
(define satellite-status
  '((total . 33)
    (implemented . 11)
    (todo . 22)
    (experimental . 1)))  ; wagasm-ssg
