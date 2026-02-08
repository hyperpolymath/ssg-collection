;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; PLAYBOOK.scm — wagasm-ssg

(define-module (wagasm-ssg playbook)
  #:export (development-workflow build-procedures testing-strategy deployment-pipeline))

(define development-workflow
  '((language . "WebAssembly Text (WAT)")
    (status . "EXPERIMENTAL")

    (local-setup
     ((step-1 . "Install WABT (WebAssembly Binary Toolkit)")
      (step-2 . "Install Deno runtime")
      (step-3 . "Clone repository")
      (step-4 . "Run 'just compile' to build WASM")
      (step-5 . "Run 'just run' to generate site")))

    (development-cycle
     ((edit . "Modify src/wagasm-ssg.wat")
      (compile . "wat2wasm src/wagasm-ssg.wat -o src/wagasm-ssg.wasm")
      (test . "just test")
      (run . "deno run --allow-read --allow-write runtime/host.ts")
      (verify . "Check _site/index.html output")))

    (wat-development-tips
     ((memory-layout . "Document memory offsets carefully")
      (string-handling . "Use data segments for static strings")
      (debugging . "Use log() host import liberally")
      (stack-discipline . "Keep stack balanced in all code paths")))))

(define build-procedures
  '((compilation
     ((tool . "wat2wasm")
      (source . "src/wagasm-ssg.wat")
      (output . "src/wagasm-ssg.wasm")
      (flags . "--debug-names for development")))

    (validation
     ((tool . "wasm-validate")
      (purpose . "Verify WASM binary is well-formed")))

    (execution
     ((runtime . "Deno")
      (host . "runtime/host.ts")
      (permissions . "--allow-read --allow-write")
      (output-dir . "_site/")))

    (distribution
     ((artifact . "src/wagasm-ssg.wasm")
      (portability . "Runs on any WASM runtime with host imports")))))

(define testing-strategy
  '((unit-tests
     ((wat-validation . "Syntax and type checking via wat2wasm")
      (wasm-validation . "Binary validation via wasm-validate")
      (output-verification . "Check generated HTML structure")))

    (integration-tests
     ((host-integration . "Verify host imports work correctly")
      (file-output . "Verify files written to _site/")
      (content-generation . "Verify HTML content matches expectations")))

    (e2e-tests
     ((full-build . "Compile and run full site generation")
      (multi-page . "Generate multiple pages when supported")
      (template-variations . "Test different template configurations")))

    (language-compliance
     ((wat-only-in-src . "No TS/JS/RS/PY files in src/")
      (host-io-only . "Host runtime has no SSG logic")
      (no-assemblyscript . "Verify no AS transpilation")))))

(define deployment-pipeline
  '((ci-stages
     ((lint . "Verify language compliance")
      (compile . "wat2wasm compilation")
      (validate . "wasm-validate check")
      (test . "Run test suite")
      (build . "Generate example site")
      (security . "CodeQL analysis")))

    (release-process
     ((version-bump . "Update STATE.scm version")
      (changelog . "Document changes")
      (tag . "Create git tag")
      (artifact . "Publish WASM binary")))

    (security-requirements
     ((sha-pinned-actions . "All GitHub Actions use SHA pins")
      (checksum-verification . "Downloaded tools verified")
      (minimal-permissions . "Workflow uses read-all default")))))
