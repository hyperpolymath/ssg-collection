;; SPDX-License-Identifier: AGPL-3.0-or-later
;; wagasm-ssg - Pure WebAssembly Text (WAT) static site generator
;; EXPERIMENTAL: Pure WAT SSG with host runtime for I/O

(module
  ;; Memory: 1 page = 64KB for HTML content
  (memory (export "memory") 1)

  ;; Import host functions for I/O (provided by runtime)
  (import "host" "write_file" (func $write_file (param i32 i32 i32 i32)))
  (import "host" "log" (func $log (param i32 i32)))

  ;; Global state
  (global $html_ptr (mut i32) (i32.const 0))
  (global $title_ptr (mut i32) (i32.const 1024))
  (global $body_ptr (mut i32) (i32.const 2048))

  ;; HTML template strings stored in memory
  ;; Memory layout:
  ;;   0-1023: Generated HTML output
  ;;   1024-2047: Title buffer
  ;;   2048-4095: Body buffer
  ;;   4096+: Static strings

  ;; Data segments for HTML boilerplate
  (data (i32.const 4096) "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<title>")
  ;; Offset 4156
  (data (i32.const 4160) "</title>\n<style>body{font-family:Georgia,serif;max-width:800px;margin:2rem auto;padding:1rem}h1{color:#228B22}</style>\n</head>\n<body>\n")
  ;; Offset 4288
  (data (i32.const 4300) "</body>\n</html>\n")

  ;; Helper: Store byte at html_ptr and increment
  (func $emit (param $byte i32)
    (i32.store8 (global.get $html_ptr) (local.get $byte))
    (global.set $html_ptr (i32.add (global.get $html_ptr) (i32.const 1)))
  )

  ;; Helper: Copy n bytes from src to html output
  (func $copy_to_html (param $src i32) (param $len i32)
    (local $i i32)
    (local.set $i (i32.const 0))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
        (call $emit (i32.load8_u (i32.add (local.get $src) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  ;; Set page title (copy to title buffer)
  (func (export "set_title") (param $ptr i32) (param $len i32)
    (memory.copy (global.get $title_ptr) (local.get $ptr) (local.get $len))
    (i32.store8 (i32.add (global.get $title_ptr) (local.get $len)) (i32.const 0))
  )

  ;; Set page body content
  (func (export "set_body") (param $ptr i32) (param $len i32)
    (memory.copy (global.get $body_ptr) (local.get $ptr) (local.get $len))
    (i32.store8 (i32.add (global.get $body_ptr) (local.get $len)) (i32.const 0))
  )

  ;; Build the HTML page
  (func (export "build_page") (result i32)
    ;; Reset output pointer
    (global.set $html_ptr (i32.const 0))

    ;; DOCTYPE and head start (60 bytes)
    (call $copy_to_html (i32.const 4096) (i32.const 60))

    ;; Title (read from title buffer until null)
    (call $copy_string_from (global.get $title_ptr))

    ;; Head end and body start (128 bytes)
    (call $copy_to_html (i32.const 4160) (i32.const 128))

    ;; Body content
    (call $copy_string_from (global.get $body_ptr))

    ;; Close tags (16 bytes)
    (call $copy_to_html (i32.const 4300) (i32.const 16))

    ;; Return length of generated HTML
    (global.get $html_ptr)
  )

  ;; Helper: Copy null-terminated string to html output
  (func $copy_string_from (param $src i32)
    (local $byte i32)
    (block $done
      (loop $loop
        (local.set $byte (i32.load8_u (local.get $src)))
        (br_if $done (i32.eqz (local.get $byte)))
        (call $emit (local.get $byte))
        (local.set $src (i32.add (local.get $src) (i32.const 1)))
        (br $loop)
      )
    )
  )

  ;; Get output HTML pointer (always 0)
  (func (export "get_html_ptr") (result i32)
    (i32.const 0)
  )

  ;; Initialize SSG
  (func (export "init")
    ;; Set default title
    (i32.store (i32.const 1024) (i32.const 0x67615757)) ;; "Waga"
    (i32.store (i32.const 1028) (i32.const 0x53206D73)) ;; "sm S"
    (i32.store (i32.const 1032) (i32.const 0x00004753)) ;; "SG\0\0"

    ;; Set default body
    (i32.store (i32.const 2048) (i32.const 0x6C6C6548)) ;; "Hell"
    (i32.store (i32.const 2052) (i32.const 0x7266206F)) ;; "o fr"
    (i32.store (i32.const 2056) (i32.const 0x57206D6F)) ;; "om W"
    (i32.store (i32.const 2060) (i32.const 0x004D5341)) ;; "ASM\0"
  )

  ;; Build and write a page to file
  (func (export "generate_site")
    (local $len i32)

    ;; Build the HTML
    (local.set $len (call $build_page))

    ;; Write to file: filename at 8192, filename_len, html at 0, html_len
    ;; Store "index.html" at 8192
    (i32.store (i32.const 8192) (i32.const 0x65646E69)) ;; "inde"
    (i32.store (i32.const 8196) (i32.const 0x74682E78)) ;; "x.ht"
    (i32.store (i32.const 8200) (i32.const 0x00006C6D)) ;; "ml\0\0"

    ;; Call host to write file
    (call $write_file
      (i32.const 8192)  ;; filename ptr
      (i32.const 10)    ;; filename len "index.html"
      (i32.const 0)     ;; html ptr
      (local.get $len)  ;; html len
    )

    ;; Log completion
    (call $log (i32.const 8192) (i32.const 10))
  )
)
