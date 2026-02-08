---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my wagasm-ssg site.

## Code Example

Here's a simple example in WebAssembly Text format:

```wat
(module
  ;; Import the fd_write function from WASI for console output
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))

  ;; Memory for storing our string
  (memory (export "memory") 1)

  ;; Store "Hello, World!\n" at memory offset 8
  (data (i32.const 8) "Hello, World!\n")

  ;; iov structure: pointer to string, length of string
  (data (i32.const 0) "\08\00\00\00")  ;; pointer = 8
  (data (i32.const 4) "\0e\00\00\00")  ;; length = 14

  ;; Main function to print the greeting
  (func (export "_start")
    ;; fd_write(stdout=1, iovs=0, iovs_len=1, nwritten=24)
    (call $fd_write
      (i32.const 1)   ;; file descriptor (stdout)
      (i32.const 0)   ;; iovs pointer
      (i32.const 1)   ;; iovs length
      (i32.const 24)) ;; where to store bytes written
    drop
  )
)
```

Happy building!
