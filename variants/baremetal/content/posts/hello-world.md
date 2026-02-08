---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my baremetal-ssg site.

## Code Example

Here's a simple example in x86-64 Assembly:

```asm
section .data
    msg db "Hello, World!", 10
    len equ $ - msg

section .text
    global _start

_start:
    mov rax, 1          ; sys_write
    mov rdi, 1          ; stdout
    mov rsi, msg        ; message address
    mov rdx, len        ; message length
    syscall

    mov rax, 60         ; sys_exit
    xor rdi, rdi        ; exit code 0
    syscall
```

Happy building!
