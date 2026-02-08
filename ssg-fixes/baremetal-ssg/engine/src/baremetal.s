# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
#
# baremetal-ssg: RISC-V assembly static site generator
# Direct hardware-level site generation with zero abstraction overhead
#
# Target: RISC-V 64-bit (RV64I base instruction set)
# Calling convention: RISC-V standard (a0-a7 args, a0-a1 return)

.section .rodata
    .align 3

# HTML template strings
html_doctype:
    .asciz "<!DOCTYPE html>\n"
html_html_open:
    .asciz "<html lang=\"en\">\n"
html_head_open:
    .asciz "<head>\n    <meta charset=\"UTF-8\">\n    <title>"
html_head_close:
    .asciz "</title>\n</head>\n"
html_body_open:
    .asciz "<body>\n    <article>\n        <h1>"
html_body_middle:
    .asciz "</h1>\n        "
html_body_close:
    .asciz "\n    </article>\n</body>\n</html>\n"

# Status messages
msg_building:
    .asciz "baremetal-ssg: Forging site at hardware level...\n"
msg_page_built:
    .asciz "baremetal-ssg: Built page: "
msg_newline:
    .asciz "\n"
msg_complete:
    .asciz "baremetal-ssg: Build complete. Zero abstraction overhead.\n"

# Output directory
output_dir:
    .asciz "_site"

.section .data
    .align 3

# Page buffer (4KB)
page_buffer:
    .space 4096

# Path buffer (256 bytes)
path_buffer:
    .space 256

.section .text
    .align 2
    .global _start
    .global build_site
    .global render_page
    .global write_file
    .global escape_html

# -----------------------------------------------------------------------------
# _start: Entry point
# -----------------------------------------------------------------------------
_start:
    # Set up stack
    la      sp, stack_top

    # Print building message
    la      a0, msg_building
    call    print_string

    # Create output directory (syscall: mkdir)
    la      a0, output_dir
    li      a1, 0755            # permissions: rwxr-xr-x
    li      a7, 83              # sys_mkdir
    ecall

    # Build example page
    la      a0, example_title
    la      a1, example_path
    la      a2, example_content
    call    build_page

    # Print complete message
    la      a0, msg_complete
    call    print_string

    # Exit
    li      a0, 0
    li      a7, 93              # sys_exit
    ecall

# -----------------------------------------------------------------------------
# build_page: Build a single HTML page
# Arguments:
#   a0 = pointer to title string
#   a1 = pointer to output path (without extension)
#   a2 = pointer to content string
# Returns: none
# -----------------------------------------------------------------------------
build_page:
    # Save registers
    addi    sp, sp, -48
    sd      ra, 40(sp)
    sd      s0, 32(sp)
    sd      s1, 24(sp)
    sd      s2, 16(sp)
    sd      s3, 8(sp)

    mv      s0, a0              # s0 = title
    mv      s1, a1              # s1 = path
    mv      s2, a2              # s2 = content

    # Build path: _site/{path}.html
    la      a0, path_buffer
    la      a1, output_dir
    call    strcpy
    la      a0, path_buffer
    call    strlen
    la      a1, path_buffer
    add     a0, a1, a0
    li      a1, '/'
    sb      a1, 0(a0)
    addi    a0, a0, 1
    mv      a1, s1
    call    strcpy
    la      a0, path_buffer
    call    strlen
    la      a1, path_buffer
    add     a0, a1, a0
    la      a1, html_ext
    call    strcpy

    # Build HTML content
    la      s3, page_buffer     # s3 = buffer pointer

    # <!DOCTYPE html>
    mv      a0, s3
    la      a1, html_doctype
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # <html>
    mv      a0, s3
    la      a1, html_html_open
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # <head>...<title>
    mv      a0, s3
    la      a1, html_head_open
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # title (escaped)
    mv      a0, s3
    mv      a1, s0
    call    escape_html_copy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # </title></head>
    mv      a0, s3
    la      a1, html_head_close
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # <body><article><h1>
    mv      a0, s3
    la      a1, html_body_open
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # title again (escaped)
    mv      a0, s3
    mv      a1, s0
    call    escape_html_copy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # </h1>
    mv      a0, s3
    la      a1, html_body_middle
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # content
    mv      a0, s3
    mv      a1, s2
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # </article></body></html>
    mv      a0, s3
    la      a1, html_body_close
    call    strcpy
    mv      a0, s3
    call    strlen
    add     s3, s3, a0

    # Calculate total length
    la      a0, page_buffer
    sub     s3, s3, a0          # s3 = total length

    # Write file
    la      a0, path_buffer
    la      a1, page_buffer
    mv      a2, s3
    call    write_file

    # Print status
    la      a0, msg_page_built
    call    print_string
    la      a0, path_buffer
    call    print_string
    la      a0, msg_newline
    call    print_string

    # Restore registers
    ld      ra, 40(sp)
    ld      s0, 32(sp)
    ld      s1, 24(sp)
    ld      s2, 16(sp)
    ld      s3, 8(sp)
    addi    sp, sp, 48
    ret

# -----------------------------------------------------------------------------
# escape_html_copy: Copy string with HTML escaping
# Arguments:
#   a0 = destination buffer
#   a1 = source string
# Returns: none
# -----------------------------------------------------------------------------
escape_html_copy:
    addi    sp, sp, -16
    sd      ra, 8(sp)

escape_loop:
    lbu     t0, 0(a1)
    beqz    t0, escape_done

    # Check for special characters
    li      t1, '<'
    beq     t0, t1, escape_lt
    li      t1, '>'
    beq     t0, t1, escape_gt
    li      t1, '&'
    beq     t0, t1, escape_amp
    li      t1, '"'
    beq     t0, t1, escape_quot

    # Normal character
    sb      t0, 0(a0)
    addi    a0, a0, 1
    addi    a1, a1, 1
    j       escape_loop

escape_lt:
    # &lt;
    li      t0, '&'
    sb      t0, 0(a0)
    li      t0, 'l'
    sb      t0, 1(a0)
    li      t0, 't'
    sb      t0, 2(a0)
    li      t0, ';'
    sb      t0, 3(a0)
    addi    a0, a0, 4
    addi    a1, a1, 1
    j       escape_loop

escape_gt:
    # &gt;
    li      t0, '&'
    sb      t0, 0(a0)
    li      t0, 'g'
    sb      t0, 1(a0)
    li      t0, 't'
    sb      t0, 2(a0)
    li      t0, ';'
    sb      t0, 3(a0)
    addi    a0, a0, 4
    addi    a1, a1, 1
    j       escape_loop

escape_amp:
    # &amp;
    li      t0, '&'
    sb      t0, 0(a0)
    li      t0, 'a'
    sb      t0, 1(a0)
    li      t0, 'm'
    sb      t0, 2(a0)
    li      t0, 'p'
    sb      t0, 3(a0)
    li      t0, ';'
    sb      t0, 4(a0)
    addi    a0, a0, 5
    addi    a1, a1, 1
    j       escape_loop

escape_quot:
    # &quot;
    li      t0, '&'
    sb      t0, 0(a0)
    li      t0, 'q'
    sb      t0, 1(a0)
    li      t0, 'u'
    sb      t0, 2(a0)
    li      t0, 'o'
    sb      t0, 3(a0)
    li      t0, 't'
    sb      t0, 4(a0)
    li      t0, ';'
    sb      t0, 5(a0)
    addi    a0, a0, 6
    addi    a1, a1, 1
    j       escape_loop

escape_done:
    sb      zero, 0(a0)         # null terminate
    ld      ra, 8(sp)
    addi    sp, sp, 16
    ret

# -----------------------------------------------------------------------------
# write_file: Write buffer to file
# Arguments:
#   a0 = path string
#   a1 = buffer pointer
#   a2 = length
# Returns: none
# -----------------------------------------------------------------------------
write_file:
    addi    sp, sp, -32
    sd      ra, 24(sp)
    sd      s0, 16(sp)
    sd      s1, 8(sp)

    mv      s0, a1              # buffer
    mv      s1, a2              # length

    # Open file (O_WRONLY | O_CREAT | O_TRUNC = 0x241)
    li      a1, 0x241
    li      a2, 0644            # permissions
    li      a7, 56              # sys_openat
    li      a0, -100            # AT_FDCWD
    ecall

    mv      t0, a0              # file descriptor

    # Write content
    mv      a0, t0
    mv      a1, s0
    mv      a2, s1
    li      a7, 64              # sys_write
    ecall

    # Close file
    mv      a0, t0
    li      a7, 57              # sys_close
    ecall

    ld      ra, 24(sp)
    ld      s0, 16(sp)
    ld      s1, 8(sp)
    addi    sp, sp, 32
    ret

# -----------------------------------------------------------------------------
# print_string: Print null-terminated string to stdout
# Arguments:
#   a0 = string pointer
# Returns: none
# -----------------------------------------------------------------------------
print_string:
    addi    sp, sp, -16
    sd      ra, 8(sp)
    sd      s0, 0(sp)

    mv      s0, a0
    call    strlen
    mv      a2, a0              # length

    li      a0, 1               # stdout
    mv      a1, s0              # buffer
    li      a7, 64              # sys_write
    ecall

    ld      ra, 8(sp)
    ld      s0, 0(sp)
    addi    sp, sp, 16
    ret

# -----------------------------------------------------------------------------
# strlen: Calculate string length
# Arguments:
#   a0 = string pointer
# Returns:
#   a0 = length
# -----------------------------------------------------------------------------
strlen:
    mv      t0, a0
strlen_loop:
    lbu     t1, 0(t0)
    beqz    t1, strlen_done
    addi    t0, t0, 1
    j       strlen_loop
strlen_done:
    sub     a0, t0, a0
    ret

# -----------------------------------------------------------------------------
# strcpy: Copy string
# Arguments:
#   a0 = destination
#   a1 = source
# Returns: none
# -----------------------------------------------------------------------------
strcpy:
strcpy_loop:
    lbu     t0, 0(a1)
    sb      t0, 0(a0)
    beqz    t0, strcpy_done
    addi    a0, a0, 1
    addi    a1, a1, 1
    j       strcpy_loop
strcpy_done:
    ret

# -----------------------------------------------------------------------------
# Data section for example page
# -----------------------------------------------------------------------------
.section .rodata
html_ext:
    .asciz ".html"
example_title:
    .asciz "Welcome to baremetal-ssg"
example_path:
    .asciz "index"
example_content:
    .asciz "<p>Static site generation at the hardware level.</p>"

# -----------------------------------------------------------------------------
# Stack
# -----------------------------------------------------------------------------
.section .bss
    .align 4
stack_bottom:
    .space 8192
stack_top:
