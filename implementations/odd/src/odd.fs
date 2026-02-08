\ SPDX-License-Identifier: AGPL-3.0-or-later
\ SPDX-FileCopyrightText: 2025 hyperpolymath
\
\ odd-ssg - Forth Static Site Generator
\ "Let stacks push. Let words define. Let sites compile."
\
\ Run with: gforth src/odd.fs -e "build bye"
\           gforth src/odd.fs -e "init bye"
\           gforth src/odd.fs -e "clean bye"

\ ============================================================================
\ Configuration
\ ============================================================================

: content-dir s" content" ;
: output-dir s" _site" ;
: site-title s" Odd Site" ;

\ ============================================================================
\ String Utilities
\ ============================================================================

: string-append ( addr1 u1 addr2 u2 -- addr3 u3 )
  \ Append two strings, result in PAD
  >r >r
  pad swap move
  r> r> pad + swap move
  pad over + ;

: starts-with? ( addr1 u1 addr2 u2 -- flag )
  \ Check if string1 starts with string2
  2dup 2>r
  min
  2r> drop
  compare 0= ;

: trim-left ( addr u -- addr' u' )
  \ Remove leading whitespace
  begin
    dup 0> while
    over c@ bl <= while
    1 /string
  repeat then ;

: trim-right ( addr u -- addr u' )
  \ Remove trailing whitespace
  begin
    dup 0> while
    2dup + 1- c@ bl <= while
    1-
  repeat then ;

: trim ( addr u -- addr' u' )
  trim-left trim-right ;

\ ============================================================================
\ File I/O Helpers
\ ============================================================================

variable file-handle
create line-buffer 1024 allot
variable line-length

: read-line-safe ( -- addr u flag )
  \ Read a line, return string and success flag
  line-buffer 1024 file-handle @ read-line throw
  if line-buffer swap true
  else drop 0 0 false then ;

: write-string ( addr u fid -- )
  write-file throw ;

: write-line ( addr u fid -- )
  2dup write-string
  s\" \n" rot write-string ;

\ ============================================================================
\ HTML Escaping
\ ============================================================================

create escaped-buffer 4096 allot
variable escaped-pos

: escape-reset 0 escaped-pos ! ;

: escape-char ( c -- )
  case
    [char] & of s" &amp;" endof
    [char] < of s" &lt;" endof
    [char] > of s" &gt;" endof
    [char] " of s" &quot;" endof
    dup 1 >r escaped-buffer escaped-pos @ + c! 1 escaped-pos +! r>
  endcase
  dup if
    escaped-buffer escaped-pos @ + swap move
    escaped-pos +!
  else 2drop then ;

: escape-html ( addr u -- addr' u' )
  escape-reset
  0 do
    dup i + c@ escape-char
  loop drop
  escaped-buffer escaped-pos @ ;

\ ============================================================================
\ Frontmatter Parser
\ ============================================================================

create page-title 256 allot
variable page-title-len
create page-date 32 allot
variable page-date-len
variable page-draft

: init-metadata
  0 page-title-len !
  0 page-date-len !
  false page-draft ! ;

: parse-frontmatter-line ( addr u -- )
  \ Parse "key: value" format
  2dup s" title:" starts-with? if
    6 /string trim
    page-title swap dup page-title-len ! move
    exit
  then
  2dup s" date:" starts-with? if
    5 /string trim
    page-date swap dup page-date-len ! move
    exit
  then
  2dup s" draft:" starts-with? if
    6 /string trim
    s" true" compare 0= page-draft !
    exit
  then
  2drop ;

\ ============================================================================
\ Markdown Parser
\ ============================================================================

variable in-code-block
variable out-file

: emit-html ( addr u -- )
  out-file @ write-string ;

: emit-line ( addr u -- )
  out-file @ write-line ;

: parse-header ( addr u level -- )
  \ Parse header with given level (1-6)
  >r r@ /string trim
  s" <h" emit-html
  r@ 0 <# #s #> emit-html
  s" >" emit-html
  emit-html
  s" </h" emit-html
  r> 0 <# #s #> emit-html
  s" >" emit-line ;

: parse-list-item ( addr u -- )
  2 /string trim
  s" <li>" emit-html
  emit-html
  s" </li>" emit-line ;

: parse-paragraph ( addr u -- )
  s" <p>" emit-html
  emit-html
  s" </p>" emit-line ;

: parse-markdown-line ( addr u -- )
  \ Skip empty lines
  dup 0= if 2drop exit then

  \ Code blocks
  2dup s" ```" starts-with? if
    in-code-block @ if
      s" </code></pre>" emit-line
      false in-code-block !
    else
      s" <pre><code>" emit-html
      true in-code-block !
    then
    2drop exit
  then

  in-code-block @ if
    escape-html emit-line
    exit
  then

  \ Headers
  2dup s" ### " starts-with? if 3 parse-header exit then
  2dup s" ## " starts-with? if 2 parse-header exit then
  2dup s" # " starts-with? if 1 parse-header exit then

  \ List items
  2dup s" - " starts-with? if parse-list-item exit then
  2dup s" * " starts-with? if parse-list-item exit then

  \ Paragraph
  trim parse-paragraph ;

\ ============================================================================
\ HTML Generation
\ ============================================================================

: write-html-header ( -- )
  s" <!DOCTYPE html>" emit-line
  s" <html lang='en'>" emit-line
  s" <head>" emit-line
  s" <meta charset='UTF-8'>" emit-line
  s" <title>" emit-html
  page-title page-title-len @ emit-html
  s" </title>" emit-line
  s" <style>" emit-line
  s" body{font-family:monospace;max-width:70ch;margin:0 auto;padding:2rem;}" emit-line
  s" pre{background:#f4f4f4;padding:1rem;overflow-x:auto;}" emit-line
  s" </style>" emit-line
  s" </head>" emit-line
  s" <body>" emit-line
  s" <article>" emit-line
  s" <h1>" emit-html
  page-title page-title-len @ emit-html
  s" </h1>" emit-line
  page-date-len @ if
    s" <time>" emit-html
    page-date page-date-len @ emit-html
    s" </time>" emit-line
  then ;

: write-html-footer ( -- )
  s" </article>" emit-line
  s" <footer><p>Generated by odd-ssg (Forth)</p></footer>" emit-line
  s" </body>" emit-line
  s" </html>" emit-line ;

\ ============================================================================
\ File Processing
\ ============================================================================

: process-file ( src-addr src-u out-addr out-u -- )
  \ Open output file
  w/o create-file throw out-file !

  \ Open input file
  r/o open-file throw file-handle !

  init-metadata
  false in-code-block !

  \ Read first line, check for frontmatter
  read-line-safe if
    2dup s" ---" compare 0= if
      2drop
      \ Read frontmatter
      begin
        read-line-safe while
        2dup s" ---" compare 0= if
          2drop true
        else
          parse-frontmatter-line false
        then
      until
    else
      \ No frontmatter, process as content
      s" Untitled" page-title swap dup page-title-len ! move
      write-html-header
      parse-markdown-line
    then
  then

  write-html-header

  \ Process content lines
  begin
    read-line-safe while
    parse-markdown-line
  repeat

  write-html-footer

  file-handle @ close-file throw
  out-file @ close-file throw

  ." Processed: " type cr ;

\ ============================================================================
\ Commands
\ ============================================================================

: build ( -- )
  ." odd-ssg: Building site..." cr

  \ Create output directory (via shell)
  s" mkdir -p " pad swap move
  output-dir pad 9 + swap move
  pad 9 output-dir nip + system

  \ Process index.md -> index.html
  s" content/index.md" s" _site/index.html" process-file

  ." odd-ssg: Build complete." cr ;

: init ( -- )
  ." odd-ssg: Initializing new site..." cr

  \ Create directories
  s" mkdir -p content" system
  s" mkdir -p _site" system

  \ Create sample content
  s" content/index.md" w/o create-file throw
  dup s" ---" rot write-line
  dup s" title: Welcome to Odd" rot write-line
  dup s" date: 2025-01-18" rot write-line
  dup s" ---" rot write-line
  dup s" " rot write-line
  dup s" # Welcome" rot write-line
  dup s" " rot write-line
  dup s" This site was built with **odd-ssg**, a Forth-powered static site generator." rot write-line
  dup s" " rot write-line
  dup s" ## Stack-Based Features" rot write-line
  dup s" " rot write-line
  dup s" - Postfix notation throughout" rot write-line
  dup s" - Word definitions for extensibility" rot write-line
  dup s" - Minimal memory footprint" rot write-line
  dup s" - Interactive REPL development" rot write-line
  close-file throw

  ." odd-ssg: Site initialized." cr
  ." Run 'gforth src/odd.fs -e \"build bye\"' to build." cr ;

: clean ( -- )
  ." odd-ssg: Cleaning _site/..." cr
  s" rm -rf _site" system
  ." odd-ssg: Clean complete." cr ;

: help ( -- )
  ." odd-ssg - Forth Static Site Generator" cr
  cr
  ." USAGE:" cr
  ."   gforth src/odd.fs -e '<command> bye'" cr
  cr
  ." COMMANDS:" cr
  ."   build    Build the site" cr
  ."   init     Initialize a new site" cr
  ."   clean    Remove generated files" cr
  ."   help     Show this help" cr
  cr
  ." FEATURES:" cr
  ."   - Stack-based processing" cr
  ."   - Word-defined transformations" cr
  ."   - Minimal dependencies (Gforth)" cr ;

\ Default action
\ help
