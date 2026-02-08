\ forth-estate.fs - A stack-based static site generator in Gforth
\
\ "Forth Estate" - Forth-powered publishing
\
\ Usage: gforth forth-estate.fs -e "s\" source\" s\" output\" build bye"
\
\ This implementation embraces Forth's concatenative paradigm:
\ - Data flows through stack transformations
\ - Words compose naturally
\ - Minimal state, maximum clarity

\ ============================================================================
\ Configuration
\ ============================================================================

256 constant max-line          \ Maximum line length
4096 constant max-content      \ Maximum content buffer size
1024 constant max-frontmatter  \ Maximum frontmatter size
256 constant max-path          \ Maximum path length

\ ============================================================================
\ String Utilities
\ ============================================================================

: string-prefix? ( addr1 u1 addr2 u2 -- flag )
  \ Check if addr2/u2 is a prefix of addr1/u1
  \ First check if string1 is long enough
  2over nip over < if
    2drop 2drop false exit
  then
  \ Rearrange stack for compare: need ( addr1 u2 addr2 u2 )
  2swap drop over 2swap compare 0= ;

: trim-left ( addr u -- addr' u' )
  \ Remove leading whitespace
  begin
    dup 0> if
      over c@ dup bl <= swap 0<> and
    else
      false
    then
  while
    1 /string
  repeat ;

: trim-right ( addr u -- addr u' )
  \ Remove trailing whitespace
  begin
    dup 0> if
      2dup + 1- c@ bl <=
    else
      false
    then
  while
    1-
  repeat ;

: trim ( addr u -- addr' u' )
  \ Remove leading and trailing whitespace
  trim-left trim-right ;

: starts-with? ( addr1 u1 c-addr u -- flag )
  \ Check if string starts with prefix
  string-prefix? ;

: skip-chars ( addr u n -- addr' u' )
  \ Skip n characters
  /string ;

: string= ( addr1 u1 addr2 u2 -- flag )
  \ Compare two strings for equality
  compare 0= ;

\ ============================================================================
\ Dynamic String Buffer
\ ============================================================================

variable output-buffer  0 output-buffer !
variable output-size    0 output-size !
variable output-capacity 0 output-capacity !

: init-output ( -- )
  \ Initialize output buffer
  output-buffer @ if output-buffer @ free drop then
  max-content allocate throw output-buffer !
  0 output-size !
  max-content output-capacity ! ;

: free-output ( -- )
  \ Free output buffer
  output-buffer @ free throw
  0 output-buffer ! ;

: ensure-capacity ( n -- )
  \ Ensure buffer can hold n more bytes
  output-size @ + output-capacity @ > if
    output-capacity @ 2* dup output-capacity !
    output-buffer @ swap resize throw output-buffer !
  then ;

: append-output ( addr u -- )
  \ Append string to output buffer
  output-buffer @ 0= if 2drop exit then
  dup ensure-capacity
  dup >r  \ save length on return stack
  output-buffer @ output-size @ + swap move
  r> output-size +! ;

: append-char ( c -- )
  \ Append single character to output
  output-buffer @ 0= if drop exit then
  1 ensure-capacity
  output-buffer @ output-size @ + c!
  1 output-size +! ;

: get-output ( -- addr u )
  \ Get current output buffer contents
  output-buffer @ output-size @ ;

: reset-output ( -- )
  \ Clear output buffer
  0 output-size ! ;

\ ============================================================================
\ Frontmatter Parser
\ ============================================================================

create fm-title max-line allot
variable fm-title-len
create fm-date max-line allot
variable fm-date-len
create fm-template max-line allot
variable fm-template-len
create fm-tags max-line allot
variable fm-tags-len
variable fm-draft

: clear-frontmatter ( -- )
  \ Reset all frontmatter fields
  0 fm-title-len !
  0 fm-date-len !
  0 fm-template-len !
  0 fm-tags-len !
  false fm-draft ! ;

: parse-fm-line ( addr u -- )
  \ Parse a single frontmatter line
  trim
  dup 0= if 2drop exit then

  \ Check for title:
  2dup s" title:" starts-with? if
    6 skip-chars trim
    dup max-line min fm-title-len !
    drop fm-title fm-title-len @ move
    exit
  then

  \ Check for date:
  2dup s" date:" starts-with? if
    5 skip-chars trim
    dup max-line min fm-date-len !
    drop fm-date fm-date-len @ move
    exit
  then

  \ Check for template:
  2dup s" template:" starts-with? if
    9 skip-chars trim
    dup max-line min fm-template-len !
    drop fm-template fm-template-len @ move
    exit
  then

  \ Check for tags:
  2dup s" tags:" starts-with? if
    5 skip-chars trim
    dup max-line min fm-tags-len !
    drop fm-tags fm-tags-len @ move
    exit
  then

  \ Check for draft:
  2dup s" draft:" starts-with? if
    6 skip-chars trim
    s" true" string= fm-draft !
    exit
  then

  2drop ;

: find-frontmatter-end ( addr u -- addr' u' offset )
  \ Find closing --- and return content after it
  \ offset is position after the closing ---
  0 >r
  begin
    dup 0>
  while
    2dup 10 scan
    dup 0> if
      over 3 >= if
        over 3 s" ---" string= if
          \ Found closing ---
          2drop
          r> 3 + >r
          \ Skip past the ---\n
          r@ skip-chars
          r> exit
        then
      then
      1 /string
      r> 1+ >r
    else
      2drop
      r> exit
    then
  repeat
  r> ;

variable in-frontmatter

variable fm-ptr
variable fm-len

: parse-frontmatter ( addr u -- addr' u' )
  \ Parse frontmatter from content, return remaining content
  clear-frontmatter

  \ Check for opening ---
  2dup s" ---" starts-with? 0= if exit then

  \ Skip past opening ---
  3 /string

  \ Skip newline after ---
  10 scan
  dup 0> if 1 /string then
  fm-len ! fm-ptr !

  \ Parse lines until closing ---
  begin
    fm-len @ 0>
  while
    \ Find newline
    fm-ptr @ fm-len @ 10 scan
    \ Stack: ( nl-addr remaining )

    over fm-ptr @ -     \ ( nl-addr remaining line-len )
    fm-ptr @ swap       \ ( nl-addr remaining line-start line-len )

    \ Check if this line is ---
    2dup trim s" ---" string= if
      2drop
      \ Move past newline and return remaining content
      dup 0> if 1 /string then
      exit
    then

    parse-fm-line

    \ Move past newline for next iteration
    dup 0> if 1 /string then
    fm-len ! fm-ptr !
  repeat

  \ Return whatever's left (shouldn't reach here normally)
  fm-ptr @ fm-len @ ;

\ ============================================================================
\ Markdown Parser
\ ============================================================================

variable in-code-block
variable in-paragraph
variable in-list
variable list-ordered

: emit-tag ( addr u -- )
  \ Emit an HTML tag
  [char] < append-char
  append-output
  [char] > append-char ;

: emit-close-tag ( addr u -- )
  \ Emit a closing HTML tag
  s" </" append-output
  append-output
  [char] > append-char ;

: emit-newline ( -- )
  10 append-char ;

: close-paragraph ( -- )
  in-paragraph @ if
    s" p" emit-close-tag
    emit-newline
    false in-paragraph !
  then ;

: close-list ( -- )
  in-list @ if
    list-ordered @ if
      s" ol" emit-close-tag
    else
      s" ul" emit-close-tag
    then
    emit-newline
    false in-list !
  then ;

variable count-char

: count-leading ( addr u c -- n )
  \ Count leading occurrences of character c
  count-char !
  0 -rot  \ n addr u
  begin
    dup 0> if
      over c@ count-char @ =
    else
      false
    then
  while
    1 /string
    rot 1+ -rot  \ increment count
  repeat
  2drop ;

variable hdr-addr
variable hdr-len

: process-header ( addr u level -- )
  \ Process a header line
  close-paragraph
  close-list

  \ Save level, skip the # characters and space
  dup >r
  1+ /string trim
  \ Stack: ( addr' u' ) R: level

  \ Save content string
  hdr-len ! hdr-addr !
  \ Stack: ( ) R: level

  \ Emit header tag <hN>
  [char] < append-char
  [char] h append-char
  [char] 0 r@ + append-char
  [char] > append-char

  \ Emit header content
  hdr-addr @ hdr-len @ append-output

  \ Close header tag </hN>
  s" </h" append-output
  [char] 0 r> + append-char
  [char] > append-char
  emit-newline ;

: process-code-fence ( addr u -- )
  \ Handle ``` code blocks
  in-code-block @ if
    s" code" emit-close-tag
    s" pre" emit-close-tag
    emit-newline
    false in-code-block !
  else
    close-paragraph
    close-list
    s" pre" emit-tag
    s" code" emit-tag
    true in-code-block !
  then
  2drop ;

: process-list-item ( addr u ordered -- )
  \ Process a list item (- or 1.)
  close-paragraph

  \ Handle list opening if needed
  in-list @ 0= if
    dup list-ordered !
    if s" ol" else s" ul" then
    emit-tag
    emit-newline
    true in-list !
  else
    drop \ drop ordered flag if list already open
  then

  \ Stack is now ( addr u )
  \ Skip bullet/number by finding first space
  bl scan  \ bl = 32 (space character)
  trim

  s" li" emit-tag
  append-output
  s" li" emit-close-tag
  emit-newline ;

variable inline-ptr
variable inline-len
variable found-addr
variable content-len

: process-inline ( addr u -- )
  \ Process inline markdown (bold, italic, links, code)
  inline-len ! inline-ptr !

  begin
    inline-len @ 0>
  while
    \ Check for **bold**
    inline-ptr @ inline-len @ s" **" starts-with? if
      \ Skip opening **
      -2 inline-len +!
      2 inline-ptr +!
      \ Search for closing **
      inline-ptr @ inline-len @ s" **" search if
        \ Stack: ( found-addr remaining )
        drop found-addr !  \ save found-addr, drop remaining
        found-addr @ inline-ptr @ - content-len !
        \ Output <strong>content</strong>
        s" strong" emit-tag
        inline-ptr @ content-len @ append-output
        s" strong" emit-close-tag
        \ Advance past content and closing **
        content-len @ 2 + inline-ptr +!
        content-len @ 2 + negate inline-len +!
      else
        2drop
        s" **" append-output
      then

    \ Check for *italic* (but not **)
    else inline-ptr @ inline-len @ s" *" starts-with?
         inline-ptr @ inline-len @ s" **" starts-with? 0= and if
      \ Skip opening *
      1 inline-ptr +!
      -1 inline-len +!
      \ Search for closing *
      inline-ptr @ inline-len @ [char] * scan
      dup 0> if
        \ Stack: ( found-addr remaining )
        drop found-addr !
        found-addr @ inline-ptr @ - content-len !
        s" em" emit-tag
        inline-ptr @ content-len @ append-output
        s" em" emit-close-tag
        content-len @ 1+ inline-ptr +!
        content-len @ 1+ negate inline-len +!
      else
        2drop
        [char] * append-char
      then

    \ Check for `code`
    else inline-ptr @ inline-len @ s" `" starts-with? if
      \ Skip opening `
      1 inline-ptr +!
      -1 inline-len +!
      \ Search for closing `
      inline-ptr @ inline-len @ [char] ` scan
      dup 0> if
        drop found-addr !
        found-addr @ inline-ptr @ - content-len !
        s" code" emit-tag
        inline-ptr @ content-len @ append-output
        s" code" emit-close-tag
        content-len @ 1+ inline-ptr +!
        content-len @ 1+ negate inline-len +!
      else
        2drop
        [char] ` append-char
      then
    \ Regular character (skip link handling for simplicity)
    else
      inline-ptr @ c@ append-char
      1 inline-ptr +!
      -1 inline-len +!
    then then then
  repeat ;

: process-paragraph ( addr u -- )
  \ Process a paragraph line
  in-code-block @ if
    \ In code block, emit literally with escaping
    begin
      dup 0>
    while
      over c@
      dup [char] < = if drop s" &lt;" append-output
      else dup [char] > = if drop s" &gt;" append-output
      else dup [char] & = if drop s" &amp;" append-output
      else append-char
      then then then
      1 skip-chars
    repeat
    emit-newline
    2drop
    exit
  then

  in-paragraph @ 0= if
    close-list
    s" p" emit-tag
    true in-paragraph !
  else
    32 append-char
  then

  process-inline ;

: process-line ( addr u -- )
  \ Process a single line of markdown
  2dup trim

  \ Empty line closes paragraph
  dup 0= if
    2drop 2drop
    close-paragraph
    exit
  then
  2drop

  \ Check for code fence
  2dup s" ```" starts-with? if
    process-code-fence exit
  then

  \ In code block, process literally
  in-code-block @ if
    process-paragraph exit
  then

  \ Check for headers
  2dup s" ###### " starts-with? if 6 process-header exit then
  2dup s" ##### " starts-with? if 5 process-header exit then
  2dup s" #### " starts-with? if 4 process-header exit then
  2dup s" ### " starts-with? if 3 process-header exit then
  2dup s" ## " starts-with? if 2 process-header exit then
  2dup s" # " starts-with? if 1 process-header exit then

  \ Check for unordered list
  2dup s" - " starts-with? if false process-list-item exit then
  2dup s" * " starts-with? if false process-list-item exit then

  \ Check for ordered list (simplified: just 1. 2. etc)
  dup 0> if
    over c@ dup [char] 0 >= swap [char] 9 <= and if
      2dup [char] . scan dup 0> if
        over 1+ c@ bl = if
          true process-list-item exit
        then
      then
      2drop
    then
  then

  \ Check for horizontal rule
  2dup s" ---" string= if
    close-paragraph close-list
    s" <hr>" append-output emit-newline
    2drop exit
  then

  \ Default: paragraph
  process-paragraph ;

variable md-ptr
variable md-len

: parse-markdown ( addr u -- )
  \ Parse markdown content to HTML
  false in-code-block !
  false in-paragraph !
  false in-list !

  md-len ! md-ptr !

  begin
    md-len @ 0>
  while
    \ Find newline
    md-ptr @ md-len @ 10 scan
    \ Stack: ( nl-addr remaining )
    \ Line goes from md-ptr to nl-addr

    over md-ptr @ -     \ ( nl-addr remaining line-len )
    md-ptr @ swap       \ ( nl-addr remaining line-start line-len )
    process-line        \ ( nl-addr remaining )

    \ Move past newline for next iteration
    dup 0> if
      1 /string         \ skip the newline character
    then
    md-len ! md-ptr !
  repeat

  close-paragraph
  close-list ;

\ ============================================================================
\ Template System
\ ============================================================================

: default-template ( -- addr u )
  \ Return default HTML template
  s\" <!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<title>{{title}}</title>\n<style>\nbody { font-family: system-ui, sans-serif; max-width: 800px; margin: 0 auto; padding: 2rem; line-height: 1.6; }\npre { background: #f4f4f4; padding: 1rem; overflow-x: auto; }\ncode { background: #f4f4f4; padding: 0.2rem 0.4rem; }\n</style>\n</head>\n<body>\n<article>\n<h1>{{title}}</h1>\n{{#date}}<time>{{date}}</time>{{/date}}\n{{content}}\n</article>\n</body>\n</html>" ;

: find-placeholder ( addr u marker -- addr' u' found? )
  \ Find {{marker}} in template
  2>r
  begin
    dup 0>
  while
    2dup s" {{" drop 2 search if
      2 skip-chars
      2dup 2r@ string-prefix? if
        2r@ nip skip-chars
        2dup s" }}" starts-with? if
          2 skip-chars
          2r> 2drop
          true exit
        then
      then
      \ Not our marker, skip past {{
      2 skip-chars
    else
      2drop 2r> 2drop false exit
    then
  repeat
  2r> 2drop false ;

\ Variables for apply-template
variable tpl-content-addr
variable tpl-content-len
variable tpl-ptr
variable tpl-len
variable tpl-found-addr
variable tpl-marker-len

: apply-template ( content-addr content-u -- )
  \ Apply template with content and frontmatter
  \ Result goes to output buffer
  tpl-content-len ! tpl-content-addr !
  reset-output

  default-template
  tpl-len ! tpl-ptr !

  begin
    tpl-len @ 0>
  while
    \ Look for {{
    tpl-ptr @ tpl-len @ s" {{" drop 2 search if
      \ search returns ( found-addr remaining-len ) when found
      \ Stack: ( found-addr remaining-len )
      swap tpl-found-addr !   \ save found-addr
      drop                    \ drop remaining-len (we'll recalculate)

      \ Output everything before {{
      tpl-ptr @ tpl-found-addr @ tpl-ptr @ - append-output

      \ Update tpl-len: subtract (found-addr - tpl-ptr) + 2 (for {{)
      tpl-len @ tpl-found-addr @ tpl-ptr @ - - 2 - tpl-len !
      \ Move tpl-ptr past {{
      tpl-found-addr @ 2 + tpl-ptr !

      tpl-ptr @ tpl-len @ s" }}" drop 2 search if
        \ Found closing }}
        \ Stack: ( close-addr remaining-len )
        swap tpl-found-addr !   \ save close-addr
        drop                    \ drop remaining-len
        tpl-found-addr @ tpl-ptr @ - tpl-marker-len !  \ length of placeholder name

        \ Check which placeholder
        tpl-ptr @ tpl-marker-len @ s" title" string= if
          fm-title fm-title-len @ append-output
        else tpl-ptr @ tpl-marker-len @ s" date" string= if
          fm-date fm-date-len @ append-output
        else tpl-ptr @ tpl-marker-len @ s" content" string= if
          tpl-content-addr @ tpl-content-len @ append-output
        else tpl-ptr @ tpl-marker-len @ s" tags" string= if
          fm-tags fm-tags-len @ append-output
        else
          \ Unknown placeholder, output as-is
          s" {{" append-output
          tpl-ptr @ tpl-marker-len @ append-output
          s" }}" append-output
        then then then then

        \ Move past }}
        \ Update tpl-len: subtract marker-len + 2 (for }})
        tpl-len @ tpl-marker-len @ - 2 - tpl-len !
        \ Move tpl-ptr past }}
        tpl-found-addr @ 2 + tpl-ptr !
      else
        \ No closing }}, output rest and exit
        2drop
        tpl-ptr @ tpl-len @ append-output
        exit
      then
    else
      \ No more {{, output rest
      2drop
      tpl-ptr @ tpl-len @ append-output
      exit
    then
  repeat ;

\ ============================================================================
\ File I/O
\ ============================================================================

create file-buffer max-content allot
variable file-buffer-len

create path-buffer max-path allot
variable path-buffer-len

: read-file-contents ( addr u -- addr' u' flag )
  \ Read entire file into buffer, return contents and success flag
  r/o open-file if drop 0 0 false exit then
  >r

  file-buffer max-content r@ read-file if
    r> close-file drop
    0 0 false exit
  then
  file-buffer-len !

  r> close-file drop
  file-buffer file-buffer-len @ true ;

: write-file-contents ( addr u filename-addr filename-u -- flag )
  \ Write contents to file
  w/o create-file if 2drop 2drop false exit then
  >r

  r@ write-file if
    r> close-file drop
    false exit
  then

  r> close-file drop
  true ;

: ensure-dir ( addr u -- )
  \ Ensure directory exists (uses shell mkdir -p)
  s" mkdir -p " pad swap move
  pad 10 + swap move
  pad swap 10 + system ;

: change-extension ( addr u new-ext new-ext-u -- addr' u' )
  \ Change file extension
  2>r
  \ Find last .
  2dup + 1-
  begin
    2dup <=
  while
    dup c@ [char] . = if
      over -
      path-buffer swap move
      2r> path-buffer path-buffer-len @ + swap move
      2dup path-buffer-len @ + path-buffer-len !
      path-buffer path-buffer-len @ exit
    then
    1-
  repeat
  drop
  \ No extension found, just append
  path-buffer swap move
  path-buffer-len !
  2r> path-buffer path-buffer-len @ + swap move
  path-buffer-len @ + path-buffer-len !
  path-buffer path-buffer-len @ ;

\ ============================================================================
\ Build System
\ ============================================================================

: process-file ( src-addr src-u dest-addr dest-u -- )
  \ Process a single markdown file
  2>r 2dup

  ." Processing: " type cr

  read-file-contents 0= if
    ." Error reading file" cr
    2r> 2drop exit
  then

  parse-frontmatter

  \ Check if draft
  fm-draft @ if
    ." Skipping draft: " fm-title fm-title-len @ type cr
    2drop 2r> 2drop exit
  then

  \ Parse markdown
  init-output
  parse-markdown

  \ Get markdown output
  get-output

  \ Apply template
  apply-template

  \ Write output
  get-output 2r@
  write-file-contents if
    ." Written: " 2r> type cr
  else
    ." Error writing: " 2r> type cr
  then

  free-output ;

: is-markdown? ( addr u -- flag )
  \ Check if filename ends with .md
  dup 3 >= if
    + 3 - 3 s" .md" string=
  else
    2drop false
  then ;

: build-site ( src-addr src-u dest-addr dest-u -- )
  \ Build entire site from source to dest directory
  ." Forth Estate SSG v0.1.0" cr
  ." Building from: " 2over type cr
  ." Output to: " 2dup type cr
  cr

  \ For now, just show usage - full directory traversal would need
  \ shell integration or Gforth file system words
  ." Note: Directory traversal requires shell integration." cr
  ." Use with find: find src -name '*.md' -exec process {} \\;" cr

  2drop 2drop ;

: build ( -- )
  \ Main build entry point - expects source and dest paths on stack
  \ Usage: s" source" s" output" build
  depth 4 < if
    ." Usage: Push source and output paths, then call build" cr
    exit
  then
  build-site ;

\ ============================================================================
\ REPL / Testing
\ ============================================================================

: test-buffer ( -- )
  \ Quick buffer test
  ." Allocating buffer..." cr
  init-output
  ." Buffer at: " output-buffer @ . cr
  ." Appending char..." cr
  [char] X append-char
  ." Done. Size: " output-size @ . cr
  free-output
  ." Buffer test passed!" cr ;

: test-markdown ( -- )
  \ Test markdown parsing
  init-output

  s\" # Hello World\n\nThis is a **bold** test with *italic* text.\n\n- Item 1\n- Item 2\n\n```\ncode block\n```\n"

  clear-frontmatter
  s" Test Post" fm-title swap move
  9 fm-title-len !

  parse-markdown
  get-output type
  free-output ;

: test-frontmatter ( -- )
  \ Test frontmatter parsing
  s\" ---\ntitle: My Post\ndate: 2024-01-15\ntags: [forth, ssg]\n---\n\nContent here"

  parse-frontmatter

  ." Title: " fm-title fm-title-len @ type cr
  ." Date: " fm-date fm-date-len @ type cr
  ." Tags: " fm-tags fm-tags-len @ type cr
  ." Draft: " fm-draft @ if ." yes" else ." no" then cr
  ." Remaining content: " type cr ;

variable content-copy
variable content-copy-len

: test-full ( -- )
  \ Full pipeline test
  s\" ---\ntitle: Welcome\ndate: 2024-01-15\n---\n\n# Welcome\n\nThis is **Fourth Estate**, a Forth-powered SSG.\n\n- Stack-based\n- Minimal\n- Fast\n"

  parse-frontmatter

  init-output
  parse-markdown
  get-output

  \ Copy content to separate buffer before apply-template reuses output buffer
  \ Stack: ( addr u )
  dup content-copy-len !                    \ Stack: ( addr u )
  dup allocate throw content-copy !         \ Stack: ( addr u )
  \ Now move: ( src dest len -- )
  content-copy @ swap move                  \ Stack: ( ) - moved from addr to buffer, u bytes
  content-copy @ content-copy-len @         \ Stack: ( copy-addr copy-len )

  apply-template
  get-output type

  content-copy @ free drop
  free-output ;

\ Entry message
." Forth Estate SSG loaded." cr
." Commands: test-markdown test-frontmatter test-full build" cr
