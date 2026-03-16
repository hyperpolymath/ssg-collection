⍝ SPDX-License-Identifier: PMPL-1.0-or-later
⍝ SPDX-FileCopyrightText: 2025 hyperpolymath
⍝
⍝ iota-ssg - APL Static Site Generator
⍝ "Let arrays reshape. Let operators compose. Let sites transform."
⍝
⍝ Run with: dyalog src/iota.apl -x
⍝           gnu-apl -f src/iota.apl

⍝ ============================================================================
⍝ Configuration
⍝ ============================================================================

CONTENT∆DIR←'content'
OUTPUT∆DIR←'_site'
SITE∆TITLE←'Iota Site'

⍝ ============================================================================
⍝ String Utilities
⍝ ============================================================================

∇ R←TRIM S
  ⍝ Remove leading and trailing whitespace
  R←((∨\S≠' ')∧⌽∨\⌽S≠' ')/S
∇

∇ R←S STARTS∆WITH P
  ⍝ Check if S starts with prefix P
  →(⍴P)>⍴S ⋄ R←0 ⋄ →0
  R←∧/P=S[⍳⍴P]
∇

∇ R←ESCAPE∆HTML S
  ⍝ Escape HTML special characters
  R←S
  R←∊('&amp;' '&')⎕R R
  R←∊('&lt;' '<')⎕R R
  R←∊('&gt;' '>')⎕R R
  R←∊('&quot;' '"')⎕R R
∇

∇ R←JOIN∆LINES V
  ⍝ Join vector of strings with newlines
  R←∊V,¨⊂(⎕UCS 10)
∇

⍝ ============================================================================
⍝ File I/O
⍝ ============================================================================

∇ LINES←READ∆FILE PATH
  ⍝ Read file as vector of lines
  LINES←⊃⎕NGET PATH 1
∇

∇ PATH WRITE∆FILE CONTENT
  ⍝ Write content to file
  CONTENT ⎕NPUT PATH 1
∇

⍝ ============================================================================
⍝ Frontmatter Parser
⍝ ============================================================================

PAGE∆TITLE←'Untitled'
PAGE∆DATE←''
PAGE∆DRAFT←0
BODY∆START←1

∇ PARSE∆FRONTMATTER LINES;I;LINE;COLON;KEY;VALUE
  ⍝ Parse YAML-like frontmatter
  PAGE∆TITLE←'Untitled'
  PAGE∆DATE←''
  PAGE∆DRAFT←0
  BODY∆START←1

  →(0=⍴LINES)/0
  →(~(TRIM 1⊃LINES)≡'---')/0

  I←2
LOOP:
  →(I>⍴LINES)/0
  LINE←TRIM I⊃LINES
  →(LINE≡'---')/DONE

  COLON←LINE⍳':'
  →(COLON>⍴LINE)/NEXT
  KEY←TRIM COLON↑LINE
  VALUE←TRIM (COLON+1)↓LINE

  →(KEY≡'title')/SET∆TITLE
  →(KEY≡'date')/SET∆DATE
  →(KEY≡'draft')/SET∆DRAFT
  →NEXT

SET∆TITLE:
  PAGE∆TITLE←VALUE
  →NEXT
SET∆DATE:
  PAGE∆DATE←VALUE
  →NEXT
SET∆DRAFT:
  PAGE∆DRAFT←(VALUE≡'true')∨VALUE≡'yes'
  →NEXT

NEXT:
  I←I+1
  →LOOP

DONE:
  BODY∆START←I+1
∇

⍝ ============================================================================
⍝ Markdown Parser
⍝ ============================================================================

∇ HTML←PARSE∆MARKDOWN LINES;I;LINE;TRIMMED;IN∆CODE;TEXT
  ⍝ Convert markdown to HTML
  HTML←''
  IN∆CODE←0
  I←BODY∆START

MDLOOP:
  →(I>⍴LINES)/MDDONE
  LINE←I⊃LINES
  TRIMMED←TRIM LINE

  ⍝ Code blocks
  →(~TRIMMED STARTS∆WITH '```')/NOTCODE
  →IN∆CODE/ENDCODE
  HTML←HTML,'<pre><code>'
  IN∆CODE←1
  →MDNEXT
ENDCODE:
  HTML←HTML,'</code></pre>',(⎕UCS 10)
  IN∆CODE←0
  →MDNEXT

NOTCODE:
  →(~IN∆CODE)/NOTINCODE
  HTML←HTML,(ESCAPE∆HTML LINE),(⎕UCS 10)
  →MDNEXT

NOTINCODE:
  ⍝ Headers
  →(~TRIMMED STARTS∆WITH '### ')/NOH3
  TEXT←4↓TRIMMED
  HTML←HTML,'<h3>',TEXT,'</h3>',(⎕UCS 10)
  →MDNEXT
NOH3:
  →(~TRIMMED STARTS∆WITH '## ')/NOH2
  TEXT←3↓TRIMMED
  HTML←HTML,'<h2>',TEXT,'</h2>',(⎕UCS 10)
  →MDNEXT
NOH2:
  →(~TRIMMED STARTS∆WITH '# ')/NOH1
  TEXT←2↓TRIMMED
  HTML←HTML,'<h1>',TEXT,'</h1>',(⎕UCS 10)
  →MDNEXT
NOH1:
  ⍝ List items
  →(~(TRIMMED STARTS∆WITH '- ')∨TRIMMED STARTS∆WITH '* ')/NOLIST
  TEXT←2↓TRIMMED
  HTML←HTML,'<li>',TEXT,'</li>',(⎕UCS 10)
  →MDNEXT
NOLIST:
  ⍝ Empty
  →(0=⍴TRIMMED)/MDNEXT
  ⍝ Paragraph
  HTML←HTML,'<p>',TRIMMED,'</p>',(⎕UCS 10)

MDNEXT:
  I←I+1
  →MDLOOP

MDDONE:
∇

⍝ ============================================================================
⍝ HTML Generation
⍝ ============================================================================

∇ HTML←GENERATE∆PAGE CONTENT∆HTML;NL
  ⍝ Generate complete HTML page
  NL←⎕UCS 10
  HTML←'<!DOCTYPE html>',NL
  HTML←HTML,'<html lang="en">',NL
  HTML←HTML,'<head>',NL
  HTML←HTML,'<meta charset="UTF-8">',NL
  HTML←HTML,'<title>',(ESCAPE∆HTML PAGE∆TITLE),' | ',SITE∆TITLE,'</title>',NL
  HTML←HTML,'<style>',NL
  HTML←HTML,'body{font-family:monospace;max-width:70ch;margin:0 auto;padding:2rem;}',NL
  HTML←HTML,'pre{background:#f5f5f5;padding:1rem;}',NL
  HTML←HTML,'</style>',NL
  HTML←HTML,'</head>',NL
  HTML←HTML,'<body>',NL
  HTML←HTML,'<article>',NL
  HTML←HTML,'<h1>',(ESCAPE∆HTML PAGE∆TITLE),'</h1>',NL
  →(0=⍴PAGE∆DATE)/NODATE
  HTML←HTML,'<time>',PAGE∆DATE,'</time>',NL
NODATE:
  HTML←HTML,CONTENT∆HTML
  HTML←HTML,'</article>',NL
  HTML←HTML,'<footer><p>Generated by iota-ssg (APL)</p></footer>',NL
  HTML←HTML,'</body>',NL
  HTML←HTML,'</html>',NL
∇

⍝ ============================================================================
⍝ Commands
⍝ ============================================================================

∇ BUILD;LINES;CONTENT∆HTML;HTML;SRC;OUT
  ⍝ Build the site
  ⎕←'iota-ssg: Building site...'
  ⎕←'  Output directory: ',OUTPUT∆DIR

  ⍝ Create output directory
  ⎕SH 'mkdir -p ',OUTPUT∆DIR

  ⍝ Process index.md
  SRC←CONTENT∆DIR,'/index.md'
  OUT←OUTPUT∆DIR,'/index.html'

  ⎕←'  Processing: ',SRC
  LINES←READ∆FILE SRC
  PARSE∆FRONTMATTER LINES

  →PAGE∆DRAFT/SKIPBUILD
  CONTENT∆HTML←PARSE∆MARKDOWN LINES
  HTML←GENERATE∆PAGE CONTENT∆HTML
  OUT WRITE∆FILE HTML
  ⎕←'  Written: ',OUT
  →BUILDDONE
SKIPBUILD:
  ⎕←'    Skipping draft'
BUILDDONE:
  ⎕←'iota-ssg: Build complete.'
∇

∇ INIT;SAMPLE;NL
  ⍝ Initialize new site
  ⎕←'iota-ssg: Initializing new site...'

  ⎕SH 'mkdir -p ',CONTENT∆DIR
  ⎕SH 'mkdir -p ',OUTPUT∆DIR

  NL←⎕UCS 10
  SAMPLE←'---',NL
  SAMPLE←SAMPLE,'title: Welcome to Iota',NL
  SAMPLE←SAMPLE,'date: 2025-01-18',NL
  SAMPLE←SAMPLE,'---',NL,NL
  SAMPLE←SAMPLE,'# Welcome',NL,NL
  SAMPLE←SAMPLE,'This site was built with **iota-ssg**, an APL-powered SSG.',NL,NL
  SAMPLE←SAMPLE,'## APL Features',NL,NL
  SAMPLE←SAMPLE,'- Array-oriented processing',NL
  SAMPLE←SAMPLE,'- Symbolic operators',NL
  SAMPLE←SAMPLE,'- Tacit programming',NL
  SAMPLE←SAMPLE,'- Concise notation',NL,NL
  SAMPLE←SAMPLE,'## Code Example',NL,NL
  SAMPLE←SAMPLE,'```apl',NL
  SAMPLE←SAMPLE,'⍝ Generate first 10 primes',NL
  SAMPLE←SAMPLE,'(⊢~∘.×⍨)1↓⍳20',NL
  SAMPLE←SAMPLE,'```',NL

  (CONTENT∆DIR,'/index.md') WRITE∆FILE SAMPLE

  ⎕←'  Created sample content'
  ⎕←'iota-ssg: Site initialized.'
  ⎕←'Run BUILD to build.'
∇

∇ CLEAN
  ⍝ Clean output directory
  ⎕←'iota-ssg: Cleaning ',OUTPUT∆DIR,'...'
  ⎕SH 'rm -rf ',OUTPUT∆DIR
  ⎕←'iota-ssg: Clean complete.'
∇

∇ HELP
  ⍝ Show help
  ⎕←'iota-ssg - APL Static Site Generator'
  ⎕←''
  ⎕←'USAGE:'
  ⎕←'  Load in APL interpreter, then call:'
  ⎕←'  BUILD - Build the site'
  ⎕←'  INIT  - Initialize new site'
  ⎕←'  CLEAN - Remove generated files'
  ⎕←'  HELP  - Show this help'
  ⎕←''
  ⎕←'FEATURES:'
  ⎕←'  - Array-based content processing'
  ⎕←'  - Operator composition for transforms'
  ⎕←'  - Concise, expressive code'
∇

⍝ Show help on load
HELP
