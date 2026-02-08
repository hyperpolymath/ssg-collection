NB. SPDX-License-Identifier: AGPL-3.0-or-later
NB. SPDX-FileCopyrightText: 2025 hyperpolymath
NB.
NB. jura-ssg: J static site generator
NB. "Prehistoric site generation" - named after the Jurassic period
NB. Array-oriented, tacit programming for maximum expressiveness

NB. ═══════════════════════════════════════════════════════════════════
NB. CONFIGURATION
NB. ═══════════════════════════════════════════════════════════════════

SITE_NAME =: 'jura-site'
OUTPUT_DIR =: '_site'

NB. ═══════════════════════════════════════════════════════════════════
NB. STRING UTILITIES
NB. ═══════════════════════════════════════════════════════════════════

NB. Replace substring in string
NB. (old;new) stringreplace string
stringreplace =: 4 : 0
  'old new' =. x
  new (I. old E. y)} :: ] y
)

NB. Join strings with separator
NB. sep joinstrings boxed_strings
joinstrings =: 4 : 0
  ; (< x) ,. y
)

NB. ═══════════════════════════════════════════════════════════════════
NB. HTML ESCAPING
NB. ═══════════════════════════════════════════════════════════════════

NB. Escape HTML special characters
escapehtml =: 3 : 0
  r =. y
  r =. ('&';'&amp;') stringreplace r
  r =. ('<';'&lt;') stringreplace r
  r =. ('>';'&gt;') stringreplace r
  r =. ('"';'&quot;') stringreplace r
  r =. ('''';'&#39;') stringreplace r
  r
)

NB. ═══════════════════════════════════════════════════════════════════
NB. MARKDOWN PARSING
NB. ═══════════════════════════════════════════════════════════════════

NB. Process a single markdown line
processline =: 3 : 0
  line =. y
  if. 0 = # line do. '' return. end.

  NB. Heading 1
  if. '# ' -: 2 {. line do.
    '<h1>' , (2 }. line) , '</h1>' , LF return.
  end.

  NB. Heading 2
  if. '## ' -: 3 {. line do.
    '<h2>' , (3 }. line) , '</h2>' , LF return.
  end.

  NB. Heading 3
  if. '### ' -: 4 {. line do.
    '<h3>' , (4 }. line) , '</h3>' , LF return.
  end.

  NB. List item
  if. ('- ' -: 2 {. line) +. ('* ' -: 2 {. line) do.
    '<li>' , (2 }. line) , '</li>' , LF return.
  end.

  NB. Paragraph
  '<p>' , line , '</p>' , LF
)

NB. Parse markdown to HTML
parsemarkdown =: 3 : 0
  lines =. LF cut y
  ; processline each lines
)

NB. ═══════════════════════════════════════════════════════════════════
NB. PAGE RENDERING
NB. ═══════════════════════════════════════════════════════════════════

NB. Render page to HTML
NB. (title ; content) renderpage ''
renderpage =: 3 : 0
  'title content' =. y
  escaped =. escapehtml title

  html =. '<!DOCTYPE html>' , LF
  html =. html , '<html lang="en">' , LF
  html =. html , '<head>' , LF
  html =. html , '    <meta charset="UTF-8">' , LF
  html =. html , '    <title>' , escaped , '</title>' , LF
  html =. html , '</head>' , LF
  html =. html , '<body>' , LF
  html =. html , '    <article>' , LF
  html =. html , '        <h1>' , escaped , '</h1>' , LF
  html =. html , '        ' , content , LF
  html =. html , '    </article>' , LF
  html =. html , '</body>' , LF
  html =. html , '</html>' , LF
)

NB. ═══════════════════════════════════════════════════════════════════
NB. FILE OPERATIONS
NB. ═══════════════════════════════════════════════════════════════════

NB. Write content to file
writefile =: 4 : 0
  content =. x
  path =. y
  content fwrite path
)

NB. Create directory
ensuredir =: 3 : 0
  if. -. fexist y do.
    mkdir y
  end.
)

NB. ═══════════════════════════════════════════════════════════════════
NB. SITE BUILDING
NB. ═══════════════════════════════════════════════════════════════════

NB. Build a single page
NB. (outputDir ; title ; path ; content) buildpage ''
buildpage =: 3 : 0
  'outputDir title path content' =. y

  html =. renderpage title ; content
  outpath =. outputDir , '/' , path , '.html'

  html writefile outpath
  echo 'jura-ssg: Built ' , outpath
)

NB. Build the site
NB. pages buildsite ''  where pages is a table of (title;path;content)
buildsite =: 3 : 0
  pages =. y

  echo 'jura-ssg: Building site ''' , SITE_NAME , '''...'
  ensuredir OUTPUT_DIR

  for_page. pages do.
    'title path content' =. page
    buildpage OUTPUT_DIR ; title ; path ; content
  end.

  echo 'jura-ssg: Built ' , (": # pages) , ' pages'
)

NB. ═══════════════════════════════════════════════════════════════════
NB. MAIN
NB. ═══════════════════════════════════════════════════════════════════

main =: 3 : 0
  NB. Example pages
  pages =. ,: 'Welcome to jura-ssg' ; 'index' ; '<p>Prehistoric site generation with J.</p>'

  buildsite pages
)

NB. Tacit one-liner for simple page generation:
NB. render =: '<html><head><title>' , ] , '</title></head><body><h1>' , ] , '</h1></body></html>' ,~ [

NB. Run main
main ''
