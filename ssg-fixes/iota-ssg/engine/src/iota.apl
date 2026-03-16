⍝ SPDX-License-Identifier: PMPL-1.0-or-later
⍝ SPDX-FileCopyrightText: 2025 hyperpolymath
⍝
⍝ iota-ssg: APL static site generator
⍝ "Site generation in a single expression"
⍝ Named after APL's iota (⍳) function - the index generator

⍝ ═══════════════════════════════════════════════════════════════════
⍝ CONFIGURATION
⍝ ═══════════════════════════════════════════════════════════════════

∇ config←GetConfig
    config←⍬
    config,←⊂'name' 'iota-site'
    config,←⊂'output' '_site'
    config,←⊂'charset' 'UTF-8'
∇

⍝ ═══════════════════════════════════════════════════════════════════
⍝ HTML ESCAPING
⍝ ═══════════════════════════════════════════════════════════════════

∇ result←EscapeHTML text
    ⍝ Escape HTML special characters
    ⍝ & must be replaced first to avoid double-escaping
    result←text
    result←'&amp;'⎕R'&'⊢result
    result←'&lt;'⎕R'<'⊢result
    result←'&gt;'⎕R'>'⊢result
    result←'&quot;'⎕R'"'⊢result
    result←'&#39;'⎕R''''⊢result
∇

⍝ ═══════════════════════════════════════════════════════════════════
⍝ MARKDOWN PARSING
⍝ ═══════════════════════════════════════════════════════════════════

∇ html←ParseMarkdown markdown
    ⍝ Convert simple Markdown to HTML
    lines←(⎕UCS 10)⎕SPLIT markdown
    html←⊃,/ProcessLine¨lines
∇

∇ result←ProcessLine line
    ⍝ Process a single Markdown line
    →(0=≢line)/Empty
    →(line≡⍬)/Empty
    →('# '≡2↑line)/H1
    →('## '≡3↑line)/H2
    →('### '≡4↑line)/H3
    →(('- '≡2↑line)∨('* '≡2↑line))/LI
    →Para
Empty:result←'' ⋄ →0
H1:result←'<h1>',(2↓line),'</h1>',⎕UCS 10 ⋄ →0
H2:result←'<h2>',(3↓line),'</h2>',⎕UCS 10 ⋄ →0
H3:result←'<h3>',(4↓line),'</h3>',⎕UCS 10 ⋄ →0
LI:result←'<li>',(2↓line),'</li>',⎕UCS 10 ⋄ →0
Para:result←'<p>',line,'</p>',⎕UCS 10
∇

⍝ ═══════════════════════════════════════════════════════════════════
⍝ PAGE RENDERING
⍝ ═══════════════════════════════════════════════════════════════════

∇ html←RenderPage args
    ⍝ Render a page to HTML
    ⍝ args: title content
    (title content)←args
    escaped←EscapeHTML title

    html←'<!DOCTYPE html>',⎕UCS 10
    html,←'<html lang="en">',⎕UCS 10
    html,←'<head>',⎕UCS 10
    html,←'    <meta charset="UTF-8">',⎕UCS 10
    html,←'    <title>',escaped,'</title>',⎕UCS 10
    html,←'</head>',⎕UCS 10
    html,←'<body>',⎕UCS 10
    html,←'    <article>',⎕UCS 10
    html,←'        <h1>',escaped,'</h1>',⎕UCS 10
    html,←'        ',content,⎕UCS 10
    html,←'    </article>',⎕UCS 10
    html,←'</body>',⎕UCS 10
    html,←'</html>',⎕UCS 10
∇

⍝ ═══════════════════════════════════════════════════════════════════
⍝ FILE OPERATIONS
⍝ ═══════════════════════════════════════════════════════════════════

∇ WriteFile args
    ⍝ Write content to file
    (path content)←args
    (⊂content)⎕NPUT path 1
∇

∇ EnsureDir path
    ⍝ Create directory if it doesn't exist
    :If ~⎕NEXISTS path
        ⎕MKDIR path
    :EndIf
∇

⍝ ═══════════════════════════════════════════════════════════════════
⍝ SITE BUILDING
⍝ ═══════════════════════════════════════════════════════════════════

∇ BuildPage args;outPath;html
    ⍝ Build a single page
    ⍝ args: outputDir title path content
    (outputDir title path content)←args

    html←RenderPage title content
    outPath←outputDir,'/',path,'.html'

    WriteFile outPath html
    ⎕←'iota-ssg: Built ',outPath
∇

∇ BuildSite pages;config;outputDir;count
    ⍝ Build the entire site
    ⍝ pages: list of (title path content) tuples
    config←GetConfig
    outputDir←2⊃⊃(config[;1]⍳⊂'output')⌷config

    ⎕←'iota-ssg: Building site...'
    EnsureDir outputDir

    count←≢pages
    :For page :In pages
        BuildPage outputDir,page
    :EndFor

    ⎕←'iota-ssg: Built ',(⍕count),' pages'
∇

⍝ ═══════════════════════════════════════════════════════════════════
⍝ MAIN
⍝ ═══════════════════════════════════════════════════════════════════

∇ Main
    ⍝ Main entry point
    pages←⍬
    pages,←⊂'Welcome to iota-ssg' 'index' '<p>Site generation in a single expression.</p>'

    BuildSite pages
∇

⍝ One-liner version (for demonstration):
⍝ {⎕NPUT(⍵,'.html')(('<!DOCTYPE html><html><head><title>',⍺,'</title></head><body><h1>',⍺,'</h1><p>Welcome</p></body></html>'))}

⍝ Run main
Main
