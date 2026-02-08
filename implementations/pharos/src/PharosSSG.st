"SPDX-License-Identifier: AGPL-3.0-or-later"
"SPDX-FileCopyrightText: 2025 hyperpolymath"
"
pharos-ssg - Pharo Smalltalk Static Site Generator
'Let messages send. Let objects live. Let sites emerge.'

Load with: Metacello new load: 'PharosSSG'.
Run with: PharosSSG build. PharosSSG init. PharosSSG clean.
"

Object subclass: #PharosSSG
    instanceVariableNames: ''
    classVariableNames: 'ContentDir OutputDir SiteTitle'
    package: 'PharosSSG'

"Class-side initialization"
PharosSSG class >> initialize [
    ContentDir := 'content'.
    OutputDir := '_site'.
    SiteTitle := 'Pharos Site'.
]

"Configuration accessors"
PharosSSG class >> contentDir [ ^ ContentDir ]
PharosSSG class >> outputDir [ ^ OutputDir ]
PharosSSG class >> siteTitle [ ^ SiteTitle ]

"String utilities"
PharosSSG class >> escapeHtml: aString [
    ^ aString
        copyReplaceAll: '&' with: '&amp;';
        copyReplaceAll: '<' with: '&lt;';
        copyReplaceAll: '>' with: '&gt;';
        copyReplaceAll: '"' with: '&quot;'
]

"Page metadata"
Object subclass: #PageMeta
    instanceVariableNames: 'title date draft'
    classVariableNames: ''
    package: 'PharosSSG'

PageMeta >> initialize [
    title := 'Untitled'.
    date := ''.
    draft := false.
]

PageMeta >> title [ ^ title ]
PageMeta >> title: aString [ title := aString ]
PageMeta >> date [ ^ date ]
PageMeta >> date: aString [ date := aString ]
PageMeta >> draft [ ^ draft ]
PageMeta >> draft: aBoolean [ draft := aBoolean ]

"Frontmatter Parser"
PharosSSG class >> parseFrontmatter: lines [
    | meta bodyStart inFrontmatter |
    meta := PageMeta new.
    bodyStart := 1.

    lines isEmpty ifTrue: [ ^ { meta. bodyStart } ].

    (lines first trimBoth = '---') ifFalse: [ ^ { meta. bodyStart } ].

    inFrontmatter := true.
    2 to: lines size do: [ :i |
        | line colonIndex key value |
        line := (lines at: i) trimBoth.

        line = '---' ifTrue: [
            bodyStart := i + 1.
            inFrontmatter := false.
            ^ { meta. bodyStart }
        ].

        inFrontmatter ifTrue: [
            colonIndex := line indexOf: $:.
            colonIndex > 0 ifTrue: [
                key := (line copyFrom: 1 to: colonIndex - 1) trimBoth.
                value := (line copyFrom: colonIndex + 1 to: line size) trimBoth.

                key = 'title' ifTrue: [ meta title: value ].
                key = 'date' ifTrue: [ meta date: value ].
                key = 'draft' ifTrue: [
                    meta draft: (value = 'true' or: [ value = 'yes' ])
                ].
            ]
        ]
    ].

    ^ { meta. bodyStart }
]

"Markdown Parser"
PharosSSG class >> parseMarkdown: lines from: startIndex [
    | html inCode |
    html := '' writeStream.
    inCode := false.

    startIndex to: lines size do: [ :i |
        | line trimmed |
        line := lines at: i.
        trimmed := line trimBoth.

        "Code blocks"
        (trimmed beginsWith: '```') ifTrue: [
            inCode ifTrue: [
                html nextPutAll: '</code></pre>'; cr.
                inCode := false
            ] ifFalse: [
                html nextPutAll: '<pre><code>'.
                inCode := true
            ].
        ] ifFalse: [
            inCode ifTrue: [
                html nextPutAll: (self escapeHtml: line); cr
            ] ifFalse: [
                "Headers"
                (trimmed beginsWith: '### ') ifTrue: [
                    html nextPutAll: '<h3>';
                         nextPutAll: (trimmed copyFrom: 5 to: trimmed size);
                         nextPutAll: '</h3>'; cr
                ] ifFalse: [
                (trimmed beginsWith: '## ') ifTrue: [
                    html nextPutAll: '<h2>';
                         nextPutAll: (trimmed copyFrom: 4 to: trimmed size);
                         nextPutAll: '</h2>'; cr
                ] ifFalse: [
                (trimmed beginsWith: '# ') ifTrue: [
                    html nextPutAll: '<h1>';
                         nextPutAll: (trimmed copyFrom: 3 to: trimmed size);
                         nextPutAll: '</h1>'; cr
                ] ifFalse: [
                "List items"
                ((trimmed beginsWith: '- ') or: [trimmed beginsWith: '* ']) ifTrue: [
                    html nextPutAll: '<li>';
                         nextPutAll: (trimmed copyFrom: 3 to: trimmed size);
                         nextPutAll: '</li>'; cr
                ] ifFalse: [
                "Paragraph"
                trimmed isEmpty ifFalse: [
                    html nextPutAll: '<p>';
                         nextPutAll: trimmed;
                         nextPutAll: '</p>'; cr
                ]]]]]
            ]
        ]
    ].

    ^ html contents
]

"HTML Generation"
PharosSSG class >> generatePage: meta content: contentHtml [
    | html |
    html := '' writeStream.

    html nextPutAll: '<!DOCTYPE html>'; cr;
         nextPutAll: '<html lang="en">'; cr;
         nextPutAll: '<head>'; cr;
         nextPutAll: '<meta charset="UTF-8">'; cr;
         nextPutAll: '<title>';
         nextPutAll: (self escapeHtml: meta title);
         nextPutAll: ' | ';
         nextPutAll: self siteTitle;
         nextPutAll: '</title>'; cr;
         nextPutAll: '<style>'; cr;
         nextPutAll: 'body{font-family:Georgia,serif;max-width:700px;margin:0 auto;padding:2rem;}'; cr;
         nextPutAll: 'pre{background:#f5f5f5;padding:1rem;}'; cr;
         nextPutAll: '</style>'; cr;
         nextPutAll: '</head>'; cr;
         nextPutAll: '<body>'; cr;
         nextPutAll: '<article>'; cr;
         nextPutAll: '<h1>';
         nextPutAll: (self escapeHtml: meta title);
         nextPutAll: '</h1>'; cr.

    meta date isEmpty ifFalse: [
        html nextPutAll: '<time>';
             nextPutAll: meta date;
             nextPutAll: '</time>'; cr
    ].

    html nextPutAll: contentHtml;
         nextPutAll: '</article>'; cr;
         nextPutAll: '<footer><p>Generated by pharos-ssg (Pharo Smalltalk)</p></footer>'; cr;
         nextPutAll: '</body>'; cr;
         nextPutAll: '</html>'; cr.

    ^ html contents
]

"Process single file"
PharosSSG class >> processFile: srcPath to: outPath [
    | content lines result meta bodyStart contentHtml html |

    Transcript show: '  Processing: ', srcPath; cr.

    content := srcPath asFileReference contents.
    lines := content lines.

    result := self parseFrontmatter: lines.
    meta := result first.
    bodyStart := result second.

    meta draft ifTrue: [
        Transcript show: '    Skipping draft'; cr.
        ^ self
    ].

    contentHtml := self parseMarkdown: lines from: bodyStart.
    html := self generatePage: meta content: contentHtml.

    outPath asFileReference writeStreamDo: [ :stream |
        stream nextPutAll: html
    ].

    Transcript show: '  Written: ', outPath; cr.
]

"Commands"
PharosSSG class >> build [
    | srcPath outPath |

    Transcript show: 'pharos-ssg: Building site...'; cr;
               show: '  Output directory: ', self outputDir; cr.

    self outputDir asFileReference ensureCreateDirectory.

    srcPath := self contentDir, '/index.md'.
    outPath := self outputDir, '/index.html'.

    srcPath asFileReference exists ifFalse: [
        Transcript show: '  No content found. Run PharosSSG init first.'; cr.
        ^ self
    ].

    self processFile: srcPath to: outPath.

    Transcript show: 'pharos-ssg: Build complete.'; cr.
]

PharosSSG class >> init [
    | samplePath sample |

    Transcript show: 'pharos-ssg: Initializing new site...'; cr.

    self contentDir asFileReference ensureCreateDirectory.
    self outputDir asFileReference ensureCreateDirectory.

    samplePath := self contentDir, '/index.md'.
    sample := '---
title: Welcome to Pharos
date: 2025-01-18
---

# Welcome

This site was built with **pharos-ssg**, a Pharo Smalltalk-powered SSG.

## Smalltalk Features

- Everything is an object
- Live coding environment
- Image-based development
- Message passing paradigm

## Code Example

```smalltalk
Object subclass: #Greeting
    instanceVariableNames: ''name''
    package: ''Demo''.

Greeting >> greet
    ^ ''Hello, '', name, ''!''

(Greeting new name: ''World'') greet.
```
'.

    samplePath asFileReference writeStreamDo: [ :stream |
        stream nextPutAll: sample
    ].

    Transcript show: '  Created sample content'; cr;
               show: 'pharos-ssg: Site initialized.'; cr;
               show: 'Run PharosSSG build to build.'; cr.
]

PharosSSG class >> clean [
    Transcript show: 'pharos-ssg: Cleaning ', self outputDir, '/...'; cr.

    self outputDir asFileReference ensureDeleteAll.

    Transcript show: 'pharos-ssg: Clean complete.'; cr.
]

PharosSSG class >> help [
    Transcript
        show: 'pharos-ssg - Pharo Smalltalk Static Site Generator'; cr; cr;
        show: 'USAGE:'; cr;
        show: '  PharosSSG build    Build the site'; cr;
        show: '  PharosSSG init     Initialize a new site'; cr;
        show: '  PharosSSG clean    Remove generated files'; cr;
        show: '  PharosSSG help     Show this help'; cr; cr;
        show: 'FEATURES:'; cr;
        show: '  - Object-oriented throughout'; cr;
        show: '  - Live development in image'; cr;
        show: '  - Message-passing architecture'; cr.
]

"Initialize on load"
PharosSSG initialize.
