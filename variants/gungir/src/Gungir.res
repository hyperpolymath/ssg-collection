// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// gungir-ssg - ReScript Static Site Generator
// "Let types infer. Let variants match. Let sites compile."
//
// Build with: deno task res:build
// Run with: deno run --allow-read --allow-write src/Gungir.res.js build

// ============================================================================
// Configuration
// ============================================================================

let contentDir = "content"
let outputDir = "_site"
let siteTitle = "Gungir Site"

// ============================================================================
// Types
// ============================================================================

type pageMeta = {
  mutable title: string,
  mutable date: string,
  mutable draft: bool,
}

type parseResult = {
  meta: pageMeta,
  bodyStart: int,
}

// ============================================================================
// String Utilities
// ============================================================================

let trim = s => String.trim(s)

let startsWith = (s, prefix) => String.startsWith(s, prefix)

let escapeHtml = s => {
  s
  ->String.replaceAll("&", "&amp;")
  ->String.replaceAll("<", "&lt;")
  ->String.replaceAll(">", "&gt;")
  ->String.replaceAll("\"", "&quot;")
}

let substring = (s, start, end) => String.substring(s, ~start, ~end)

// ============================================================================
// Frontmatter Parser
// ============================================================================

let parseFrontmatter = (lines: array<string>): parseResult => {
  let meta = {title: "Untitled", date: "", draft: false}
  let bodyStart = ref(0)

  if Array.length(lines) == 0 {
    {meta, bodyStart: 0}
  } else {
    let firstLine = trim(lines[0]->Option.getOr(""))
    if firstLine != "---" {
      {meta, bodyStart: 0}
    } else {
      let i = ref(1)
      let inFrontmatter = ref(true)

      while i.contents < Array.length(lines) && inFrontmatter.contents {
        let line = trim(lines[i.contents]->Option.getOr(""))

        if line == "---" {
          bodyStart := i.contents + 1
          inFrontmatter := false
        } else {
          switch String.indexOf(line, ":") {
          | Some(colonPos) =>
            let key = trim(substring(line, 0, colonPos))
            let value = trim(substring(line, colonPos + 1, String.length(line)))

            switch key {
            | "title" => meta.title = value
            | "date" => meta.date = value
            | "draft" => meta.draft = value == "true" || value == "yes"
            | _ => ()
            }
          | None => ()
          }
        }
        i := i.contents + 1
      }

      {meta, bodyStart: bodyStart.contents}
    }
  }
}

// ============================================================================
// Markdown Parser
// ============================================================================

let parseMarkdown = (lines: array<string>, startIdx: int): string => {
  let html = ref("")
  let inCode = ref(false)

  for i in startIdx to Array.length(lines) - 1 {
    let line = lines[i]->Option.getOr("")
    let trimmed = trim(line)

    // Code blocks
    if startsWith(trimmed, "```") {
      if inCode.contents {
        html := html.contents ++ "</code></pre>\n"
        inCode := false
      } else {
        html := html.contents ++ "<pre><code>"
        inCode := true
      }
    } else if inCode.contents {
      html := html.contents ++ escapeHtml(line) ++ "\n"
    } else if startsWith(trimmed, "### ") {
      html := html.contents ++ "<h3>" ++ substring(trimmed, 4, String.length(trimmed)) ++ "</h3>\n"
    } else if startsWith(trimmed, "## ") {
      html := html.contents ++ "<h2>" ++ substring(trimmed, 3, String.length(trimmed)) ++ "</h2>\n"
    } else if startsWith(trimmed, "# ") {
      html := html.contents ++ "<h1>" ++ substring(trimmed, 2, String.length(trimmed)) ++ "</h1>\n"
    } else if startsWith(trimmed, "- ") || startsWith(trimmed, "* ") {
      html := html.contents ++ "<li>" ++ substring(trimmed, 2, String.length(trimmed)) ++ "</li>\n"
    } else if String.length(trimmed) > 0 {
      html := html.contents ++ "<p>" ++ trimmed ++ "</p>\n"
    }
  }

  html.contents
}

// ============================================================================
// HTML Generation
// ============================================================================

let generatePage = (meta: pageMeta, contentHtml: string): string => {
  let dateHtml = if meta.date == "" {
    ""
  } else {
    `<time>${meta.date}</time>\n`
  }

  `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>${escapeHtml(meta.title)} | ${siteTitle}</title>
<style>
body{font-family:system-ui,sans-serif;max-width:700px;margin:0 auto;padding:2rem;}
pre{background:#f5f5f5;padding:1rem;overflow-x:auto;}
time{color:#666;font-style:italic;}
</style>
</head>
<body>
<article>
<h1>${escapeHtml(meta.title)}</h1>
${dateHtml}${contentHtml}</article>
<footer><p>Generated by gungir-ssg (ReScript)</p></footer>
</body>
</html>
`
}

// ============================================================================
// File Operations (Deno bindings)
// ============================================================================

@val external denoReadTextFile: string => promise<string> = "Deno.readTextFile"
@val external denoWriteTextFile: (string, string) => promise<unit> = "Deno.writeTextFile"
@val external denoMkdir: (string, {"recursive": bool}) => promise<unit> = "Deno.mkdir"

let readFile = async path => {
  await denoReadTextFile(path)
}

let writeFile = async (path, content) => {
  await denoWriteTextFile(path, content)
}

let ensureDir = async path => {
  try {
    await denoMkdir(path, {"recursive": true})
  } catch {
  | _ => ()
  }
}

// ============================================================================
// Commands
// ============================================================================

let processFile = async (srcPath, outPath) => {
  Console.log(`  Processing: ${srcPath}`)

  let content = await readFile(srcPath)
  let lines = String.split(content, "\n")

  let {meta, bodyStart} = parseFrontmatter(lines)

  if meta.draft {
    Console.log("    Skipping draft")
  } else {
    let contentHtml = parseMarkdown(lines, bodyStart)
    let html = generatePage(meta, contentHtml)

    await writeFile(outPath, html)
    Console.log(`  Written: ${outPath}`)
  }
}

let cmdBuild = async () => {
  Console.log("gungir-ssg: Building site...")
  Console.log(`  Output directory: ${outputDir}`)

  await ensureDir(outputDir)

  let srcPath = `${contentDir}/index.md`
  let outPath = `${outputDir}/index.html`

  try {
    await processFile(srcPath, outPath)
    Console.log("gungir-ssg: Build complete.")
  } catch {
  | _ => Console.log("  No content found. Run 'gungir init' first.")
  }
}

let cmdInit = async () => {
  Console.log("gungir-ssg: Initializing new site...")

  await ensureDir(contentDir)
  Console.log(`  Created ${contentDir}/`)
  await ensureDir(outputDir)
  Console.log(`  Created ${outputDir}/`)

  let sample = `---
title: Welcome to Gungir
date: 2025-01-18
---

# Welcome

This site was built with **gungir-ssg**, a ReScript-powered SSG.

## ReScript Features

- Type inference throughout
- Pattern matching
- Variant types
- Fast compilation to JS

## Code Example

\`\`\`rescript
type user = {name: string, age: int}

let greet = (user: user) =>
  \`Hello, \${user.name}! You are \${Int.toString(user.age)} years old.\`

let me = {name: "World", age: 42}
Console.log(greet(me))
\`\`\`
`

  await writeFile(`${contentDir}/index.md`, sample)
  Console.log("  Created sample content")
  Console.log("gungir-ssg: Site initialized.")
  Console.log("Run 'deno run --allow-all lib/es6/src/Gungir.res.js build' to build.")
}

let cmdClean = async () => {
  Console.log(`gungir-ssg: Cleaning ${outputDir}/...`)
  try {
    @val external denoRemove: (string, {"recursive": bool}) => promise<unit> = "Deno.remove"
    await denoRemove(outputDir, {"recursive": true})
  } catch {
  | _ => ()
  }
  Console.log("gungir-ssg: Clean complete.")
}

let cmdHelp = () => {
  Console.log("gungir-ssg - ReScript Static Site Generator")
  Console.log("")
  Console.log("USAGE:")
  Console.log("  deno run --allow-all lib/es6/src/Gungir.res.js <command>")
  Console.log("")
  Console.log("COMMANDS:")
  Console.log("  build    Build the site")
  Console.log("  init     Initialize a new site")
  Console.log("  clean    Remove generated files")
  Console.log("  help     Show this help")
  Console.log("")
  Console.log("FEATURES:")
  Console.log("  - Type-safe processing")
  Console.log("  - Pattern matching transforms")
  Console.log("  - Compiled to optimized JS")
}

// ============================================================================
// Main
// ============================================================================

@val external denoArgs: array<string> = "Deno.args"

let main = async () => {
  let cmd = denoArgs[0]->Option.getOr("help")

  switch cmd {
  | "build" => await cmdBuild()
  | "init" => await cmdInit()
  | "clean" => await cmdClean()
  | "help" => cmdHelp()
  | _ =>
    Console.log(`Unknown command: ${cmd}`)
    cmdHelp()
  }
}

let _ = main()
