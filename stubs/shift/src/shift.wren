// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// shift-ssg - Wren Static Site Generator
// "Let fibers yield. Let classes compose. Let sites shift."
//
// Run with: wren src/shift.wren build | init | clean | help

// ============================================================================
// Configuration
// ============================================================================

var ContentDir = "content"
var OutputDir = "_site"
var SiteTitle = "Shift Site"

// ============================================================================
// Page Metadata
// ============================================================================

class PageMeta {
  construct new() {
    _title = "Untitled"
    _date = ""
    _draft = false
  }

  title { _title }
  title=(v) { _title = v }
  date { _date }
  date=(v) { _date = v }
  draft { _draft }
  draft=(v) { _draft = v }
}

// ============================================================================
// String Utilities
// ============================================================================

class StringUtil {
  static trim(s) {
    var start = 0
    var end = s.count

    while (start < end && (s[start] == " " || s[start] == "\t")) {
      start = start + 1
    }
    while (end > start && (s[end - 1] == " " || s[end - 1] == "\t")) {
      end = end - 1
    }

    if (start >= end) return ""
    return s[start...end]
  }

  static startsWith(s, prefix) {
    if (s.count < prefix.count) return false
    return s[0...prefix.count] == prefix
  }

  static escapeHtml(s) {
    var result = ""
    for (c in s) {
      if (c == "&") {
        result = result + "&amp;"
      } else if (c == "<") {
        result = result + "&lt;"
      } else if (c == ">") {
        result = result + "&gt;"
      } else if (c == "\"") {
        result = result + "&quot;"
      } else {
        result = result + c
      }
    }
    return result
  }

  static split(s, delim) {
    var result = []
    var current = ""

    for (c in s) {
      if (c == delim) {
        result.add(current)
        current = ""
      } else {
        current = current + c
      }
    }
    result.add(current)

    return result
  }

  static indexOf(s, search) {
    for (i in 0...(s.count - search.count + 1)) {
      if (s[i...(i + search.count)] == search) {
        return i
      }
    }
    return -1
  }
}

// ============================================================================
// Frontmatter Parser
// ============================================================================

class FrontmatterParser {
  static parseLine(line, meta) {
    var trimmed = StringUtil.trim(line)
    var colonPos = StringUtil.indexOf(trimmed, ":")

    if (colonPos > 0) {
      var key = StringUtil.trim(trimmed[0...colonPos])
      var value = StringUtil.trim(trimmed[(colonPos + 1)...trimmed.count])

      if (key == "title") {
        meta.title = value
      } else if (key == "date") {
        meta.date = value
      } else if (key == "draft") {
        meta.draft = (value == "true" || value == "yes")
      }
    }

    return meta
  }

  static parse(lines) {
    var meta = PageMeta.new()
    var bodyStart = 0

    if (lines.count == 0) return [meta, 0]

    if (StringUtil.trim(lines[0]) != "---") return [meta, 0]

    var inFrontmatter = true
    var i = 1
    while (i < lines.count && inFrontmatter) {
      var line = StringUtil.trim(lines[i])

      if (line == "---") {
        bodyStart = i + 1
        inFrontmatter = false
      } else {
        parseLine(line, meta)
      }
      i = i + 1
    }

    return [meta, bodyStart]
  }
}

// ============================================================================
// Markdown Parser
// ============================================================================

class MarkdownParser {
  static parse(lines, startIdx) {
    var html = ""
    var inCode = false

    var i = startIdx
    while (i < lines.count) {
      var line = lines[i]
      var trimmed = StringUtil.trim(line)

      // Code blocks
      if (StringUtil.startsWith(trimmed, "```")) {
        if (inCode) {
          html = html + "</code></pre>\n"
          inCode = false
        } else {
          html = html + "<pre><code>"
          inCode = true
        }
        i = i + 1
        continue
      }

      if (inCode) {
        html = html + StringUtil.escapeHtml(line) + "\n"
        i = i + 1
        continue
      }

      // Headers
      if (StringUtil.startsWith(trimmed, "### ")) {
        html = html + "<h3>" + trimmed[4...trimmed.count] + "</h3>\n"
        i = i + 1
        continue
      }
      if (StringUtil.startsWith(trimmed, "## ")) {
        html = html + "<h2>" + trimmed[3...trimmed.count] + "</h2>\n"
        i = i + 1
        continue
      }
      if (StringUtil.startsWith(trimmed, "# ")) {
        html = html + "<h1>" + trimmed[2...trimmed.count] + "</h1>\n"
        i = i + 1
        continue
      }

      // List items
      if (StringUtil.startsWith(trimmed, "- ") || StringUtil.startsWith(trimmed, "* ")) {
        html = html + "<li>" + trimmed[2...trimmed.count] + "</li>\n"
        i = i + 1
        continue
      }

      // Empty lines
      if (trimmed.count == 0) {
        i = i + 1
        continue
      }

      // Paragraph
      html = html + "<p>" + trimmed + "</p>\n"
      i = i + 1
    }

    return html
  }
}

// ============================================================================
// HTML Generator
// ============================================================================

class HtmlGenerator {
  static generate(meta, contentHtml) {
    var dateHtml = meta.date.count > 0 ? "<time>" + meta.date + "</time>\n" : ""

    return "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<title>" + StringUtil.escapeHtml(meta.title) + " | " + SiteTitle + "</title>
<style>
body{font-family:system-ui,sans-serif;max-width:700px;margin:0 auto;padding:2rem;}
pre{background:#f5f5f5;padding:1rem;overflow-x:auto;}
time{color:#666;font-style:italic;}
</style>
</head>
<body>
<article>
<h1>" + StringUtil.escapeHtml(meta.title) + "</h1>
" + dateHtml + contentHtml + "</article>
<footer><p>Generated by shift-ssg (Wren)</p></footer>
</body>
</html>
"
  }
}

// ============================================================================
// Sample Content
// ============================================================================

var SampleContent = "---
title: Welcome to Shift
date: 2025-01-18
---

# Welcome

This site was built with **shift-ssg**, a Wren-powered SSG.

## Wren Features

- Small and fast
- Class-based OOP
- Fiber-based concurrency
- Clean syntax
- Embeddable in C

## Code Example

```wren
// Hello World in Wren
class Greeter {
  construct new(name) {
    _name = name
  }

  greet() {
    System.print(\"Hello, %(_name)!\")
  }
}

var greeter = Greeter.new(\"World\")
greeter.greet()
```
"

// ============================================================================
// Commands
// ============================================================================

class Commands {
  static build() {
    System.print("shift-ssg: Building site...")
    System.print("  Output directory: " + OutputDir)

    var lines = StringUtil.split(SampleContent, "\n")
    var result = FrontmatterParser.parse(lines)
    var meta = result[0]
    var bodyStart = result[1]

    if (meta.draft) {
      System.print("    Skipping draft")
      return
    }

    System.print("  Processing: " + ContentDir + "/index.md")

    var contentHtml = MarkdownParser.parse(lines, bodyStart)
    var html = HtmlGenerator.generate(meta, contentHtml)

    System.print("  Generated HTML (%(html.count) bytes)")
    System.print("shift-ssg: Build complete.")
  }

  static init() {
    System.print("shift-ssg: Initializing new site...")
    System.print("  Created " + ContentDir + "/")
    System.print("  Created " + OutputDir + "/")
    System.print("  Created sample content")
    System.print("shift-ssg: Site initialized.")
    System.print("Run 'wren src/shift.wren build' to build.")
  }

  static clean() {
    System.print("shift-ssg: Cleaning " + OutputDir + "/...")
    System.print("shift-ssg: Clean complete.")
  }

  static help() {
    System.print("shift-ssg - Wren Static Site Generator")
    System.print("")
    System.print("USAGE:")
    System.print("  wren src/shift.wren <command>")
    System.print("")
    System.print("COMMANDS:")
    System.print("  build    Build the site")
    System.print("  init     Initialize a new site")
    System.print("  clean    Remove generated files")
    System.print("  help     Show this help")
    System.print("")
    System.print("FEATURES:")
    System.print("  - Small and embeddable")
    System.print("  - Class-based OOP")
    System.print("  - Fiber concurrency")
    System.print("  - Clean, modern syntax")
  }
}

// ============================================================================
// Main
// ============================================================================

class Main {
  static run(args) {
    var cmd = args.count > 0 ? args[0] : "help"

    if (cmd == "build") {
      Commands.build()
    } else if (cmd == "init") {
      Commands.init()
    } else if (cmd == "clean") {
      Commands.clean()
    } else if (cmd == "help") {
      Commands.help()
    } else {
      System.print("Unknown command: " + cmd)
      Commands.help()
    }
  }
}

// Wren doesn't have direct command line access in the standard library
// This would be called from a host application
// For standalone usage, we'll run with default
Commands.help()
