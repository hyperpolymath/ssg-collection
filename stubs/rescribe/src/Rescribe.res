// Rescribe.res - ReScript-powered static site generator
//
// "Rescribe" - Type-safe site generation with JavaScript output
//
// ReScript's sound type system compiles to efficient JavaScript.
// Perfect for static sites that need both safety and web compatibility.

// ============================================================================
// Types
// ============================================================================

type frontmatter = {
  title: string,
  date: string,
  tags: array<string>,
  draft: bool,
  template: string,
}

type parserState = {
  mutable html: string,
  mutable inPara: bool,
  mutable inCode: bool,
  mutable inList: bool,
}

let emptyFrontmatter = {
  title: "",
  date: "",
  tags: [],
  draft: false,
  template: "default",
}

let initState = () => {
  html: "",
  inPara: false,
  inCode: false,
  inList: false,
}

// ============================================================================
// String Utilities
// ============================================================================

let startsWith = (str, prefix) => {
  Js.String2.startsWith(str, prefix)
}

let trim = str => Js.String2.trim(str)

let stripPrefix = (str, prefix) => {
  if startsWith(str, prefix) {
    Js.String2.substr(str, ~from=Js.String2.length(prefix))
  } else {
    str
  }
}

let escapeHtml = str => {
  str
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
  ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
}

let split = (str, sep) => Js.String2.split(str, sep)

// ============================================================================
// Frontmatter Parser
// ============================================================================

let parseFmLine = (line, fm) => {
  switch Js.String2.indexOf(line, ":") {
  | -1 => fm
  | colonIdx =>
    let key = trim(Js.String2.substring(line, ~from=0, ~to_=colonIdx))
    let value = trim(Js.String2.substr(line, ~from=colonIdx + 1))
    switch key {
    | "title" => {...fm, title: value}
    | "date" => {...fm, date: value}
    | "template" => {...fm, template: value}
    | "draft" => {...fm, draft: value == "true" || value == "yes"}
    | "tags" =>
      let tagStr = if startsWith(value, "[") {
        Js.String2.substring(value, ~from=1, ~to_=Js.String2.length(value) - 1)
      } else {
        value
      }
      let tags = split(tagStr, ",")
        ->Js.Array2.map(trim)
        ->Js.Array2.filter(s => Js.String2.length(s) > 0)
      {...fm, tags}
    | _ => fm
    }
  }
}

let parseFrontmatter = content => {
  let lines = split(content, "\n")
  switch lines[0] {
  | Some(first) if trim(first) == "---" =>
    let rec findEnd = (idx, fm) => {
      switch lines[idx] {
      | None => (fm, "")
      | Some(line) if trim(line) == "---" =>
        let body = lines
          ->Js.Array2.sliceFrom(idx + 1)
          ->Js.Array2.joinWith("\n")
        (fm, body)
      | Some(line) => findEnd(idx + 1, parseFmLine(line, fm))
      }
    }
    findEnd(1, emptyFrontmatter)
  | _ => (emptyFrontmatter, content)
  }
}

// ============================================================================
// Markdown Parser
// ============================================================================

let processInline = text => {
  text
  ->Js.String2.replaceByRe(%re("/\*\*([^*]+)\*\*/g"), "<strong>$1</strong>")
  ->Js.String2.replaceByRe(%re("/\*([^*]+)\*/g"), "<em>$1</em>")
  ->Js.String2.replaceByRe(%re("/`([^`]+)`/g"), "<code>$1</code>")
}

let closePara = st => {
  if st.inPara {
    st.html = st.html ++ "</p>\n"
    st.inPara = false
  }
}

let closeList = st => {
  if st.inList {
    st.html = st.html ++ "</ul>\n"
    st.inList = false
  }
}

let processLine = (line, st) => {
  let tr = trim(line)

  // Code fence
  if startsWith(tr, "```") {
    if st.inCode {
      st.html = st.html ++ "</code></pre>\n"
      st.inCode = false
    } else {
      closePara(st)
      closeList(st)
      st.html = st.html ++ "<pre><code>"
      st.inCode = true
    }
  }
  // Inside code block
  else if st.inCode {
    st.html = st.html ++ escapeHtml(line) ++ "\n"
  }
  // Empty line
  else if tr == "" {
    closePara(st)
    closeList(st)
  }
  // Headers
  else if startsWith(tr, "### ") {
    closePara(st)
    closeList(st)
    let content = processInline(trim(stripPrefix(tr, "### ")))
    st.html = st.html ++ "<h3>" ++ content ++ "</h3>\n"
  } else if startsWith(tr, "## ") {
    closePara(st)
    closeList(st)
    let content = processInline(trim(stripPrefix(tr, "## ")))
    st.html = st.html ++ "<h2>" ++ content ++ "</h2>\n"
  } else if startsWith(tr, "# ") {
    closePara(st)
    closeList(st)
    let content = processInline(trim(stripPrefix(tr, "# ")))
    st.html = st.html ++ "<h1>" ++ content ++ "</h1>\n"
  }
  // List items
  else if startsWith(tr, "- ") || startsWith(tr, "* ") {
    closePara(st)
    if !st.inList {
      st.html = st.html ++ "<ul>\n"
      st.inList = true
    }
    let item = processInline(trim(Js.String2.substr(tr, ~from=2)))
    st.html = st.html ++ "<li>" ++ item ++ "</li>\n"
  }
  // Paragraph
  else {
    if !st.inPara {
      st.html = st.html ++ "<p>"
      st.inPara = true
    } else {
      st.html = st.html ++ " "
    }
    st.html = st.html ++ processInline(tr)
  }
}

let parseMarkdown = content => {
  let st = initState()
  let lines = split(content, "\n")
  lines->Js.Array2.forEach(line => processLine(line, st))
  closePara(st)
  closeList(st)
  if st.inCode {
    st.html = st.html ++ "</code></pre>\n"
  }
  st.html
}

// ============================================================================
// Template Engine
// ============================================================================

let defaultTemplate = `<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>{{title}}</title>
<style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style>
</head><body><article><h1>{{title}}</h1><time>{{date}}</time>
{{content}}
</article></body></html>`

let applyTemplate = (fm, html) => {
  defaultTemplate
  ->Js.String2.replace("{{title}}", fm.title)
  ->Js.String2.replace("{{title}}", fm.title)
  ->Js.String2.replace("{{date}}", fm.date)
  ->Js.String2.replace("{{content}}", html)
}

// ============================================================================
// Tests
// ============================================================================

let testMarkdown = () => {
  Js.log("=== Test: Markdown ===")
  let md = `# Hello World

This is a **bold** test with *italic* text.

- Item 1
- Item 2

\`\`\`
code block
\`\`\`
`
  Js.log(parseMarkdown(md))
}

let testFrontmatter = () => {
  Js.log("=== Test: Frontmatter ===")
  let content = `---
title: My Post
date: 2024-01-15
tags: [rescript, ssg]
draft: false
---

Content here
`
  let (fm, body) = parseFrontmatter(content)
  Js.log("Title: " ++ fm.title)
  Js.log("Date: " ++ fm.date)
  Js.log("Tags: " ++ Js.Array2.joinWith(fm.tags, ", "))
  Js.log("Draft: " ++ (fm.draft ? "true" : "false"))
  Js.log("Body: " ++ body)
}

let testFull = () => {
  Js.log("=== Test: Full Pipeline ===")
  let content = `---
title: Welcome
date: 2024-01-15
---

# Welcome

This is **Rescribe**, a ReScript SSG.

- Type safe
- JavaScript output
- Fast compilation
`
  let (fm, body) = parseFrontmatter(content)
  let html = parseMarkdown(body)
  let output = applyTemplate(fm, html)
  Js.log(output)
}

// ============================================================================
// Main
// ============================================================================

let args = Node.Process.argv

switch args[2] {
| Some("test-markdown") => testMarkdown()
| Some("test-frontmatter") => testFrontmatter()
| Some("test-full") => testFull()
| _ =>
  Js.log("Rescribe SSG - ReScript powered")
  Js.log("Commands: test-markdown test-frontmatter test-full")
}
