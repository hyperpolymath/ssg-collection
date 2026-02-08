// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//
// eclipse_ssg.pony - Pony-powered static site generator
//
// "Eclipse" - Actor-model site generation with reference capabilities
//
// Pony's actors enable lock-free concurrent processing while
// reference capabilities guarantee memory safety without GC pauses.

use "files"
use "collections"

// ============================================================================
// Types
// ============================================================================

class val Frontmatter
  let title: String
  let date: String
  let tags: Array[String] val
  let draft: Bool

  new val create(
    title': String = "",
    date': String = "",
    tags': Array[String] val = recover val Array[String] end,
    draft': Bool = false
  ) =>
    title = title'
    date = date'
    tags = tags'
    draft = draft'

class val SiteConfig
  let content_dir: String
  let output_dir: String
  let template_dir: String
  let static_dir: String
  let site_title: String

  new val create(
    content_dir': String = "content",
    output_dir': String = "public",
    template_dir': String = "templates",
    static_dir': String = "static",
    site_title': String = "My Site"
  ) =>
    content_dir = content_dir'
    output_dir = output_dir'
    template_dir = template_dir'
    static_dir = static_dir'
    site_title = site_title'

class val ParseResult
  let frontmatter: Frontmatter
  let body: String

  new val create(frontmatter': Frontmatter, body': String) =>
    frontmatter = frontmatter'
    body = body'

// ============================================================================
// String Utilities
// ============================================================================

primitive StringUtils
  fun trim(s: String): String =>
    s.clone().>strip()

  fun starts_with(s: String, prefix: String): Bool =>
    if prefix.size() > s.size() then
      false
    else
      try
        s.substring(0, prefix.size().isize()) == prefix
      else
        false
      end
    end

  fun drop_prefix(s: String, prefix: String): String =>
    if starts_with(s, prefix) then
      try
        s.substring(prefix.size().isize())
      else
        s
      end
    else
      s
    end

  fun split_lines(s: String): Array[String] iso^ =>
    let lines = recover iso Array[String] end
    var current = recover iso String end
    for c in s.values() do
      if c == '\n' then
        lines.push(current = recover iso String end)
      else
        current.push(c)
      end
    end
    if current.size() > 0 then
      lines.push(consume current)
    end
    consume lines

  fun join_lines(lines: Array[String] box): String =>
    let result = recover iso String end
    var first = true
    for line in lines.values() do
      if not first then
        result.append("\n")
      end
      result.append(line)
      first = false
    end
    consume result

  fun escape_html(s: String): String =>
    let result = recover iso String end
    for c in s.values() do
      match c
      | '<' => result.append("&lt;")
      | '>' => result.append("&gt;")
      | '&' => result.append("&amp;")
      | '"' => result.append("&quot;")
      else
        result.push(c)
      end
    end
    consume result

// ============================================================================
// Frontmatter Parser
// ============================================================================

primitive FrontmatterParser
  fun parse(content: String): ParseResult =>
    let lines = StringUtils.split_lines(content)
    if lines.size() == 0 then
      return ParseResult(Frontmatter, content)
    end

    try
      let first = StringUtils.trim(lines(0)?)
      if first != "---" then
        return ParseResult(Frontmatter, content)
      end

      var title: String = ""
      var date: String = ""
      var draft: Bool = false
      var body_start: USize = 1

      for i in Range(1, lines.size()) do
        let line = StringUtils.trim(lines(i)?)
        if line == "---" then
          body_start = i + 1
          break
        end

        // Parse key: value
        try
          let colon = line.find(":")?
          let key = StringUtils.trim(line.substring(0, colon))
          let value = StringUtils.trim(line.substring(colon + 1))

          match key
          | "title" => title = value
          | "date" => date = value
          | "draft" => draft = (value == "true") or (value == "yes")
          end
        end
      end

      // Collect body
      let body_lines = recover iso Array[String] end
      for i in Range(body_start, lines.size()) do
        body_lines.push(lines(i)?)
      end
      let body = StringUtils.join_lines(consume body_lines)

      ParseResult(Frontmatter(title, date, recover val Array[String] end, draft), body)
    else
      ParseResult(Frontmatter, content)
    end

// ============================================================================
// Markdown Parser
// ============================================================================

primitive MarkdownParser
  fun parse(content: String): String =>
    let lines = StringUtils.split_lines(content)
    let result = recover iso String end
    var in_para: Bool = false
    var in_code: Bool = false
    var in_list: Bool = false

    for line in lines.values() do
      let trimmed = StringUtils.trim(line)

      if StringUtils.starts_with(trimmed, "```") then
        // Code fence toggle
        if in_code then
          result.append("</code></pre>\n")
          in_code = false
        else
          if in_para then
            result.append("</p>\n")
            in_para = false
          end
          if in_list then
            result.append("</ul>\n")
            in_list = false
          end
          result.append("<pre><code>")
          in_code = true
        end
      elseif in_code then
        result.append(StringUtils.escape_html(line))
        result.append("\n")
      elseif trimmed.size() == 0 then
        // Empty line
        if in_para then
          result.append("</p>\n")
          in_para = false
        end
        if in_list then
          result.append("</ul>\n")
          in_list = false
        end
      elseif StringUtils.starts_with(trimmed, "### ") then
        _close_blocks(result, in_para = false, in_list = false)
        let text = StringUtils.drop_prefix(trimmed, "### ")
        result.append("<h3>")
        result.append(text)
        result.append("</h3>\n")
      elseif StringUtils.starts_with(trimmed, "## ") then
        _close_blocks(result, in_para = false, in_list = false)
        let text = StringUtils.drop_prefix(trimmed, "## ")
        result.append("<h2>")
        result.append(text)
        result.append("</h2>\n")
      elseif StringUtils.starts_with(trimmed, "# ") then
        _close_blocks(result, in_para = false, in_list = false)
        let text = StringUtils.drop_prefix(trimmed, "# ")
        result.append("<h1>")
        result.append(text)
        result.append("</h1>\n")
      elseif StringUtils.starts_with(trimmed, "- ") or StringUtils.starts_with(trimmed, "* ") then
        if in_para then
          result.append("</p>\n")
          in_para = false
        end
        if not in_list then
          result.append("<ul>\n")
          in_list = true
        end
        let text = try trimmed.substring(2) else "" end
        result.append("<li>")
        result.append(text)
        result.append("</li>\n")
      else
        // Paragraph
        if in_list then
          result.append("</ul>\n")
          in_list = false
        end
        if not in_para then
          result.append("<p>")
          in_para = true
        else
          result.append(" ")
        end
        result.append(trimmed)
      end
    end

    // Close any open tags
    if in_para then
      result.append("</p>\n")
    end
    if in_list then
      result.append("</ul>\n")
    end
    if in_code then
      result.append("</code></pre>\n")
    end

    consume result

  fun _close_blocks(result: String iso, in_para: Bool, in_list: Bool) =>
    if in_para then
      result.append("</p>\n")
    end
    if in_list then
      result.append("</ul>\n")
    end

// ============================================================================
// Template Engine
// ============================================================================

primitive TemplateEngine
  fun apply(fm: Frontmatter, html: String): String =>
    let template = """<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>{{title}}</title>
<style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style>
</head>
<body>
<article>
<h1>{{title}}</h1>
<time>{{date}}</time>
{{content}}
</article>
</body>
</html>"""

    let result = template.clone()
    try
      while result.find("{{title}}")? >= 0 do
        let pos = result.find("{{title}}")?
        result.delete(pos, pos + 9)
        result.insert_in_place(pos, fm.title)
      end
    end
    try
      while result.find("{{date}}")? >= 0 do
        let pos = result.find("{{date}}")?
        result.delete(pos, pos + 8)
        result.insert_in_place(pos, fm.date)
      end
    end
    try
      while result.find("{{content}}")? >= 0 do
        let pos = result.find("{{content}}")?
        result.delete(pos, pos + 11)
        result.insert_in_place(pos, html)
      end
    end
    consume result

// ============================================================================
// File Operations Actor
// ============================================================================

actor FileEmitter
  let _env: Env

  new create(env: Env) =>
    _env = env

  be write_file(path: String, content: String) =>
    try
      let file_path = FilePath(_env.root as AmbientAuth, path)?
      let file = CreateFile(file_path) as File
      file.write(content)
      file.dispose()
      _env.out.print("  Written: " + path)
    else
      _env.err.print("  Error writing: " + path)
    end

// ============================================================================
// Page Builder Actor
// ============================================================================

actor PageBuilder
  let _env: Env
  let _emitter: FileEmitter

  new create(env: Env, emitter: FileEmitter) =>
    _env = env
    _emitter = emitter

  be build(src_path: String, out_path: String, content: String) =>
    let result = FrontmatterParser.parse(content)
    let html = MarkdownParser.parse(result.body)
    let output = TemplateEngine(result.frontmatter, html)
    _emitter.write_file(out_path, output)
    _env.out.print("  " + src_path + " -> " + out_path)

// ============================================================================
// Site Builder Actor
// ============================================================================

actor SiteBuilder
  let _env: Env
  let _config: SiteConfig
  let _emitter: FileEmitter
  var _count: USize = 0

  new create(env: Env, config: SiteConfig) =>
    _env = env
    _config = config
    _emitter = FileEmitter(env)

  be build() =>
    _env.out.print("Building site...")

    try
      let auth = _env.root as AmbientAuth
      let content_path = FilePath(auth, _config.content_dir)?
      let output_path = FilePath(auth, _config.output_dir)?

      // Create output directory
      output_path.mkdir()

      // Process markdown files
      if content_path.exists() then
        let entries = content_path.entries()?
        for entry in entries.values() do
          if entry.path.ends_with(".md") then
            _process_file(entry.path, auth)
          end
        end
      else
        _env.out.print("No content directory found. Creating " + _config.content_dir + "/")
        content_path.mkdir()
      end

      _env.out.print("\nBuild complete")
    else
      _env.err.print("Build failed")
    end

  fun ref _process_file(filename: String, auth: AmbientAuth) =>
    try
      let src_path = _config.content_dir + "/" + filename
      let file_path = FilePath(auth, src_path)?
      let file = OpenFile(file_path) as File
      let content = file.read_string(file.size())
      file.dispose()

      let out_name = filename.clone()
      try
        let dot_pos = out_name.rfind(".")?
        out_name.delete(dot_pos, out_name.size().isize())
      end
      out_name.append(".html")
      let out_path = _config.output_dir + "/" + consume out_name

      let builder = PageBuilder(_env, _emitter)
      builder.build(src_path, out_path, consume content)
      _count = _count + 1
    end

// ============================================================================
// Main Actor
// ============================================================================

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    let args = env.args

    if args.size() < 2 then
      _print_usage()
      return
    end

    try
      let cmd = args(1)?
      match cmd
      | "build" => _build()
      | "init" =>
        if args.size() > 2 then
          _init(args(2)?)
        else
          env.err.print("Error: 'init' requires a site name")
          env.err.print("Usage: eclipse-ssg init <name>")
        end
      | "clean" => _clean()
      | "--help" => _print_usage()
      | "-h" => _print_usage()
      else
        env.err.print("Unknown command: " + cmd)
        env.err.print("Run 'eclipse-ssg --help' for usage.")
      end
    end

  fun ref _build() =>
    let config = SiteConfig
    let builder = SiteBuilder(_env, config)
    builder.build()

  fun ref _init(name: String) =>
    _env.out.print("Initializing new site: " + name)

    try
      let auth = _env.root as AmbientAuth

      // Create directories
      let dirs = [
        name + "/content"
        name + "/templates"
        name + "/static"
        name + "/public"
      ]

      for dir in dirs.values() do
        let dir_path = FilePath(auth, dir)?
        dir_path.mkdir()
        _env.out.print("  Created " + dir + "/")
      end

      // Create sample content
      let sample = """---
title: Welcome to Eclipse
date: 2025-01-18
---

# Welcome

This is your first post built with **Eclipse SSG**.

## Features

- Actor-model concurrency
- Reference capability safety
- Lock-free parallel builds

```pony
actor Main
  new create(env: Env) =>
    env.out.print("Hello, Eclipse!")
```
"""
      let content_path = FilePath(auth, name + "/content/index.md")?
      let file = CreateFile(content_path) as File
      file.write(sample)
      file.dispose()
      _env.out.print("  Created sample content")

      _env.out.print("\nSite initialized! Run 'eclipse-ssg build' in " + name + "/ to build.")
    else
      _env.err.print("Error: Failed to create site")
    end

  fun ref _clean() =>
    let config = SiteConfig
    _env.out.print("Cleaning " + config.output_dir + "/...")

    try
      let auth = _env.root as AmbientAuth
      let output_path = FilePath(auth, config.output_dir)?
      if output_path.exists() then
        output_path.remove()
      end
      _env.out.print("Clean complete.")
    else
      _env.out.print("Clean complete.")
    end

  fun _print_usage() =>
    _env.out.print("Eclipse SSG - Pony-powered static site generator")
    _env.out.print("")
    _env.out.print("USAGE:")
    _env.out.print("  eclipse-ssg <command> [options]")
    _env.out.print("")
    _env.out.print("COMMANDS:")
    _env.out.print("  build              Build the site")
    _env.out.print("  init <name>        Create a new site")
    _env.out.print("  clean              Remove generated files")
    _env.out.print("")
    _env.out.print("OPTIONS:")
    _env.out.print("  -h, --help         Show this help")
    _env.out.print("")
    _env.out.print("EXAMPLES:")
    _env.out.print("  eclipse-ssg init my-blog")
    _env.out.print("  cd my-blog && eclipse-ssg build")
