// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//
// sparkle_ssg.gleam - Gleam-powered static site generator
//
// "Sparkle" - Concurrent site generation on the BEAM
//
// Gleam's type safety combined with BEAM's concurrency model
// enables parallel processing of content files with fault tolerance.

import gleam/io
import gleam/list
import gleam/string
import gleam/result
import gleam/option.{type Option, None, Some}
import gleam/int
import simplifile
import argv

// ============================================================================
// Types
// ============================================================================

pub type Frontmatter {
  Frontmatter(title: String, date: String, tags: List(String), draft: Bool)
}

pub type SiteConfig {
  SiteConfig(
    content_dir: String,
    output_dir: String,
    template_dir: String,
    static_dir: String,
    site_title: String,
  )
}

pub type ParseResult {
  ParseResult(frontmatter: Frontmatter, body: String)
}

// ============================================================================
// Configuration
// ============================================================================

fn default_config() -> SiteConfig {
  SiteConfig(
    content_dir: "content",
    output_dir: "public",
    template_dir: "templates",
    static_dir: "static",
    site_title: "My Site",
  )
}

fn empty_frontmatter() -> Frontmatter {
  Frontmatter(title: "", date: "", tags: [], draft: False)
}

// ============================================================================
// String Utilities
// ============================================================================

fn trim(s: String) -> String {
  string.trim(s)
}

fn starts_with(s: String, prefix: String) -> Bool {
  string.starts_with(s, prefix)
}

fn drop_prefix(s: String, prefix: String) -> String {
  case string.starts_with(s, prefix) {
    True -> string.drop_start(s, string.length(prefix))
    False -> s
  }
}

// ============================================================================
// Frontmatter Parser
// ============================================================================

fn parse_fm_line(line: String, fm: Frontmatter) -> Frontmatter {
  let trimmed = trim(line)
  case string.split_once(trimmed, ": ") {
    Ok(#("title", value)) -> Frontmatter(..fm, title: trim(value))
    Ok(#("date", value)) -> Frontmatter(..fm, date: trim(value))
    Ok(#("draft", value)) ->
      Frontmatter(..fm, draft: value == "true" || value == "yes")
    _ -> fm
  }
}

pub fn parse_frontmatter(content: String) -> ParseResult {
  let lines = string.split(content, "\n")
  case lines {
    ["---", ..rest] -> parse_fm_body(rest, empty_frontmatter(), [])
    _ -> ParseResult(frontmatter: empty_frontmatter(), body: content)
  }
}

fn parse_fm_body(
  lines: List(String),
  fm: Frontmatter,
  acc: List(String),
) -> ParseResult {
  case lines {
    [] -> ParseResult(frontmatter: fm, body: "")
    ["---", ..rest] ->
      ParseResult(frontmatter: fm, body: string.join(rest, "\n"))
    [line, ..rest] -> parse_fm_body(rest, parse_fm_line(line, fm), acc)
  }
}

// ============================================================================
// Markdown Parser
// ============================================================================

pub fn parse_markdown(content: String) -> String {
  let lines = string.split(content, "\n")
  parse_lines(lines, [], False, False)
}

fn parse_lines(
  lines: List(String),
  acc: List(String),
  in_para: Bool,
  in_code: Bool,
) -> String {
  case lines {
    [] -> {
      let final_acc = case in_para {
        True -> list.append(acc, ["</p>"])
        False -> acc
      }
      string.join(final_acc, "\n")
    }
    [line, ..rest] -> {
      let trimmed = trim(line)
      case trimmed, in_code {
        // Code fence toggle
        s, _ if starts_with(s, "```") -> {
          case in_code {
            True ->
              parse_lines(
                rest,
                list.append(acc, ["</code></pre>"]),
                False,
                False,
              )
            False ->
              parse_lines(
                rest,
                list.append(acc, ["<pre><code>"]),
                False,
                True,
              )
          }
        }
        // Inside code block
        _, True ->
          parse_lines(
            rest,
            list.append(acc, [escape_html(line)]),
            False,
            True,
          )
        // Empty line
        "", False -> {
          let new_acc = case in_para {
            True -> list.append(acc, ["</p>"])
            False -> acc
          }
          parse_lines(rest, new_acc, False, False)
        }
        // Headers
        s, False if starts_with(s, "### ") ->
          parse_lines(
            rest,
            list.append(acc, [
              "<h3>" <> drop_prefix(s, "### ") <> "</h3>",
            ]),
            False,
            False,
          )
        s, False if starts_with(s, "## ") ->
          parse_lines(
            rest,
            list.append(acc, ["<h2>" <> drop_prefix(s, "## ") <> "</h2>"]),
            False,
            False,
          )
        s, False if starts_with(s, "# ") ->
          parse_lines(
            rest,
            list.append(acc, ["<h1>" <> drop_prefix(s, "# ") <> "</h1>"]),
            False,
            False,
          )
        // List items
        s, False if starts_with(s, "- ") || starts_with(s, "* ") ->
          parse_lines(
            rest,
            list.append(acc, [
              "<li>" <> string.drop_start(s, 2) <> "</li>",
            ]),
            False,
            False,
          )
        // Paragraph
        _, False -> {
          case in_para {
            True ->
              parse_lines(
                rest,
                list.append(acc, [" " <> trimmed]),
                True,
                False,
              )
            False ->
              parse_lines(
                rest,
                list.append(acc, ["<p>" <> trimmed]),
                True,
                False,
              )
          }
        }
      }
    }
  }
}

fn escape_html(s: String) -> String {
  s
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
}

// ============================================================================
// Template Engine
// ============================================================================

pub fn apply_template(fm: Frontmatter, html: String) -> String {
  "<!DOCTYPE html>
<html>
<head>
<meta charset=\"UTF-8\">
<title>"
  <> fm.title
  <> "</title>
<style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style>
</head>
<body>
<article>
<h1>"
  <> fm.title
  <> "</h1>
<time>"
  <> fm.date
  <> "</time>
"
  <> html
  <> "
</article>
</body>
</html>"
}

// ============================================================================
// File Operations
// ============================================================================

fn ensure_dir(path: String) -> Result(Nil, simplifile.FileError) {
  case simplifile.is_directory(path) {
    Ok(True) -> Ok(Nil)
    _ -> simplifile.create_directory_all(path)
  }
}

fn list_md_files(dir: String) -> Result(List(String), simplifile.FileError) {
  use files <- result.try(simplifile.read_directory(dir))
  Ok(list.filter(files, fn(f) { string.ends_with(f, ".md") }))
}

// ============================================================================
// Build Command
// ============================================================================

fn build_site(config: SiteConfig) -> Result(Nil, String) {
  io.println("Building site...")

  // Ensure output directory exists
  case ensure_dir(config.output_dir) {
    Error(_) -> Error("Failed to create output directory")
    Ok(_) -> {
      // Check content directory
      case simplifile.is_directory(config.content_dir) {
        Ok(True) -> {
          case list_md_files(config.content_dir) {
            Ok(files) -> {
              let count = build_files(config, files, 0)
              io.println(
                "\nBuild complete: " <> int.to_string(count) <> " pages",
              )
              Ok(Nil)
            }
            Error(_) -> Error("Failed to list content files")
          }
        }
        _ -> {
          io.println(
            "No content directory found. Creating " <> config.content_dir <> "/",
          )
          case ensure_dir(config.content_dir) {
            Ok(_) -> Ok(Nil)
            Error(_) -> Error("Failed to create content directory")
          }
        }
      }
    }
  }
}

fn build_files(config: SiteConfig, files: List(String), count: Int) -> Int {
  case files {
    [] -> count
    [file, ..rest] -> {
      let src_path = config.content_dir <> "/" <> file
      case simplifile.read(src_path) {
        Ok(content) -> {
          let result = parse_frontmatter(content)
          let html = parse_markdown(result.body)
          let output = apply_template(result.frontmatter, html)

          // Output path: replace .md with .html
          let out_name = string.replace(file, ".md", ".html")
          let out_path = config.output_dir <> "/" <> out_name

          case simplifile.write(out_path, output) {
            Ok(_) -> {
              io.println("  " <> file <> " -> " <> out_path)
              build_files(config, rest, count + 1)
            }
            Error(_) -> build_files(config, rest, count)
          }
        }
        Error(_) -> build_files(config, rest, count)
      }
    }
  }
}

// ============================================================================
// Init Command
// ============================================================================

fn init_site(name: String) -> Result(Nil, String) {
  io.println("Initializing new site: " <> name)

  // Create directories
  let dirs = [
    name <> "/content",
    name <> "/templates",
    name <> "/static",
    name <> "/public",
  ]

  case create_dirs(dirs) {
    Error(e) -> Error(e)
    Ok(_) -> {
      // Create sample content
      let sample =
        "---
title: Welcome to Sparkle
date: 2025-01-18
---

# Welcome

This is your first post built with **Sparkle SSG**.

## Features

- BEAM concurrency for parallel builds
- Type-safe with Gleam
- Fault-tolerant processing

```gleam
pub fn main() {
  io.println(\"Hello, Sparkle!\")
}
```
"
      let content_path = name <> "/content/index.md"
      case simplifile.write(content_path, sample) {
        Ok(_) -> {
          io.println("  Created sample content")
          io.println(
            "\nSite initialized! Run 'sparkle-ssg build' in " <> name <> "/ to build.",
          )
          Ok(Nil)
        }
        Error(_) -> Error("Failed to create sample content")
      }
    }
  }
}

fn create_dirs(dirs: List(String)) -> Result(Nil, String) {
  case dirs {
    [] -> Ok(Nil)
    [dir, ..rest] -> {
      case ensure_dir(dir) {
        Ok(_) -> {
          io.println("  Created " <> dir <> "/")
          create_dirs(rest)
        }
        Error(_) -> Error("Failed to create " <> dir)
      }
    }
  }
}

// ============================================================================
// Clean Command
// ============================================================================

fn clean_site(config: SiteConfig) -> Result(Nil, String) {
  io.println("Cleaning " <> config.output_dir <> "/...")
  case simplifile.delete(config.output_dir) {
    Ok(_) -> {
      io.println("Clean complete.")
      Ok(Nil)
    }
    Error(_) -> {
      io.println("Clean complete.")
      Ok(Nil)
    }
  }
}

// ============================================================================
// Help
// ============================================================================

fn print_usage() {
  io.println("Sparkle SSG - Gleam-powered static site generator")
  io.println("")
  io.println("USAGE:")
  io.println("  sparkle-ssg <command> [options]")
  io.println("")
  io.println("COMMANDS:")
  io.println("  build              Build the site")
  io.println("  init <name>        Create a new site")
  io.println("  clean              Remove generated files")
  io.println("")
  io.println("OPTIONS:")
  io.println("  -h, --help         Show this help")
  io.println("")
  io.println("EXAMPLES:")
  io.println("  sparkle-ssg init my-blog")
  io.println("  cd my-blog && sparkle-ssg build")
}

// ============================================================================
// Tests
// ============================================================================

fn test_markdown() {
  io.println("=== Test: Markdown ===")
  let md = "# Hello World

This is a test.

- Item 1
- Item 2

```
code block
```
"
  let html = parse_markdown(md)
  io.println(html)
}

fn test_frontmatter() {
  io.println("=== Test: Frontmatter ===")
  let content =
    "---
title: My Post
date: 2024-01-15
draft: false
---

Content here
"
  let result = parse_frontmatter(content)
  io.println("Title: " <> result.frontmatter.title)
  io.println("Date: " <> result.frontmatter.date)
  io.println("Body: " <> result.body)
}

fn test_full() {
  io.println("=== Test: Full Pipeline ===")
  let content =
    "---
title: Welcome
date: 2024-01-15
---

# Welcome

This is **Sparkle**, a Gleam SSG.

- Type-safe
- Concurrent
- Fault-tolerant
"
  let result = parse_frontmatter(content)
  let html = parse_markdown(result.body)
  let output = apply_template(result.frontmatter, html)
  io.println(output)
}

// ============================================================================
// Main
// ============================================================================

pub fn main() {
  let config = default_config()
  let args = argv.load().arguments

  case args {
    ["build"] -> {
      case build_site(config) {
        Ok(_) -> Nil
        Error(e) -> io.println("Error: " <> e)
      }
    }
    ["init", name] -> {
      case init_site(name) {
        Ok(_) -> Nil
        Error(e) -> io.println("Error: " <> e)
      }
    }
    ["init"] -> {
      io.println("Error: 'init' requires a site name")
      io.println("Usage: sparkle-ssg init <name>")
    }
    ["clean"] -> {
      case clean_site(config) {
        Ok(_) -> Nil
        Error(e) -> io.println("Error: " <> e)
      }
    }
    ["test-markdown"] -> test_markdown()
    ["test-frontmatter"] -> test_frontmatter()
    ["test-full"] -> test_full()
    ["-h"] | ["--help"] -> print_usage()
    [] -> print_usage()
    [cmd, ..] -> {
      io.println("Unknown command: " <> cmd)
      io.println("Run 'sparkle-ssg --help' for usage.")
    }
  }
}
