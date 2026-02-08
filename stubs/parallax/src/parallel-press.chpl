// parallel-press.chpl - Data-parallel static site generator in Chapel
//
// "ParallelPress" - Massively parallel site generation
//
// Chapel's data parallelism shines here:
// - forall loops distribute work across locales
// - Domains express the shape of our data
// - Reductions aggregate results cleanly
//
// Usage: ./parallel-press --source=content --output=public

module ParallelPress {
  use IO;
  use FileSystem;
  use List;
  use Map;
  use Regex;
  use Time;

  // ============================================================================
  // Configuration
  // ============================================================================

  config const source = "content";
  config const output = "output";
  config const templateDir = "templates";
  config const includeDrafts = false;
  config const verbose = false;

  // ============================================================================
  // Frontmatter Types
  // ============================================================================

  record Frontmatter {
    var title: string;
    var date: string;
    var tags: list(string);
    var draft: bool;
    var template: string;
  }

  proc initFrontmatter(): Frontmatter {
    var fm: Frontmatter;
    fm.title = "";
    fm.date = "";
    fm.tags = new list(string);
    fm.draft = false;
    fm.template = "default";
    return fm;
  }

  // ============================================================================
  // Post Record
  // ============================================================================

  record Post {
    var path: string;
    var frontmatter: Frontmatter;
    var content: string;
    var html: string;
  }

  // ============================================================================
  // String Utilities
  // ============================================================================

  proc trim(s: string): string {
    var start = 0;
    var end = s.size - 1;

    while start <= end && (s[start] == ' ' || s[start] == '\t' || s[start] == '\n' || s[start] == '\r') {
      start += 1;
    }
    while end >= start && (s[end] == ' ' || s[end] == '\t' || s[end] == '\n' || s[end] == '\r') {
      end -= 1;
    }

    if start > end then return "";
    return s[start..end];
  }

  proc startsWith(s: string, prefix: string): bool {
    if prefix.size > s.size then return false;
    return s[0..<prefix.size] == prefix;
  }

  proc stripPrefix(s: string, prefix: string): string {
    if startsWith(s, prefix) {
      return s[prefix.size..];
    }
    return s;
  }

  proc splitLines(s: string): list(string) {
    var lines: list(string);
    var current = "";

    for ch in s {
      if ch == '\n' {
        lines.pushBack(current);
        current = "";
      } else if ch != '\r' {
        current += ch;
      }
    }
    if current.size > 0 {
      lines.pushBack(current);
    }

    return lines;
  }

  proc escapeHtml(s: string): string {
    var result = "";
    for ch in s {
      select ch {
        when '<' do result += "&lt;";
        when '>' do result += "&gt;";
        when '&' do result += "&amp;";
        when '"' do result += "&quot;";
        otherwise do result += ch;
      }
    }
    return result;
  }

  // ============================================================================
  // Frontmatter Parser
  // ============================================================================

  proc parseFrontmatter(content: string): (Frontmatter, string) {
    var fm = initFrontmatter();
    var lines = splitLines(content);

    if lines.size == 0 || trim(lines[0]) != "---" {
      return (fm, content);
    }

    var endIndex = -1;
    for i in 1..<lines.size {
      if trim(lines[i]) == "---" {
        endIndex = i;
        break;
      }
    }

    if endIndex == -1 {
      return (fm, content);
    }

    // Parse YAML-like frontmatter
    for i in 1..<endIndex {
      var line = trim(lines[i]);
      if line.size == 0 then continue;

      var colonPos = line.find(":");
      if colonPos == -1 then continue;

      var key = trim(line[0..<colonPos]);
      var value = trim(line[colonPos+1..]);

      select key {
        when "title" do fm.title = value;
        when "date" do fm.date = value;
        when "template" do fm.template = value;
        when "draft" do fm.draft = (value == "true" || value == "yes");
        when "tags" {
          // Parse [tag1, tag2] or tag1, tag2
          var tagStr = value;
          if startsWith(tagStr, "[") {
            tagStr = tagStr[1..<tagStr.size-1];
          }
          for tag in tagStr.split(",") {
            var t = trim(tag);
            if t.size > 0 then fm.tags.pushBack(t);
          }
        }
      }
    }

    // Build remaining content
    var remaining = "";
    for i in (endIndex+1)..<lines.size {
      if remaining.size > 0 then remaining += "\n";
      remaining += lines[i];
    }

    return (fm, remaining);
  }

  // ============================================================================
  // Markdown Parser
  // ============================================================================

  proc parseMarkdown(content: string): string {
    var lines = splitLines(content);
    var html = "";
    var inParagraph = false;
    var inCodeBlock = false;
    var inList = false;
    var listOrdered = false;

    proc closeParagraph() {
      if inParagraph {
        html += "</p>\n";
        inParagraph = false;
      }
    }

    proc closeList() {
      if inList {
        html += if listOrdered then "</ol>\n" else "</ul>\n";
        inList = false;
      }
    }

    for line in lines {
      var trimmed = trim(line);

      // Code blocks
      if startsWith(trimmed, "```") {
        if inCodeBlock {
          html += "</code></pre>\n";
          inCodeBlock = false;
        } else {
          closeParagraph();
          closeList();
          html += "<pre><code>";
          inCodeBlock = true;
        }
        continue;
      }

      if inCodeBlock {
        html += escapeHtml(line) + "\n";
        continue;
      }

      // Empty line
      if trimmed.size == 0 {
        closeParagraph();
        closeList();
        continue;
      }

      // Headers
      if startsWith(trimmed, "######") {
        closeParagraph();
        closeList();
        html += "<h6>" + processInline(trim(stripPrefix(trimmed, "######"))) + "</h6>\n";
        continue;
      }
      if startsWith(trimmed, "#####") {
        closeParagraph();
        closeList();
        html += "<h5>" + processInline(trim(stripPrefix(trimmed, "#####"))) + "</h5>\n";
        continue;
      }
      if startsWith(trimmed, "####") {
        closeParagraph();
        closeList();
        html += "<h4>" + processInline(trim(stripPrefix(trimmed, "####"))) + "</h4>\n";
        continue;
      }
      if startsWith(trimmed, "###") {
        closeParagraph();
        closeList();
        html += "<h3>" + processInline(trim(stripPrefix(trimmed, "###"))) + "</h3>\n";
        continue;
      }
      if startsWith(trimmed, "##") {
        closeParagraph();
        closeList();
        html += "<h2>" + processInline(trim(stripPrefix(trimmed, "##"))) + "</h2>\n";
        continue;
      }
      if startsWith(trimmed, "#") {
        closeParagraph();
        closeList();
        html += "<h1>" + processInline(trim(stripPrefix(trimmed, "#"))) + "</h1>\n";
        continue;
      }

      // Unordered list
      if startsWith(trimmed, "- ") || startsWith(trimmed, "* ") {
        closeParagraph();
        if !inList || listOrdered {
          closeList();
          html += "<ul>\n";
          inList = true;
          listOrdered = false;
        }
        var itemText = trim(trimmed[2..]);
        html += "<li>" + processInline(itemText) + "</li>\n";
        continue;
      }

      // Ordered list (simple: 1. 2. etc)
      if trimmed.size >= 3 && trimmed[0].isDigit() && trimmed[1] == '.' && trimmed[2] == ' ' {
        closeParagraph();
        if !inList || !listOrdered {
          closeList();
          html += "<ol>\n";
          inList = true;
          listOrdered = true;
        }
        var itemText = trim(trimmed[3..]);
        html += "<li>" + processInline(itemText) + "</li>\n";
        continue;
      }

      // Paragraph
      if !inParagraph {
        html += "<p>";
        inParagraph = true;
      } else {
        html += " ";
      }
      html += processInline(trimmed);
    }

    closeParagraph();
    closeList();
    if inCodeBlock then html += "</code></pre>\n";

    return html;
  }

  // ============================================================================
  // Inline Processing (bold, italic, code)
  // ============================================================================

  proc processInline(text: string): string {
    var result = "";
    var i = 0;
    var n = text.size;

    while i < n {
      // Bold: **text**
      if i + 1 < n && text[i] == '*' && text[i+1] == '*' {
        var endPos = text.find("**", i + 2);
        if endPos != -1 {
          result += "<strong>" + text[i+2..<endPos] + "</strong>";
          i = endPos + 2;
          continue;
        }
      }

      // Italic: *text* or _text_
      if text[i] == '*' || text[i] == '_' {
        var marker = text[i];
        var endPos = -1;
        for j in (i+1)..<n {
          if text[j] == marker {
            endPos = j;
            break;
          }
        }
        if endPos != -1 {
          result += "<em>" + text[i+1..<endPos] + "</em>";
          i = endPos + 1;
          continue;
        }
      }

      // Inline code: `code`
      if text[i] == '`' {
        var endPos = -1;
        for j in (i+1)..<n {
          if text[j] == '`' {
            endPos = j;
            break;
          }
        }
        if endPos != -1 {
          result += "<code>" + escapeHtml(text[i+1..<endPos]) + "</code>";
          i = endPos + 1;
          continue;
        }
      }

      result += text[i];
      i += 1;
    }

    return result;
  }

  // ============================================================================
  // Template Engine
  // ============================================================================

  proc defaultTemplate(): string {
    return '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{{title}}</title>
<style>
body { font-family: system-ui, sans-serif; max-width: 800px; margin: 0 auto; padding: 2rem; line-height: 1.6; }
pre { background: #f4f4f4; padding: 1rem; overflow-x: auto; }
code { background: #f4f4f4; padding: 0.2rem 0.4rem; }
</style>
</head>
<body>
<article>
<h1>{{title}}</h1>
<time>{{date}}</time>
{{content}}
</article>
</body>
</html>';
  }

  proc applyTemplate(post: Post): string {
    var tpl = defaultTemplate();

    // Simple placeholder replacement
    tpl = tpl.replace("{{title}}", post.frontmatter.title);
    tpl = tpl.replace("{{date}}", post.frontmatter.date);
    tpl = tpl.replace("{{content}}", post.html);

    // Tags
    var tagHtml = "";
    for tag in post.frontmatter.tags {
      if tagHtml.size > 0 then tagHtml += ", ";
      tagHtml += tag;
    }
    tpl = tpl.replace("{{tags}}", tagHtml);

    return tpl;
  }

  // ============================================================================
  // File Operations
  // ============================================================================

  proc readFile(path: string): string throws {
    var f = open(path, ioMode.r);
    var reader = f.reader(locking=false);
    var content: string;
    reader.readAll(content);
    reader.close();
    f.close();
    return content;
  }

  proc writeFile(path: string, content: string) throws {
    var f = open(path, ioMode.cw);
    var writer = f.writer(locking=false);
    writer.write(content);
    writer.close();
    f.close();
  }

  proc ensureDir(path: string) throws {
    if !exists(path) {
      mkdir(path, parents=true);
    }
  }

  proc getMarkdownFiles(dir: string): list(string) throws {
    var files: list(string);

    for entry in listDir(dir) {
      var path = dir + "/" + entry;
      if isFile(path) && (entry.endsWith(".md") || entry.endsWith(".markdown")) {
        files.pushBack(path);
      }
    }

    return files;
  }

  // ============================================================================
  // Post Processing
  // ============================================================================

  proc processPost(path: string): Post throws {
    var content = readFile(path);
    var (fm, body) = parseFrontmatter(content);
    var html = parseMarkdown(body);

    var post: Post;
    post.path = path;
    post.frontmatter = fm;
    post.content = body;
    post.html = html;

    return post;
  }

  proc outputPath(inputPath: string): string {
    // content/foo.md -> output/foo.html
    var name = inputPath;

    // Strip source prefix
    if startsWith(name, source) {
      name = name[source.size..];
    }
    if startsWith(name, "/") {
      name = name[1..];
    }

    // Change extension
    if name.endsWith(".md") {
      name = name[0..<name.size-3] + ".html";
    } else if name.endsWith(".markdown") {
      name = name[0..<name.size-9] + ".html";
    }

    return output + "/" + name;
  }

  // ============================================================================
  // Parallel Build
  // ============================================================================

  proc build() throws {
    var timer: stopwatch;
    timer.start();

    writeln("ParallelPress: Building site...");
    writeln("  Source: ", source);
    writeln("  Output: ", output);

    // Get all markdown files
    var files = getMarkdownFiles(source);
    writeln("  Found ", files.size, " posts");

    if files.size == 0 {
      writeln("  No posts to process.");
      return;
    }

    // Ensure output directory exists
    ensureDir(output);

    // Process posts in parallel using forall
    var posts: [0..<files.size] Post;
    var processed: atomic int = 0;
    var skipped: atomic int = 0;

    // Parallel processing - Chapel's strength!
    forall i in 0..<files.size with (ref posts) {
      try {
        var post = processPost(files[i]);

        // Skip drafts unless includeDrafts is set
        if post.frontmatter.draft && !includeDrafts {
          skipped.add(1);
        } else {
          posts[i] = post;

          // Apply template and write output
          var finalHtml = applyTemplate(post);
          var outPath = outputPath(files[i]);

          // Ensure parent directory exists
          var lastSlash = outPath.rfind("/");
          if lastSlash != -1 {
            ensureDir(outPath[0..<lastSlash]);
          }

          writeFile(outPath, finalHtml);
          processed.add(1);

          if verbose {
            writeln("    ", files[i], " -> ", outPath);
          }
        }
      } catch e {
        writeln("  Error processing ", files[i], ": ", e.message());
      }
    }

    timer.stop();

    writeln("  Processed: ", processed.read(), " posts");
    if skipped.read() > 0 {
      writeln("  Skipped: ", skipped.read(), " drafts");
    }
    writeln("  Time: ", timer.elapsed(), " seconds");
    writeln("Build complete!");
  }

  // ============================================================================
  // Tests
  // ============================================================================

  proc testMarkdown() {
    writeln("=== Test: Markdown ===");

    var md = "# Hello World

This is a **bold** test with *italic* text.

- Item 1
- Item 2

```
code block
```";

    var html = parseMarkdown(md);
    writeln(html);
  }

  proc testFrontmatter() {
    writeln("=== Test: Frontmatter ===");

    var content = "---
title: My Post
date: 2024-01-15
tags: [chapel, ssg]
draft: false
---

Content here";

    var (fm, body) = parseFrontmatter(content);
    writeln("Title: ", fm.title);
    writeln("Date: ", fm.date);
    writeln("Tags: ", fm.tags);
    writeln("Draft: ", fm.draft);
    writeln("Body: ", body);
  }

  proc testFull() {
    writeln("=== Test: Full Pipeline ===");

    var content = "---
title: Welcome
date: 2024-01-15
---

# Welcome

This is **ParallelPress**, a Chapel-powered SSG.

- Data-parallel
- Fast
- Elegant
";

    var (fm, body) = parseFrontmatter(content);
    var html = parseMarkdown(body);

    var post: Post;
    post.frontmatter = fm;
    post.html = html;

    var output = applyTemplate(post);
    writeln(output);
  }

  // ============================================================================
  // Main
  // ============================================================================

  proc main(args: [] string) {
    if args.size > 1 {
      select args[1] {
        when "test-markdown" do testMarkdown();
        when "test-frontmatter" do testFrontmatter();
        when "test-full" do testFull();
        when "build" {
          try {
            build();
          } catch e {
            writeln("Build failed: ", e.message());
          }
        }
        otherwise {
          writeln("ParallelPress - Chapel Static Site Generator");
          writeln("Usage: parallel-press [command]");
          writeln("Commands:");
          writeln("  build            Build the site");
          writeln("  test-markdown    Test markdown parser");
          writeln("  test-frontmatter Test frontmatter parser");
          writeln("  test-full        Test full pipeline");
          writeln("Options:");
          writeln("  --source=DIR     Source directory (default: content)");
          writeln("  --output=DIR     Output directory (default: output)");
          writeln("  --includeDrafts  Include draft posts");
          writeln("  --verbose        Verbose output");
        }
      }
    } else {
      writeln("ParallelPress SSG loaded.");
      writeln("Commands: test-markdown test-frontmatter test-full build");
    }
  }
}
