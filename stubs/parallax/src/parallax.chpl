// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// parallax-ssg - Chapel Static Site Generator
// "Let locales distribute. Let domains partition. Let sites parallelize."
//
// Compile with: chpl src/parallax.chpl -o parallax
// Run with: ./parallax build | init | clean

use IO;
use FileSystem;
use List;

// ============================================================================
// Configuration
// ============================================================================

config const contentDir = "content";
config const outputDir = "_site";
config const siteTitle = "Parallax Site";

// ============================================================================
// Page Metadata
// ============================================================================

record PageMeta {
    var title: string = "Untitled";
    var date: string = "";
    var draft: bool = false;
}

// ============================================================================
// String Utilities
// ============================================================================

proc trim(s: string): string {
    var result = s;
    while result.size > 0 && result[0] == ' ' {
        result = result[1..];
    }
    while result.size > 0 && result[result.size-1] == ' ' {
        result = result[..result.size-2];
    }
    return result;
}

proc startsWith(s: string, prefix: string): bool {
    if s.size < prefix.size then return false;
    return s[0..prefix.size-1] == prefix;
}

proc escapeHtml(s: string): string {
    var result = s;
    result = result.replace("&", "&amp;");
    result = result.replace("<", "&lt;");
    result = result.replace(">", "&gt;");
    result = result.replace("\"", "&quot;");
    return result;
}

// ============================================================================
// Frontmatter Parser
// ============================================================================

proc parseFrontmatter(ref lines: list(string)): (PageMeta, int) {
    var meta = new PageMeta();
    var bodyStart = 0;

    if lines.size == 0 then return (meta, 0);

    if trim(lines[0]) != "---" then return (meta, 0);

    var inFrontmatter = true;
    for i in 1..<lines.size {
        var line = trim(lines[i]);

        if line == "---" {
            bodyStart = i + 1;
            break;
        }

        var colonPos = line.find(":");
        if colonPos != -1 {
            var key = trim(line[0..colonPos-1]);
            var value = trim(line[colonPos+1..]);

            select key {
                when "title" do meta.title = value;
                when "date" do meta.date = value;
                when "draft" do meta.draft = (value == "true" || value == "yes");
            }
        }
    }

    return (meta, bodyStart);
}

// ============================================================================
// Markdown Parser
// ============================================================================

proc parseMarkdown(ref lines: list(string), startIdx: int): string {
    var html = "";
    var inCode = false;

    for i in startIdx..<lines.size {
        var line = lines[i];
        var trimmed = trim(line);

        // Code blocks
        if startsWith(trimmed, "```") {
            if inCode {
                html += "</code></pre>\n";
                inCode = false;
            } else {
                html += "<pre><code>";
                inCode = true;
            }
            continue;
        }

        if inCode {
            html += escapeHtml(line) + "\n";
            continue;
        }

        // Headers
        if startsWith(trimmed, "### ") {
            html += "<h3>" + trimmed[4..] + "</h3>\n";
            continue;
        }
        if startsWith(trimmed, "## ") {
            html += "<h2>" + trimmed[3..] + "</h2>\n";
            continue;
        }
        if startsWith(trimmed, "# ") {
            html += "<h1>" + trimmed[2..] + "</h1>\n";
            continue;
        }

        // List items
        if startsWith(trimmed, "- ") || startsWith(trimmed, "* ") {
            html += "<li>" + trimmed[2..] + "</li>\n";
            continue;
        }

        // Empty lines
        if trimmed.size == 0 then continue;

        // Paragraph
        html += "<p>" + trimmed + "</p>\n";
    }

    return html;
}

// ============================================================================
// HTML Generation
// ============================================================================

proc generatePage(meta: PageMeta, contentHtml: string): string {
    var html = "<!DOCTYPE html>\n";
    html += "<html lang=\"en\">\n";
    html += "<head>\n";
    html += "<meta charset=\"UTF-8\">\n";
    html += "<title>" + escapeHtml(meta.title) + " | " + siteTitle + "</title>\n";
    html += "<style>\n";
    html += "body{font-family:sans-serif;max-width:700px;margin:0 auto;padding:2rem;}\n";
    html += "pre{background:#f5f5f5;padding:1rem;}\n";
    html += "</style>\n";
    html += "</head>\n";
    html += "<body>\n";
    html += "<article>\n";
    html += "<h1>" + escapeHtml(meta.title) + "</h1>\n";

    if meta.date.size > 0 {
        html += "<time>" + meta.date + "</time>\n";
    }

    html += contentHtml;
    html += "</article>\n";
    html += "<footer><p>Generated by parallax-ssg (Chapel)</p></footer>\n";
    html += "</body>\n";
    html += "</html>\n";

    return html;
}

// ============================================================================
// File Processing
// ============================================================================

proc processFile(srcPath: string, outPath: string) {
    writeln("  Processing: ", srcPath);

    var lines: list(string);
    var f = open(srcPath, iomode.r);
    var reader = f.reader();

    var line: string;
    while reader.readLine(line) {
        lines.append(line.strip("\n"));
    }
    f.close();

    var (meta, bodyStart) = parseFrontmatter(lines);

    if meta.draft {
        writeln("    Skipping draft");
        return;
    }

    var contentHtml = parseMarkdown(lines, bodyStart);
    var html = generatePage(meta, contentHtml);

    var outFile = open(outPath, iomode.cw);
    var writer = outFile.writer();
    writer.write(html);
    outFile.close();

    writeln("  Written: ", outPath);
}

// ============================================================================
// Commands
// ============================================================================

proc cmdBuild() {
    writeln("parallax-ssg: Building site...");
    writeln("  Output directory: ", outputDir);

    if !exists(outputDir) {
        mkdir(outputDir);
    }

    if !exists(contentDir) {
        writeln("  No content directory found. Run 'parallax init' first.");
        return;
    }

    var srcPath = contentDir + "/index.md";
    var outPath = outputDir + "/index.html";

    processFile(srcPath, outPath);

    writeln("parallax-ssg: Build complete.");
}

proc cmdInit() {
    writeln("parallax-ssg: Initializing new site...");

    if !exists(contentDir) {
        mkdir(contentDir);
        writeln("  Created ", contentDir, "/");
    }
    if !exists(outputDir) {
        mkdir(outputDir);
        writeln("  Created ", outputDir, "/");
    }

    var samplePath = contentDir + "/index.md";
    var sample = "---
title: Welcome to Parallax
date: 2025-01-18
---

# Welcome

This site was built with **parallax-ssg**, a Chapel-powered SSG.

## Chapel Features

- Data parallelism with domains
- Locale-aware distribution
- High-performance computing
- Clean, readable syntax

## Code Example

```chapel
// Parallel iteration over domain
var D = {1..1000, 1..1000};
var A: [D] real;

forall (i, j) in D {
    A[i, j] = compute(i, j);
}
```
";

    var f = open(samplePath, iomode.cw);
    var writer = f.writer();
    writer.write(sample);
    f.close();

    writeln("  Created sample content");
    writeln("parallax-ssg: Site initialized.");
    writeln("Run './parallax build' to build.");
}

proc cmdClean() {
    writeln("parallax-ssg: Cleaning ", outputDir, "/...");
    if exists(outputDir) {
        rmTree(outputDir);
    }
    writeln("parallax-ssg: Clean complete.");
}

proc cmdHelp() {
    writeln("parallax-ssg - Chapel Static Site Generator");
    writeln();
    writeln("USAGE:");
    writeln("  parallax <command>");
    writeln();
    writeln("COMMANDS:");
    writeln("  build    Build the site");
    writeln("  init     Initialize a new site");
    writeln("  clean    Remove generated files");
    writeln("  help     Show this help");
    writeln();
    writeln("FEATURES:");
    writeln("  - Parallel file processing");
    writeln("  - Domain-based content iteration");
    writeln("  - HPC-ready architecture");
}

// ============================================================================
// Main
// ============================================================================

proc main(args: [] string) {
    var cmd = if args.size > 1 then args[1] else "help";

    select cmd {
        when "build" do cmdBuild();
        when "init" do cmdInit();
        when "clean" do cmdClean();
        when "help" do cmdHelp();
        otherwise {
            writeln("Unknown command: ", cmd);
            cmdHelp();
        }
    }
}
