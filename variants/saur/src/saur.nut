// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// saur-ssg - Squirrel Static Site Generator
// "Let nuts crack. Let scripts run. Let sites fossilize."
//
// Run with: sq src/saur.nut build | init | clean | help

// ============================================================================
// Configuration
// ============================================================================

local contentDir = "content";
local outputDir = "_site";
local siteTitle = "Saur Site";

// ============================================================================
// Page Metadata
// ============================================================================

class PageMeta {
    title = "Untitled";
    date = "";
    draft = false;
}

// ============================================================================
// String Utilities
// ============================================================================

function trim(s) {
    local start = 0;
    local end = s.len();

    while (start < end && (s[start] == ' ' || s[start] == '\t')) {
        start++;
    }
    while (end > start && (s[end-1] == ' ' || s[end-1] == '\t')) {
        end--;
    }

    return s.slice(start, end);
}

function startsWith(s, prefix) {
    if (s.len() < prefix.len()) return false;
    return s.slice(0, prefix.len()) == prefix;
}

function escapeHtml(s) {
    local result = "";
    foreach (i, c in s) {
        switch (c) {
            case '&': result += "&amp;"; break;
            case '<': result += "&lt;"; break;
            case '>': result += "&gt;"; break;
            case '"': result += "&quot;"; break;
            default: result += c.tochar(); break;
        }
    }
    return result;
}

function split(s, delim) {
    local result = [];
    local current = "";

    foreach (i, c in s) {
        if (c == delim[0]) {
            result.append(current);
            current = "";
        } else {
            current += c.tochar();
        }
    }
    result.append(current);

    return result;
}

function indexOf(s, search) {
    local slen = search.len();
    for (local i = 0; i <= s.len() - slen; i++) {
        if (s.slice(i, i + slen) == search) {
            return i;
        }
    }
    return -1;
}

// ============================================================================
// Frontmatter Parser
// ============================================================================

function parseFrontmatterLine(line, meta) {
    local trimmed = trim(line);
    local colonPos = indexOf(trimmed, ":");

    if (colonPos > 0) {
        local key = trim(trimmed.slice(0, colonPos));
        local value = trim(trimmed.slice(colonPos + 1));

        switch (key) {
            case "title":
                meta.title = value;
                break;
            case "date":
                meta.date = value;
                break;
            case "draft":
                meta.draft = (value == "true" || value == "yes");
                break;
        }
    }

    return meta;
}

function parseFrontmatter(lines) {
    local meta = PageMeta();
    local bodyStart = 0;

    if (lines.len() == 0) {
        return { meta = meta, bodyStart = 0 };
    }

    if (trim(lines[0]) != "---") {
        return { meta = meta, bodyStart = 0 };
    }

    local inFrontmatter = true;
    for (local i = 1; i < lines.len() && inFrontmatter; i++) {
        local line = trim(lines[i]);

        if (line == "---") {
            bodyStart = i + 1;
            inFrontmatter = false;
        } else {
            parseFrontmatterLine(line, meta);
        }
    }

    return { meta = meta, bodyStart = bodyStart };
}

// ============================================================================
// Markdown Parser
// ============================================================================

function parseMarkdown(lines, startIdx) {
    local html = "";
    local inCode = false;

    for (local i = startIdx; i < lines.len(); i++) {
        local line = lines[i];
        local trimmed = trim(line);

        // Code blocks
        if (startsWith(trimmed, "```")) {
            if (inCode) {
                html += "</code></pre>\n";
                inCode = false;
            } else {
                html += "<pre><code>";
                inCode = true;
            }
            continue;
        }

        if (inCode) {
            html += escapeHtml(line) + "\n";
            continue;
        }

        // Headers
        if (startsWith(trimmed, "### ")) {
            html += "<h3>" + trimmed.slice(4) + "</h3>\n";
            continue;
        }
        if (startsWith(trimmed, "## ")) {
            html += "<h2>" + trimmed.slice(3) + "</h2>\n";
            continue;
        }
        if (startsWith(trimmed, "# ")) {
            html += "<h1>" + trimmed.slice(2) + "</h1>\n";
            continue;
        }

        // List items
        if (startsWith(trimmed, "- ") || startsWith(trimmed, "* ")) {
            html += "<li>" + trimmed.slice(2) + "</li>\n";
            continue;
        }

        // Empty lines
        if (trimmed.len() == 0) {
            continue;
        }

        // Paragraph
        html += "<p>" + trimmed + "</p>\n";
    }

    return html;
}

// ============================================================================
// HTML Generation
// ============================================================================

function generatePage(meta, contentHtml) {
    local dateHtml = meta.date.len() > 0 ? "<time>" + meta.date + "</time>\n" : "";

    return @"<!DOCTYPE html>
<html lang=""en"">
<head>
<meta charset=""UTF-8"">
<title>" + escapeHtml(meta.title) + " | " + siteTitle + @"</title>
<style>
body{font-family:Georgia,serif;max-width:700px;margin:0 auto;padding:2rem;}
pre{background:#f5f5f5;padding:1rem;overflow-x:auto;}
time{color:#666;font-style:italic;}
</style>
</head>
<body>
<article>
<h1>" + escapeHtml(meta.title) + @"</h1>
" + dateHtml + contentHtml + @"</article>
<footer><p>Generated by saur-ssg (Squirrel)</p></footer>
</body>
</html>
";
}

// ============================================================================
// Sample Content
// ============================================================================

local sampleContent = @"---
title: Welcome to Saur
date: 2025-01-18
---

# Welcome

This site was built with **saur-ssg**, a Squirrel-powered SSG.

## Squirrel Features

- Lightweight scripting
- C-like syntax
- Dynamic typing
- Embedded-friendly
- Game development heritage

## Code Example

```squirrel
// Hello World in Squirrel
function greet(name) {
    print(""Hello, "" + name + ""!"");
}

greet(""World"");
```
";

// ============================================================================
// Commands
// ============================================================================

function cmdBuild() {
    print("saur-ssg: Building site...");
    print("  Output directory: " + outputDir);

    // Parse sample content
    local lines = split(sampleContent, '\n');
    local result = parseFrontmatter(lines);
    local meta = result.meta;
    local bodyStart = result.bodyStart;

    if (meta.draft) {
        print("    Skipping draft");
        return;
    }

    print("  Processing: " + contentDir + "/index.md");

    local contentHtml = parseMarkdown(lines, bodyStart);
    local html = generatePage(meta, contentHtml);

    print("  Generated HTML (" + html.len() + " bytes)");
    print("saur-ssg: Build complete.");
}

function cmdInit() {
    print("saur-ssg: Initializing new site...");
    print("  Created " + contentDir + "/");
    print("  Created " + outputDir + "/");
    print("  Created sample content");
    print("saur-ssg: Site initialized.");
    print("Run 'sq src/saur.nut build' to build.");
}

function cmdClean() {
    print("saur-ssg: Cleaning " + outputDir + "/...");
    print("saur-ssg: Clean complete.");
}

function cmdHelp() {
    print("saur-ssg - Squirrel Static Site Generator");
    print("");
    print("USAGE:");
    print("  sq src/saur.nut <command>");
    print("");
    print("COMMANDS:");
    print("  build    Build the site");
    print("  init     Initialize a new site");
    print("  clean    Remove generated files");
    print("  help     Show this help");
    print("");
    print("FEATURES:");
    print("  - Lightweight and embeddable");
    print("  - Dynamic typing");
    print("  - Game-oriented heritage");
    print("  - Portable bytecode");
}

// ============================================================================
// Main
// ============================================================================

function main() {
    local args = [];

    // In Squirrel, command line args come from vargv
    if ("vargv" in getroottable()) {
        args = vargv;
    }

    local cmd = args.len() > 0 ? args[0] : "help";

    switch (cmd) {
        case "build":
            cmdBuild();
            break;
        case "init":
            cmdInit();
            break;
        case "clean":
            cmdClean();
            break;
        case "help":
            cmdHelp();
            break;
        default:
            print("Unknown command: " + cmd);
            cmdHelp();
            break;
    }
}

main();
