# Adding a New Engine

This guide walks through adding a new language implementation to poly-ssg.

## Prerequisites

1. Familiarity with your target language
2. Understanding of the [Engine Interface Contract](../README.md#engine-interface-contract)
3. A working development environment for your language

## Step 1: Create Directory Structure

```bash
mkdir -p engines/<language>-<name>/
cd engines/<language>-<name>/
```

Example for a hypothetical Rust engine:
```bash
mkdir -p engines/rust-oxidize/
```

## Step 2: Create manifest.json

Every engine needs a manifest describing its capabilities:

```json
{
  "name": "<engine-name>",
  "language": "<Language>",
  "implementation": "<Compiler/Runtime>",
  "version": "0.1.0",
  "build_command": ["<command>", "<args>"],
  "build_args": ["build"],
  "features": ["markdown", "frontmatter", "templates"],
  "dependencies": {
    "runtime": ["<dependency> >= <version>"],
    "optional": []
  }
}
```

## Step 3: Implement Core Features

### Required: Frontmatter Parsing

Parse YAML frontmatter between `---` delimiters:

```yaml
---
title: string (required)
date: ISO-8601 date (optional)
template: string (optional, default: "default")
tags: list of strings (optional)
draft: boolean (optional, default: false)
---
```

### Required: Markdown to HTML

Convert markdown content to valid HTML5. At minimum support:
- Headers (h1-h6)
- Paragraphs
- Bold/italic
- Links
- Code blocks
- Lists (ordered and unordered)

### Required: Template System

Apply templates to rendered content. Templates receive:
- `content`: The rendered HTML
- `title`: From frontmatter
- `date`: From frontmatter
- `tags`: From frontmatter

## Step 4: Implement Build Interface

Your engine must accept:
```
<build-command> <source-path> <output-path>
```

And must:
- Process all `.md` files in source-path
- Skip files with `draft: true` in frontmatter
- Output HTML to output-path preserving directory structure
- Exit 0 on success, non-zero on failure
- Write progress to stdout, errors to stderr

## Step 5: Test Against Corpus

```bash
# From poly-ssg root
make test-engine ENGINE=<your-engine>

# Or manually
cd engines/<your-engine>
<build-command> ../../test-corpus/posts ../../test-corpus/output/<your-engine>
```

## Step 6: Add CI Configuration

Add your engine to `.github/workflows/test.yml`:

```yaml
- name: Test <engine-name>
  run: |
    cd engines/<engine-name>
    <setup-commands>
    <test-commands>
```

## Step 7: Document

Add a README.md in your engine directory explaining:
- Installation requirements
- How to run
- Language-specific features
- Any deviations from the standard

## Submit

1. Fork the repository
2. Create a feature branch
3. Submit a merge request

## Philosophy Reminder

Remember: embrace your language's paradigm. Don't force it to work like another language. The diversity of approaches is the point.
