# Forth Estate

A stack-based static site generator written in Gforth, embracing Forth's concatenative paradigm.

## Philosophy

Forth Estate doesn't fight the language—it embraces stack-based thinking:

```forth
\ Data flows through transformations
s" post.md" read-file parse-frontmatter render-markdown
\ Result is now on the stack
```

No intermediate variables needed. Each word transforms the stack. Composition is natural.

## Requirements

- Gforth 0.7 or later

### Installation on Fedora

```bash
sudo dnf install gforth
```

### Installation on Ubuntu/Debian

```bash
sudo apt install gforth
```

### Installation on macOS

```bash
brew install gforth
```

## Usage

### Interactive Mode

```bash
gforth forth-estate.fs
```

This loads the SSG and provides test commands:

```forth
test-markdown      \ Test markdown parsing
test-frontmatter   \ Test frontmatter parsing
test-full          \ Full pipeline test
```

### Build Mode

```bash
gforth forth-estate.fs -e "s\" source-dir\" s\" output-dir\" build bye"
```

## Features

### Frontmatter Parsing

Standard YAML frontmatter between `---` delimiters:

```yaml
---
title: My Post
date: 2024-01-15
template: default
tags: [forth, ssg]
draft: false
---
```

### Markdown Support

- Headers (h1-h6)
- Paragraphs
- **Bold** and *italic*
- `inline code`
- Code blocks with triple backticks
- Unordered lists (- or *)
- Ordered lists (1. 2. 3.)
- [Links](url)
- Horizontal rules (---)

### Template System

Basic Mustache-style placeholders:

- `{{title}}` - Post title from frontmatter
- `{{date}}` - Post date from frontmatter
- `{{content}}` - Rendered HTML content
- `{{tags}}` - Tags from frontmatter

## Architecture

The SSG is organized into layers:

1. **String Utilities** - Basic string operations
2. **Dynamic Buffer** - Growing output buffer
3. **Frontmatter Parser** - YAML parsing
4. **Markdown Parser** - Markdown to HTML
5. **Template System** - Placeholder substitution
6. **File I/O** - Read/write operations
7. **Build System** - Orchestration

## Stack Conventions

Forth Estate follows standard Forth conventions:

- `( before -- after )` stack comments
- Strings as `addr u` pairs (address and length)
- Flags as `true` (-1) or `false` (0)

## Extending

The code is intentionally readable. To add features:

1. Study the existing word definitions
2. Follow the stack transformation pattern
3. Test with `test-markdown` and `test-full`

## License

MIT - Part of the poly-ssg project.
