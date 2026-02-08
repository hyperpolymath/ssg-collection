# shift-ssg

Wren-based static site generator - fiber-yielding site generation.

> "Let fibers yield. Let classes compose. Let sites shift."

## Overview

shift-ssg is a static site generator written in [Wren](https://wren.io/), a small, fast, class-based scripting language. It uses Wren's elegant class system and fiber-based concurrency for clean, composable site generation.

## Features

- **Class-based design** - Clean OOP with Wren's elegant syntax
- **Fiber-ready** - Built for Wren's cooperative concurrency model
- **Markdown processing** - Convert Markdown to HTML with frontmatter parsing
- **Template system** - Simple HTML templates with variable substitution
- **Fast execution** - Wren's small footprint and efficient VM

## Requirements

- [Wren CLI](https://wren.io/cli/) or embedded Wren runtime

## Usage

```bash
# Build the site
wren src/shift.wren build

# Initialize a new site
wren src/shift.wren init

# Clean the output directory
wren src/shift.wren clean

# Show help
wren src/shift.wren help
```

## Project Structure

```
shift-ssg/
├── src/
│   └── shift.wren    # Main SSG implementation
├── content/          # Markdown source files (created by init)
├── _site/            # Generated output (created by build)
└── README.md
```

## License

PMPL-1.0-or-later

## Part of poly-ssg-mcp

This SSG is part of the [poly-ssg-mcp](https://github.com/hyperpolymath/poly-ssg-mcp) polyglot SSG ecosystem.
