---
title: "Welcome to rats-ssg"
date: 2025-12-22
template: default
---

# Welcome

This is an example content file for rats-ssg, the unified MCP hub for 28 static site generators.

## Features

- **28 SSG Adapters**: Support for Zola, Hakyll, mdBook, and 25 more
- **MCP Protocol**: Standard tool interface for AI agents
- **Multi-Language**: SSGs in Rust, Haskell, Elixir, Julia, and more

## Getting Started

```bash
# Start the MCP server
just serve

# List available adapters
just adapter-list

# Build with Zola
deno eval "const z = await import('./adapters/zola.js'); await z.connect(); console.log(await z.tools[1].execute({path: '.'}))"
```

## Learn More

- [Cookbook](cookbook.adoc) - Command reference
- [GitHub](https://github.com/hyperpolymath/rats-ssg) - Source code
