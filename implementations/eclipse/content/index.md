---
title: Welcome to Eclipse SSG
date: 2025-12-17
draft: false
---

# Welcome to Eclipse SSG

Eclipse SSG is a satellite implementation in the hyperpolymath ecosystem, providing a unified interface to **28 static site generators** via the Model Context Protocol (MCP).

## Features

- **28 SSG Adapters**: Support for Zola, Hakyll, Cobalt, and 25 more
- **NoteG Language**: Custom templating language with LSP support
- **Accessibility**: BSL, ASL, GSL, and Makaton metadata support
- **Formal Verification**: Bernoulli verification engine

## Quick Start

```bash
# Build with Zola
just build-adapter zola ./site

# Start development server
just serve

# Run tests
just test-all
```

## Supported Generators

| Language | SSGs |
|----------|------|
| Rust | Zola, Cobalt, mdBook |
| Haskell | Hakyll, Ema |
| Elixir | Serum, NimblePublisher, Tableau |
| Julia | Franklin, Documenter, StaticWebPages |
