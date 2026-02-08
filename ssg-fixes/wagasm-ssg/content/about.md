---
title: About wagasm-ssg
---

# About wagasm-ssg

wagasm-ssg is an experimental static site generator demonstrating that WebAssembly Text (WAT) can be used as a primary programming language.

## Architecture

```
src/wagasm-ssg.wat    (ALL SSG logic)
       │
       ▼
  runtime/host.ts     (I/O only)
```

The entire site generation logic lives in the WAT file. The host runtime provides only file I/O capabilities.

## Part of poly-ssg

wagasm-ssg is a satellite project in the poly-ssg constellation, demonstrating extreme language diversity in static site generators.
