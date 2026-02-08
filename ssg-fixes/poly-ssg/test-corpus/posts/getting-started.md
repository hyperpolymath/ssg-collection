---
title: Getting Started with Forth Estate
date: 2024-01-16
template: default
tags: [tutorial, forth]
---

# Getting Started

This guide will help you build your first site with Forth Estate.

## Prerequisites

You'll need:

- Gforth 0.7 or later
- A Unix-like environment

## Installation

Clone the poly-ssg repository:

```bash
git clone https://github.com/hyperpolymath/poly-ssg.git
cd poly-ssg/engines/forth-estate
```

## Your First Build

Create a markdown file in your source directory:

```markdown
---
title: My First Post
date: 2024-01-20
---

# Hello World

Welcome to my site!
```

Then run Forth Estate:

```bash
gforth forth-estate.fs -e "s\" src\" s\" output\" build bye"
```

## Understanding the Stack

In Forth, data lives on the stack:

| Before | Word | After |
|--------|------|-------|
| `5 3`  | `+`  | `8`   |
| `8`    | `dup`| `8 8` |
| `8 8`  | `*`  | `64`  |

Forth Estate uses this same principle for content transformation.

## Next Steps

- Read the [Philosophy](../docs/PHILOSOPHY.md) document
- Explore the source code
- Try adding custom templates
