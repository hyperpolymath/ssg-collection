---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my gungir-ssg site.

## Code Example

Here's a simple example in ReScript:

```rescript
// Simple greeting
Js.log("Hello, World!")

// Pattern matching example
type page = Index | About | Post(string)

let renderTitle = (page: page): string => {
  switch page {
  | Index => "Welcome"
  | About => "About Us"
  | Post(title) => title
  }
}

// Pipe-first syntax
["index", "about", "contact"]
->Array.map(name => name->String.toUpperCase)
->Array.forEach(Js.log)
```

Happy building!
