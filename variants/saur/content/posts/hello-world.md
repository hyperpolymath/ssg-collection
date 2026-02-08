---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my saur-ssg site.

## Code Example

Here's a simple example in Squirrel:

```squirrel
// Hello World in Squirrel
local greeting = "Hello, World!"
print(greeting)

// A simple table for site configuration
local config = {
    title = "My Squirrel Site"
    author = "Anonymous"
    buildDate = date()
}

foreach (key, value in config) {
    print(key + ": " + value)
}
```

Happy building!
