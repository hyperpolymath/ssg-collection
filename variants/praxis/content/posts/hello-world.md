---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my doit-ssg site.

## Code Example

Here's a simple example in Io:

```io
// Hello World in Io
"Hello, World!" println

// Site configuration using prototypes
SiteConfig := Object clone do(
    title := "My Io Site"
    author := "Anonymous"
    
    describe := method(
        "Site: #{title} by #{author}" interpolate println
    )
)

// Create and use the config
config := SiteConfig clone
config title = "doit-ssg Demo"
config describe

// Process a list of pages
pages := list("index.html", "about.html", "posts.html")
pages foreach(page,
    "Processing: #{page}" interpolate println
)
```

Happy building!
