---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my shift-ssg site.

## Code Example

Here's a simple example in Wren:

```wren
// Hello World in Wren
System.print("Hello, World!")

// A simple class for site configuration
class SiteConfig {
  construct new(title, author) {
    _title = title
    _author = author
  }

  title { _title }
  author { _author }

  toString { "Site: %(_title) by %(_author)" }
}

var config = SiteConfig.new("My Wren Site", "Anonymous")
System.print(config.toString)
```

Happy building!
