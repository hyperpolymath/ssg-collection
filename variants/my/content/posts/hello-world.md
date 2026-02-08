---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my my-ssg site.

## Code Example

Here's a simple example in Janet:

```janet
# Hello World
(print "Hello, World!")

# Define a function to render a page
(defn render-page [title content]
  (string 
    "<!DOCTYPE html>\n"
    "<html>\n"
    "<head><title>" title "</title></head>\n"
    "<body>" content "</body>\n"
    "</html>"))

# Use PEG for simple markdown parsing
(def md-bold
  ~{:main (* "**" (capture (to "**")) "**")})

(peg/match md-bold "**bold text**")
# => @["bold text"]
```

Happy building!
