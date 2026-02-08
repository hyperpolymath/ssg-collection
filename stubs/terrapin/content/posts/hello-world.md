---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my terrapin-ssg site.

## Code Example

Here's a simple example in Logo:

```logo
; Hello World in Logo
print [Hello, World!]

; Define a procedure for site configuration
to site.config :title :author
  make "site.title :title
  make "site.author :author
  print (sentence [Site:] :title [by] :author)
end

; Call the configuration
site.config "My\ Logo\ Site "Anonymous

; List processing for content
to process.pages :pagelist
  if empty? :pagelist [stop]
  print first :pagelist
  process.pages butfirst :pagelist
end

process.pages [index.html about.html posts.html]
```

Happy building!
