<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my estate-ssg site.

## Code Example

Here's a simple example in Eiffel:

```eiffel
class
    HELLO_WORLD

create
    make

feature -- Initialization

    make
            -- Print greeting message.
        do
            print ("Hello, World!%N")
        ensure
            greeting_printed: True
        end

end
```

Happy building!
