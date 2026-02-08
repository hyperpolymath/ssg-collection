---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my anvil-ssg site.

## Code Example

Here's a simple example in Ada:

```ada
-- Hello World in Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Hello_World is
   -- Site configuration as a record type
   type Site_Config is record
      Title  : String (1 .. 50);
      Author : String (1 .. 50);
   end record;

   Config : Site_Config := (
      Title  => "My Ada Site" & (12 .. 50 => ' '),
      Author => "Anonymous" & (10 .. 50 => ' ')
   );
begin
   Put_Line ("Hello, World!");
   Put_Line ("Site: " & Config.Title);
   Put_Line ("Author: " & Config.Author);
end Hello_World;
```

Happy building!
