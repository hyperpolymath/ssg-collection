---
title: Hello World
date: 2025-01-18
draft: false
---

# Hello, World!

This is the first post on my ddraig-ssg site.

## Code Example

Here's a simple example in Idris 2:

```idris
module Main

main : IO ()
main = putStrLn "Hello, World!"

-- A more interesting example with dependent types
data Vect : Nat -> Type -> Type where
  Nil  : Vect 0 a
  (::) : a -> Vect n a -> Vect (S n) a

append : Vect n a -> Vect m a -> Vect (n + m) a
append Nil ys = ys
append (x :: xs) ys = x :: append xs ys
```

Happy building!
