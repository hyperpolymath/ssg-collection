# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Adapter configuration schema (Nickel)

let AdapterSchema = {
  name | String,
  language | String,
  description | String,
  binary | String,
  timeout | Number | default = 30000,
  tools | Array {
    name | String,
    timeout | Number | default = 30000,
  },
} in

let Adapter = fun name lang binary desc => AdapterSchema & {
  name = name,
  language = lang,
  binary = binary,
  description = desc,
  tools = [
    { name = "%{std.string.lowercase name}_init", timeout = 30000 },
    { name = "%{std.string.lowercase name}_build", timeout = 120000 },
    { name = "%{std.string.lowercase name}_serve", timeout = 0 },
    { name = "%{std.string.lowercase name}_check", timeout = 60000 },
    { name = "%{std.string.lowercase name}_version", timeout = 5000 },
  ],
} in

{
  # SSG Adapters by Language Family

  rust = {
    zola = Adapter "Zola" "Rust" "zola" "Fast SSG with built-in Sass and syntax highlighting",
    cobalt = Adapter "Cobalt" "Rust" "cobalt" "Simple static site generator",
    mdbook = Adapter "mdBook" "Rust" "mdbook" "Create books from Markdown",
  },

  haskell = {
    hakyll = Adapter "Hakyll" "Haskell" "stack" "Haskell library for generating static sites",
    ema = Adapter "Ema" "Haskell" "ema" "Static site generator with hot reload",
  },

  elixir = {
    serum = Adapter "Serum" "Elixir" "mix" "Simple static website generator",
    tableau = Adapter "Tableau" "Elixir" "mix" "Data-driven static site generator",
  },

  julia = {
    franklin = Adapter "Franklin.jl" "Julia" "julia" "Fast static site generator for technical blogging",
    documenter = Adapter "Documenter.jl" "Julia" "julia" "Documentation generator for Julia packages",
    staticwebpages = Adapter "StaticWebPages.jl" "Julia" "julia" "Academic static site generator",
  },

  clojure = {
    cryogen = Adapter "Cryogen" "Clojure" "lein" "Simple static site generator",
    perun = Adapter "Perun" "Clojure" "boot" "Composable static site generator",
  },

  ocaml = {
    yocaml = Adapter "YOCaml" "OCaml" "dune" "Static site generator in OCaml",
  },

  racket = {
    frog = Adapter "Frog" "Racket" "raco" "Racket-based static blog generator",
    pollen = Adapter "Pollen" "Racket" "raco" "Programmable publishing system",
  },

  fsharp = {
    fornax = Adapter "Fornax" "F#" "dotnet" "Scriptable static site generator",
  },

  scala = {
    laika = Adapter "Laika" "Scala" "sbt" "Customizable text markup transformer",
    scalatex = Adapter "Scalatex" "Scala" "sbt" "Scala DSL for HTML generation",
  },

  kotlin = {
    orchid = Adapter "Orchid" "Kotlin" "gradle" "Beautiful documentation made simple",
  },

  swift = {
    publish = Adapter "Publish" "Swift" "swift" "Static site generator for Swift developers",
  },

  nim = {
    nimrod = Adapter "Nimrod" "Nim" "nimble" "Nim-based static site generator",
    nimblePublisher = Adapter "NimblePublisher" "Nim" "nimble" "Static publishing framework",
  },

  d = {
    reggae = Adapter "Reggae" "D" "dub" "Build system and static site generator",
  },

  crystal = {
    marmot = Adapter "Marmot" "Crystal" "shards" "Static site generator in Crystal",
  },

  erlang = {
    zotonic = Adapter "Zotonic" "Erlang" "rebar3" "Web framework with CMS capabilities",
  },

  tcl = {
    wub = Adapter "Wub" "Tcl" "tclsh" "Web server and static site generator",
  },

  lisp = {
    coleslaw = Adapter "Coleslaw" "Common Lisp" "sbcl" "Lisp-based static site generator",
  },

  clojurescript = {
    babashka = Adapter "Babashka" "ClojureScript" "bb" "Native Clojure scripting",
  },
}
