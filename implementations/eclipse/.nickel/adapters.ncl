# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# adapters.ncl — adapter configuration

let Adapter = {
  name : String,
  language : String,
  binary : String,
  description : String | optional,
  commands : {
    init : String,
    build : String,
    serve : String,
    check : String | optional,
    deploy : String | optional,
  },
  features : Array String | default = [],
} in

{
  # Rust SSGs
  zola : Adapter,
  zola = {
    name = "Zola",
    language = "Rust",
    binary = "zola",
    description = "Fast static site generator with built-in Sass compilation",
    commands = {
      init = "init",
      build = "build",
      serve = "serve",
      check = "check",
    },
    features = ["sass", "syntax-highlighting", "search", "taxonomies"],
  },

  cobalt : Adapter,
  cobalt = {
    name = "Cobalt",
    language = "Rust",
    binary = "cobalt",
    commands = {
      init = "init",
      build = "build",
      serve = "serve",
      check = "doctor",
    },
    features = ["liquid", "sass", "drafts"],
  },

  mdbook : Adapter,
  mdbook = {
    name = "mdBook",
    language = "Rust",
    binary = "mdbook",
    commands = {
      init = "init",
      build = "build",
      serve = "serve",
      check = "test",
    },
    features = ["search", "themes", "preprocessors"],
  },

  # Haskell SSGs
  hakyll : Adapter,
  hakyll = {
    name = "Hakyll",
    language = "Haskell",
    binary = "hakyll-site",
    description = "Haskell library for generating static sites",
    commands = {
      init = "init",
      build = "build",
      serve = "watch",
    },
    features = ["pandoc", "templates", "compilers"],
  },

  ema : Adapter,
  ema = {
    name = "Ema",
    language = "Haskell",
    binary = "ema",
    commands = {
      init = "gen",
      build = "gen",
      serve = "run",
    },
    features = ["hot-reload", "tailwind", "websockets"],
  },

  # Elixir SSGs
  serum : Adapter,
  serum = {
    name = "Serum",
    language = "Elixir",
    binary = "mix serum",
    commands = {
      init = "init",
      build = "build",
      serve = "server",
    },
    features = ["plugins", "themes", "live-reload"],
  },

  # Pony SSGs
  eclipse : Adapter,
  eclipse = {
    name = "Eclipse",
    language = "Pony",
    binary = "eclipse-ssg",
    description = "Actor-model static site generator with reference capabilities",
    commands = {
      init = "init",
      build = "build",
      serve = "serve",
      check = "check",
    },
    features = ["actors", "zero-copy", "incremental", "hot-reload", "reference-capabilities"],
  },

  # All adapters list
  all = [
    "babashka", "cobalt", "coleslaw", "cryogen", "documenter",
    "eclipse", "ema", "fornax", "franklin", "frog", "hakyll",
    "laika", "marmot", "mdbook", "nimble-publisher", "nimrod",
    "orchid", "perun", "pollen", "publish", "reggae",
    "scalatex", "serum", "staticwebpages", "tableau", "wub",
    "yocaml", "zola", "zotonic"
  ],

  # Adapters by language
  by_language = {
    rust = ["zola", "cobalt", "mdbook"],
    haskell = ["hakyll", "ema"],
    elixir = ["serum", "nimble-publisher", "tableau"],
    julia = ["franklin", "documenter", "staticwebpages"],
    clojure = ["cryogen", "perun", "babashka"],
    racket = ["frog", "pollen"],
    scala = ["laika", "scalatex"],
    fsharp = ["fornax"],
    ocaml = ["yocaml"],
    swift = ["publish"],
    kotlin = ["orchid"],
    nim = ["nimrod"],
    d = ["reggae"],
    crystal = ["marmot"],
    tcl = ["wub"],
    common_lisp = ["coleslaw"],
    erlang = ["zotonic"],
    pony = ["eclipse"],
  },
}
