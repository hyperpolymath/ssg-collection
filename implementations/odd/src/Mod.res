// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * odd-ssg - Satellite SSG Adapter Provider
 *
 * Provides MCP-compatible adapters for 30 static site generators
 * with Mill-Based Synthesis engine for template processing.
 *
 * @module
 */

// Re-export core engine
module Engine = Core

// Re-export build system
module Build = Build

// Re-export types
module Types = Types

// Re-export language tooling
module Lexer = Lexer
module Parser = Parser

// Version
let version = "0.1.0"

// Adapter list
let adapters = [
  "babashka",
  "cobalt",
  "coleslaw",
  "cryogen",
  "documenter",
  "ema",
  "fornax",
  "franklin",
  "frog",
  "hakyll",
  "laika",
  "marmot",
  "mdbook",
  "nimble-publisher",
  "nimrod",
  "orchid",
  "perun",
  "pollen",
  "publish",
  "reggae",
  "scalatex",
  "serum",
  "staticwebpages",
  "tableau",
  "wub",
  "yocaml",
  "zola",
  "zotonic",
]

type adapterName = string

// Dynamic adapter loading (uses JS glue)
@module("./adapter_loader.js")
external loadAdapter: adapterName => promise<Types.ssgAdapter> = "loadAdapter"

@module("./adapter_loader.js")
external loadAllAdapters: unit => promise<Js.Dict.t<Types.ssgAdapter>> = "loadAllAdapters"
