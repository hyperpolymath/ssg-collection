// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// orbital-ssg - MCP adapters for 28 static site generators (ReScript)

module Adapters = {
  type adapter = {
    name: string,
    language: string,
    connect: unit => promise<bool>,
    build: option<string> => promise<bool>,
    serve: option<int> => promise<bool>,
  }

  // Adapter list - all 28 SSGs across 17 languages
  let adapterNames = [
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

  // Get adapter by name
  let get = async (name: string): option<adapter> => {
    try {
      let _module = await Js.import(`./adapters/${name}.res.js`)
      None // Would parse module exports
    } catch {
    | _ => None
    }
  }

  // Check all adapters
  let checkAll = async (): Js.Dict.t<bool> => {
    let results = Js.Dict.empty()
    adapterNames->Js.Array2.forEach(name => {
      results->Js.Dict.set(name, false)
    })
    results
  }

  // List all
  let list = (): array<string> => adapterNames
}

// Package metadata
let name = "orbital-ssg"
let version = "0.2.0"
let description = "MCP adapters for 28 static site generators across 17 languages"

// Main
let main = async () => {
  Js.Console.log(`${name} v${version}`)
  Js.Console.log(description)
  Js.Console.log(`\nAvailable adapters: ${Adapters.list()->Js.Array2.length->Belt.Int.toString}`)
  Adapters.list()->Js.Array2.forEach(a => Js.Console.log(`  - ${a}`))
}

let _ = main()
