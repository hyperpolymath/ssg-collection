// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Baremetal SSG - Main entry point (ReScript)

module Adapters = {
  type adapter = {
    name: string,
    language: string,
    connect: unit => promise<bool>,
  }

  let adapters: Js.Dict.t<adapter> = Js.Dict.empty()

  // Load adapter from file
  let loadAdapter = async (filename: string): option<adapter> => {
    try {
      let _module = await Js.import(`./adapters/${filename}`)
      // Dynamic import handling would go here
      None
    } catch {
    | _ => None
    }
  }

  // Load all adapters from directory
  let loadAll = async (): int => {
    Js.Console.log("Loading adapters...")
    // In ReScript/Deno, we'd use Deno.readDir
    Js.Dict.keys(adapters)->Js.Array2.length
  }

  // List all available adapters
  let list = (): unit => {
    Js.Console.log("\nAvailable adapters:")
    adapters
    ->Js.Dict.entries
    ->Js.Array2.forEach(((name, adapter)) => {
      Js.Console.log(`  - ${adapter.name} (${adapter.language})`)
    })
  }

  // Get adapter by name
  let get = (name: string): option<adapter> => {
    adapters->Js.Dict.get(name->Js.String2.toLowerCase)
  }
}

module Health = {
  type status = {
    status: string,
    adapters: int,
    version: string,
  }

  let check = (): status => {
    {
      status: "healthy",
      adapters: Adapters.adapters->Js.Dict.keys->Js.Array2.length,
      version: "1.0.0",
    }
  }
}

// Main entry point
let main = async () => {
  Js.Console.log("Baremetal SSG v1.0.0")
  Js.Console.log("====================\n")

  let count = await Adapters.loadAll()
  Js.Console.log(`Loaded ${count->Belt.Int.toString} adapters`)

  Adapters.list()

  let health = Health.check()
  Js.Console.log(`\nHealth check: ${health.status}`)
}

// Run if executed directly
let _ = main()
