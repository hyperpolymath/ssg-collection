// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// SSG Adapter type definitions

type inputSchema = {
  @as("type") type_: string,
  properties?: Js.Dict.t<Js.Json.t>,
  required?: array<string>,
}

type tool = {
  name: string,
  description: string,
  inputSchema: inputSchema,
  execute: Js.Dict.t<Js.Json.t> => promise<Js.Json.t>,
}

type t = {
  name: string,
  language: string,
  description: string,
  connect: unit => promise<bool>,
  disconnect: unit => promise<unit>,
  isConnected: unit => bool,
  tools: array<tool>,
}

// Adapter names that should be available
let coreAdapters = ["zola", "hakyll", "mdbook", "serum", "cobalt", "franklin"]

let expectedAdapterCount = 28

// Dynamic import helper
@val external importAdapter: string => promise<t> = "import"

let loadAdapter = async (name: string): option<t> => {
  try {
    let path = `../../adapters/${name}.js`
    let adapter = await importAdapter(path)
    Some(adapter)
  } catch {
  | _ => None
  }
}

// Validation functions
let hasRequiredExports = (adapter: t): bool => {
  adapter.name !== "" &&
  adapter.language !== "" &&
  adapter.description !== "" &&
  Js.Array2.isArray(adapter.tools)
}

let hasValidTools = (adapter: t): bool => {
  adapter.tools->Js.Array2.every(tool => {
    tool.name !== "" &&
    tool.description !== "" &&
    tool.inputSchema.type_ === "object"
  })
}
