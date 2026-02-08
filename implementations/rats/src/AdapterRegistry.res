// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// AdapterRegistry.res - Adapter loading and registration

open Types

// Adapter registry map
let adapters: Js.Dict.t<adapter> = Js.Dict.empty()

// Tool registry map
let tools: Js.Dict.t<toolEntry> = Js.Dict.empty()

// Get adapter by name
let getAdapter = (name: string): option<adapter> => {
  Js.Dict.get(adapters, name)
}

// Get tool by name
let getTool = (name: string): option<toolEntry> => {
  Js.Dict.get(tools, name)
}

// Register an adapter and its tools
let registerAdapter = (name: string, adapter: adapter) => {
  Js.Dict.set(adapters, name, adapter)

  // Register all tools
  adapter.tools->Array.forEach(tool => {
    Js.Dict.set(tools, tool.name, {adapter, tool})
  })
}

// Get adapter count
let adapterCount = (): int => {
  Js.Dict.keys(adapters)->Array.length
}

// Get tool count
let toolCount = (): int => {
  Js.Dict.keys(tools)->Array.length
}

// Get all adapters as array
let getAllAdapters = (): array<(string, adapter)> => {
  Js.Dict.entries(adapters)
}

// Get all tools as array
let getAllTools = (): array<(string, toolEntry)> => {
  Js.Dict.entries(tools)
}
