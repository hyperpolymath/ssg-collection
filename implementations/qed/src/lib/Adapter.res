// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Core adapter types and utilities

// Tool input schema type
type inputSchema = {
  @as("type") type_: string,
  properties: Js.Dict.t<{..}>,
  required?: array<string>,
}

// Tool execution result
type executeResult = {
  success: bool,
  stdout: string,
  stderr: string,
  code: int,
}

// Tool definition
type tool = {
  name: string,
  description: string,
  inputSchema: inputSchema,
  execute: Js.Dict.t<string> => promise<executeResult>,
}

// Adapter module interface
type adapter = {
  name: string,
  language: string,
  description: string,
  tools: array<tool>,
  connect: unit => promise<bool>,
  disconnect: unit => promise<unit>,
  isConnected: unit => bool,
}

// Helper to create tool input schema
let makeInputSchema = (~properties: Js.Dict.t<{..}>, ~required: array<string>=[]): inputSchema => {
  type_: "object",
  properties,
  required,
}

// Dynamic import helper for adapters
@val external importAdapter: string => promise<adapter> = "import"

// Adapter file name utilities
let adapterPathFromName = (name: string): string => {
  `../adapters/${name}.js`
}

let isAdapterFile = (filename: string): bool => {
  Js.String2.endsWith(filename, ".js") && filename !== "README.md"
}
