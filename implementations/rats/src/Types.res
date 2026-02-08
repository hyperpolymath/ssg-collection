// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Types.res - Shared type definitions for rats-ssg MCP Server

// Command execution result
type commandResult = {
  success: bool,
  stdout: string,
  stderr: string,
  code: int,
}

// Tool input schema (JSON Schema format)
type inputSchema = {
  @as("type") schemaType: string,
  properties: Js.Dict.t<Js.Json.t>,
  required: option<array<string>>,
}

// Tool definition
type tool = {
  name: string,
  description: string,
  inputSchema: inputSchema,
  execute: Js.Dict.t<Js.Json.t> => promise<commandResult>,
}

// SSG Adapter interface
type adapter = {
  name: string,
  language: string,
  description: string,
  tools: array<tool>,
  connect: unit => promise<bool>,
  disconnect: unit => promise<unit>,
  isConnected: unit => bool,
}

// Tool entry in registry
type toolEntry = {
  adapter: adapter,
  tool: tool,
}

// MCP Request
type mcpRequest = {
  jsonrpc: string,
  id: Js.Json.t, // Can be number or string
  method: string,
  params: option<Js.Dict.t<Js.Json.t>>,
}

// MCP Error
type mcpError = {
  code: int,
  message: string,
}

// MCP Response
type mcpResponse = {
  jsonrpc: string,
  id: Js.Json.t,
  result: option<Js.Json.t>,
  error: option<mcpError>,
}

// Server configuration
type config = {
  port: int,
  host: string,
  logLevel: string,
}

// Log level enum
type logLevel = Debug | Info | Warn | Error

let logLevelFromString = (s: string): logLevel => {
  switch s {
  | "debug" => Debug
  | "info" => Info
  | "warn" => Warn
  | "error" => Error
  | _ => Info
  }
}

let logLevelToInt = (l: logLevel): int => {
  switch l {
  | Debug => 0
  | Info => 1
  | Warn => 2
  | Error => 3
  }
}
