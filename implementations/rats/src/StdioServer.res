// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// StdioServer.res - MCP stdio transport

open Types

// External: Start stdio server (JS interop for Deno stdin/stdout)
@val
external startStdioServer: (string => promise<string>) => promise<unit> = "startStdioServer"

// Parse JSON request
let parseRequest = (line: string): result<mcpRequest, string> => {
  try {
    let json = Js.Json.parseExn(line)
    switch Js.Json.decodeObject(json) {
    | None => Error("Invalid JSON object")
    | Some(obj) =>
      let jsonrpc = switch Js.Dict.get(obj, "jsonrpc") {
      | Some(j) => Js.Json.decodeString(j)->Belt.Option.getWithDefault("2.0")
      | None => "2.0"
      }

      let id = switch Js.Dict.get(obj, "id") {
      | Some(i) => i
      | None => Js.Json.number(0.0)
      }

      let method = switch Js.Dict.get(obj, "method") {
      | Some(m) => Js.Json.decodeString(m)->Belt.Option.getWithDefault("")
      | None => ""
      }

      let params = switch Js.Dict.get(obj, "params") {
      | Some(p) => Js.Json.decodeObject(p)
      | None => None
      }

      Ok({jsonrpc, id, method, params})
    }
  } catch {
  | _ => Error("Failed to parse JSON")
  }
}

// Handle a single line
let handleLine = async (line: string): string => {
  switch parseRequest(line) {
  | Error(msg) =>
    Logger.error(`MCP parse error: ${msg}`)
    let response = Mcp.Protocol.makeErrorResponse(Js.Json.number(0.0), -32700, msg)
    Mcp.Protocol.serializeResponse(response)

  | Ok(request) =>
    let response = await Mcp.Handler.handle(request)
    Mcp.Protocol.serializeResponse(response)
  }
}

// Start the stdio server
let start = async () => {
  Logger.info("MCP server ready on stdio")
  await startStdioServer(handleLine)
}
