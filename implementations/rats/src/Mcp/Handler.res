// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Handler.res - MCP request handler

open Types

// Handle MCP request and return response
let handle = async (request: mcpRequest): mcpResponse => {
  switch request.method {
  | "initialize" => Protocol.makeSuccessResponse(request.id, Protocol.makeInitializeResult())

  | "tools/list" => Protocol.makeSuccessResponse(request.id, Protocol.makeToolsListResult())

  | "tools/call" =>
    switch ToolCall.parseParams(request.params) {
    | Error(msg) => Protocol.makeErrorResponse(request.id, -32602, msg)
    | Ok(params) =>
      switch await ToolCall.execute(params) {
      | Error(msg) => Protocol.makeErrorResponse(request.id, -32603, msg)
      | Ok(result) => Protocol.makeSuccessResponse(request.id, result)
      }
    }

  | "resources/list" =>
    Protocol.makeSuccessResponse(request.id, Protocol.makeResourcesListResult())

  | method => Protocol.makeErrorResponse(request.id, -32601, `Method not found: ${method}`)
  }
}
