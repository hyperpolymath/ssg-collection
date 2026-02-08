// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Protocol.res - MCP protocol handling

open Types

// Protocol version
let protocolVersion = "2024-11-05"

// Server info
let serverName = "rats-ssg"
let serverVersion = "0.2.0"

// Create initialize response
let makeInitializeResult = (): Js.Json.t => {
  Js.Json.object_(
    Js.Dict.fromArray([
      ("protocolVersion", Js.Json.string(protocolVersion)),
      (
        "serverInfo",
        Js.Json.object_(
          Js.Dict.fromArray([
            ("name", Js.Json.string(serverName)),
            ("version", Js.Json.string(serverVersion)),
          ]),
        ),
      ),
      (
        "capabilities",
        Js.Json.object_(
          Js.Dict.fromArray([
            ("tools", Js.Json.object_(Js.Dict.empty())),
            ("resources", Js.Json.object_(Js.Dict.empty())),
          ]),
        ),
      ),
    ]),
  )
}

// Create tool list response
let makeToolsListResult = (): Js.Json.t => {
  let toolsList =
    AdapterRegistry.getAllTools()->Array.map(((name, entry)) => {
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string(name)),
          ("description", Js.Json.string(entry.tool.description)),
          (
            "inputSchema",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("type", Js.Json.string(entry.tool.inputSchema.schemaType)),
                ("properties", Js.Json.object_(entry.tool.inputSchema.properties)),
              ]),
            ),
          ),
        ]),
      )
    })

  Js.Json.object_(Js.Dict.fromArray([("tools", Js.Json.array(toolsList))]))
}

// Create resources list response
let makeResourcesListResult = (): Js.Json.t => {
  let resourcesList =
    AdapterRegistry.getAllAdapters()->Array.map(((name, adapter)) => {
      Js.Json.object_(
        Js.Dict.fromArray([
          ("uri", Js.Json.string(`adapter://${name}`)),
          ("name", Js.Json.string(adapter.name)),
          ("description", Js.Json.string(adapter.description)),
          ("mimeType", Js.Json.string("application/json")),
        ]),
      )
    })

  Js.Json.object_(Js.Dict.fromArray([("resources", Js.Json.array(resourcesList))]))
}

// Create success response
let makeSuccessResponse = (id: Js.Json.t, result: Js.Json.t): mcpResponse => {
  {
    jsonrpc: "2.0",
    id,
    result: Some(result),
    error: None,
  }
}

// Create error response
let makeErrorResponse = (id: Js.Json.t, code: int, message: string): mcpResponse => {
  {
    jsonrpc: "2.0",
    id,
    result: None,
    error: Some({code, message}),
  }
}

// Serialize response to JSON string
let serializeResponse = (response: mcpResponse): string => {
  let obj = Js.Dict.empty()
  Js.Dict.set(obj, "jsonrpc", Js.Json.string(response.jsonrpc))
  Js.Dict.set(obj, "id", response.id)

  switch response.result {
  | Some(r) => Js.Dict.set(obj, "result", r)
  | None => ()
  }

  switch response.error {
  | Some(e) =>
    Js.Dict.set(
      obj,
      "error",
      Js.Json.object_(
        Js.Dict.fromArray([
          ("code", Js.Json.number(float_of_int(e.code))),
          ("message", Js.Json.string(e.message)),
        ]),
      ),
    )
  | None => ()
  }

  Js.Json.stringify(Js.Json.object_(obj))
}
