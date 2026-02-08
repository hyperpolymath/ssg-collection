// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// ToolCall.res - MCP tool call execution

open Types

type toolCallParams = {
  name: string,
  arguments: option<Js.Dict.t<Js.Json.t>>,
}

// Parse tool call params from JSON
let parseParams = (params: option<Js.Dict.t<Js.Json.t>>): result<toolCallParams, string> => {
  switch params {
  | None => Error("Missing params")
  | Some(p) =>
    switch Js.Dict.get(p, "name") {
    | None => Error("Missing tool name")
    | Some(nameJson) =>
      switch Js.Json.decodeString(nameJson) {
      | None => Error("Tool name must be a string")
      | Some(name) =>
        let arguments = switch Js.Dict.get(p, "arguments") {
        | Some(argsJson) =>
          switch Js.Json.decodeObject(argsJson) {
          | Some(obj) => Some(obj)
          | None => None
          }
        | None => None
        }
        Ok({name, arguments})
      }
    }
  }
}

// Execute a tool call
let execute = async (params: toolCallParams): result<Js.Json.t, string> => {
  switch AdapterRegistry.getTool(params.name) {
  | None => Error(`Tool not found: ${params.name}`)
  | Some(entry) =>
    // Ensure adapter is connected
    if !entry.adapter.isConnected() {
      let _ = await entry.adapter.connect()
    }

    let args = switch params.arguments {
    | Some(a) => a
    | None => Js.Dict.empty()
    }

    let result = await entry.tool.execute(args)

    let text = if result.success {
      result.stdout
    } else {
      result.stderr
    }

    let content =
      Js.Json.array([
        Js.Json.object_(
          Js.Dict.fromArray([
            ("type", Js.Json.string("text")),
            ("text", Js.Json.string(text)),
          ]),
        ),
      ])

    Ok(
      Js.Json.object_(
        Js.Dict.fromArray([
          ("content", content),
          ("isError", Js.Json.boolean(!result.success)),
        ]),
      ),
    )
  }
}
