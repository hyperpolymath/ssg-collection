// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * NoteG MCP Server
 * Model Context Protocol server for odd-ssg adapters
 */

open Types

// MCP Protocol types
type mcpRequest = {
  jsonrpc: string,
  id: Js.Json.t,
  method: string,
  params: option<Js.Dict.t<Js.Json.t>>,
}

type mcpResponse = {
  jsonrpc: string,
  id: Js.Json.t,
  result: option<Js.Json.t>,
  error: option<{"code": int, "message": string}>,
}

type tool = {
  name: string,
  description: string,
  inputSchema: toolInputSchema,
}

type resource = {
  uri: string,
  name: string,
  mimeType: option<string>,
  description: option<string>,
}

// Adapter list
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

// MCP Server state
type mcpServer = {
  adapters: Js.Dict.t<ssgAdapter>,
  connectedAdapters: Js.Dict.t<bool>,
}

let createServer = (): mcpServer => {
  adapters: Js.Dict.empty(),
  connectedAdapters: Js.Dict.empty(),
}

let handleInitialize = (request: mcpRequest): mcpResponse => {
  {
    jsonrpc: "2.0",
    id: request.id,
    error: None,
    result: Some(
      Js.Json.object_(
        Js.Dict.fromArray([
          ("protocolVersion", Js.Json.string("2024-11-05")),
          (
            "capabilities",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("tools", Js.Json.object_(Js.Dict.empty())),
                (
                  "resources",
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      ("subscribe", Js.Json.boolean(false)),
                      ("listChanged", Js.Json.boolean(false)),
                    ]),
                  ),
                ),
              ]),
            ),
          ),
          (
            "serverInfo",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("name", Js.Json.string("odd-ssg")),
                ("version", Js.Json.string("0.1.0")),
              ]),
            ),
          ),
        ]),
      ),
    ),
  }
}

let handleToolsList = (server: mcpServer, request: mcpRequest): mcpResponse => {
  let tools = []

  // Add meta tools
  Js.Array2.push(
    tools,
    Js.Json.object_(
      Js.Dict.fromArray([
        ("name", Js.Json.string("odd_ssg_list_adapters")),
        ("description", Js.Json.string("List all available SSG adapters")),
        (
          "inputSchema",
          Js.Json.object_(
            Js.Dict.fromArray([("type", Js.Json.string("object")), ("properties", Js.Json.object_(Js.Dict.empty()))]),
          ),
        ),
      ]),
    ),
  )->ignore

  Js.Array2.push(
    tools,
    Js.Json.object_(
      Js.Dict.fromArray([
        ("name", Js.Json.string("odd_ssg_connect")),
        ("description", Js.Json.string("Connect to an SSG adapter (check if binary is available)")),
        (
          "inputSchema",
          Js.Json.object_(
            Js.Dict.fromArray([
              ("type", Js.Json.string("object")),
              (
                "properties",
                Js.Json.object_(
                  Js.Dict.fromArray([
                    (
                      "adapter",
                      Js.Json.object_(
                        Js.Dict.fromArray([
                          ("type", Js.Json.string("string")),
                          ("description", Js.Json.string("Adapter name (e.g., 'zola', 'hakyll')")),
                        ]),
                      ),
                    ),
                  ]),
                ),
              ),
              ("required", Js.Json.array([Js.Json.string("adapter")])),
            ]),
          ),
        ),
      ]),
    ),
  )->ignore

  // Add tools from adapters
  Js.Dict.entries(server.adapters)->Belt.Array.forEach(((adapterName, adapter)) => {
    adapter.tools->Belt.Array.forEach(tool => {
      Js.Array2.push(
        tools,
        Js.Json.object_(
          Js.Dict.fromArray([
            ("name", Js.Json.string(`${adapterName}_${tool.name}`)),
            ("description", Js.Json.string(`[${adapter.name}] ${tool.description}`)),
            (
              "inputSchema",
              Js.Json.object_(
                Js.Dict.fromArray([
                  ("type", Js.Json.string(tool.inputSchema.type_)),
                  (
                    "properties",
                    Js.Json.object_(tool.inputSchema.properties),
                  ),
                ]),
              ),
            ),
          ]),
        ),
      )->ignore
    })
  })

  {
    jsonrpc: "2.0",
    id: request.id,
    error: None,
    result: Some(Js.Json.object_(Js.Dict.fromArray([("tools", Js.Json.array(tools))]))),
  }
}

let handleToolsCall = async (
  server: mcpServer,
  request: mcpRequest,
): mcpResponse => {
  let params = request.params->Belt.Option.getWithDefault(Js.Dict.empty())
  let toolName =
    Js.Dict.get(params, "name")
    ->Belt.Option.flatMap(Js.Json.decodeString)
    ->Belt.Option.getWithDefault("")
  let args =
    Js.Dict.get(params, "arguments")
    ->Belt.Option.flatMap(Js.Json.decodeObject)
    ->Belt.Option.getWithDefault(Js.Dict.empty())

  // Handle meta tools
  if toolName == "odd_ssg_list_adapters" {
    let adapterList =
      Js.Dict.entries(server.adapters)->Belt.Array.map(((name, adapter)) => {
        Js.Json.object_(
          Js.Dict.fromArray([
            ("name", Js.Json.string(name)),
            ("displayName", Js.Json.string(adapter.name)),
            ("language", Js.Json.string(adapter.language)),
            ("description", Js.Json.string(adapter.description)),
            (
              "connected",
              Js.Json.boolean(
                Js.Dict.get(server.connectedAdapters, name)->Belt.Option.getWithDefault(false),
              ),
            ),
          ]),
        )
      })

    {
      jsonrpc: "2.0",
      id: request.id,
      error: None,
      result: Some(
        Js.Json.object_(
          Js.Dict.fromArray([
            (
              "content",
              Js.Json.array([
                Js.Json.object_(
                  Js.Dict.fromArray([
                    ("type", Js.Json.string("text")),
                    ("text", Js.Json.string(Js.Json.stringify(Js.Json.array(adapterList)))),
                  ]),
                ),
              ]),
            ),
          ]),
        ),
      ),
    }
  } else if toolName == "odd_ssg_connect" {
    let adapterName =
      Js.Dict.get(args, "adapter")
      ->Belt.Option.flatMap(Js.Json.decodeString)
      ->Belt.Option.getWithDefault("")

    switch Js.Dict.get(server.adapters, adapterName) {
    | None => {
        jsonrpc: "2.0",
        id: request.id,
        result: None,
        error: Some({"code": -32602, "message": `Unknown adapter: ${adapterName}`}),
      }
    | Some(adapter) =>
      let connected = await adapter.connect()
      if connected {
        Js.Dict.set(server.connectedAdapters, adapterName, true)
      }

      {
        jsonrpc: "2.0",
        id: request.id,
        error: None,
        result: Some(
          Js.Json.object_(
            Js.Dict.fromArray([
              (
                "content",
                Js.Json.array([
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      ("type", Js.Json.string("text")),
                      (
                        "text",
                        Js.Json.string(
                          connected
                            ? `Successfully connected to ${adapter.name}`
                            : `Failed to connect to ${adapter.name} - binary not found`,
                        ),
                      ),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ),
      }
    }
  } else {
    // Find adapter and tool
    let parts = Js.String2.split(toolName, "_")
    let adapterName = parts->Belt.Array.get(0)->Belt.Option.getWithDefault("")

    switch Js.Dict.get(server.adapters, adapterName) {
    | None => {
        jsonrpc: "2.0",
        id: request.id,
        result: None,
        error: Some({"code": -32602, "message": `Unknown adapter: ${adapterName}`}),
      }
    | Some(adapter) =>
      let toolOpt = adapter.tools->Belt.Array.getBy(t => t.name == toolName)

      switch toolOpt {
      | None => {
          jsonrpc: "2.0",
          id: request.id,
          result: None,
          error: Some({"code": -32602, "message": `Unknown tool: ${toolName}`}),
        }
      | Some(tool) =>
        let argsJson =
          args
          ->Js.Dict.entries
          ->Belt.Array.map(((k, v)) => (k, v))
          ->Js.Dict.fromArray
        let result = await tool.execute(argsJson)

        {
          jsonrpc: "2.0",
          id: request.id,
          error: None,
          result: Some(
            Js.Json.object_(
              Js.Dict.fromArray([
                (
                  "content",
                  Js.Json.array([
                    Js.Json.object_(
                      Js.Dict.fromArray([
                        ("type", Js.Json.string("text")),
                        (
                          "text",
                          Js.Json.string(
                            result.success
                              ? result.stdout == "" ? "Command executed successfully" : result.stdout
                              : `Error (code ${Belt.Int.toString(result.code)}): ${result.stderr == ""
                                    ? "Unknown error"
                                    : result.stderr}`,
                          ),
                        ),
                      ]),
                    ),
                  ]),
                ),
                ("isError", Js.Json.boolean(!result.success)),
              ]),
            ),
          ),
        }
      }
    }
  }
}

let handleResourcesList = (server: mcpServer, request: mcpRequest): mcpResponse => {
  let resources =
    Js.Dict.entries(server.adapters)->Belt.Array.map(((name, adapter)) => {
      Js.Json.object_(
        Js.Dict.fromArray([
          ("uri", Js.Json.string(`odd-ssg://adapters/${name}`)),
          ("name", Js.Json.string(adapter.name)),
          ("mimeType", Js.Json.string("application/json")),
          ("description", Js.Json.string(adapter.description)),
        ]),
      )
    })

  {
    jsonrpc: "2.0",
    id: request.id,
    error: None,
    result: Some(Js.Json.object_(Js.Dict.fromArray([("resources", Js.Json.array(resources))]))),
  }
}

let handleResourcesRead = async (
  server: mcpServer,
  request: mcpRequest,
): mcpResponse => {
  let params = request.params->Belt.Option.getWithDefault(Js.Dict.empty())
  let uri =
    Js.Dict.get(params, "uri")
    ->Belt.Option.flatMap(Js.Json.decodeString)
    ->Belt.Option.getWithDefault("")

  let uriRegex = %re("/^odd-ssg:\/\/adapters\/(.+)$/")
  switch Js.Re.exec_(uriRegex, uri) {
  | None => {
      jsonrpc: "2.0",
      id: request.id,
      result: None,
      error: Some({"code": -32602, "message": `Invalid URI: ${uri}`}),
    }
  | Some(result) =>
    let captures = Js.Re.captures(result)
    let adapterName =
      captures->Belt.Array.get(1)->Belt.Option.flatMap(Js.Nullable.toOption)->Belt.Option.getWithDefault(
        "",
      )

    switch Js.Dict.get(server.adapters, adapterName) {
    | None => {
        jsonrpc: "2.0",
        id: request.id,
        result: None,
        error: Some({"code": -32602, "message": `Unknown adapter: ${adapterName}`}),
      }
    | Some(adapter) =>
      let content = Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string(adapter.name)),
          ("language", Js.Json.string(adapter.language)),
          ("description", Js.Json.string(adapter.description)),
          (
            "connected",
            Js.Json.boolean(
              Js.Dict.get(server.connectedAdapters, adapterName)->Belt.Option.getWithDefault(false),
            ),
          ),
          (
            "tools",
            Js.Json.array(
              adapter.tools->Belt.Array.map(t =>
                Js.Json.object_(
                  Js.Dict.fromArray([
                    ("name", Js.Json.string(t.name)),
                    ("description", Js.Json.string(t.description)),
                  ]),
                )
              ),
            ),
          ),
        ]),
      )

      {
        jsonrpc: "2.0",
        id: request.id,
        error: None,
        result: Some(
          Js.Json.object_(
            Js.Dict.fromArray([
              (
                "contents",
                Js.Json.array([
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      ("uri", Js.Json.string(uri)),
                      ("mimeType", Js.Json.string("application/json")),
                      ("text", Js.Json.string(Js.Json.stringify(content))),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ),
      }
    }
  }
}

let handleRequest = async (server: mcpServer, request: mcpRequest): mcpResponse => {
  switch request.method {
  | "initialize" => handleInitialize(request)
  | "tools/list" => handleToolsList(server, request)
  | "tools/call" => await handleToolsCall(server, request)
  | "resources/list" => handleResourcesList(server, request)
  | "resources/read" => await handleResourcesRead(server, request)
  | method => {
      jsonrpc: "2.0",
      id: request.id,
      result: None,
      error: Some({"code": -32601, "message": `Method not found: ${method}`}),
    }
  }
}
