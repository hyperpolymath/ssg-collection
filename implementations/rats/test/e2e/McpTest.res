// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// McpTest.res - End-to-end tests for MCP protocol

// Deno test bindings
@scope("Deno") @val
external test: (string, @uncurry (unit => promise<unit>)) => unit = "test"

// Assertions
module Assert = {
  @module("@std/assert") external assertEquals: ('a, 'a, ~msg: string=?) => unit = "assertEquals"
  @module("@std/assert") external assertExists: ('a, ~msg: string=?) => unit = "assertExists"
}

// Test: Initialize response structure
let _ = test("e2e - initialize response", async () => {
  let result = Mcp.Protocol.makeInitializeResult()

  Assert.assertExists(result, ~msg="Initialize result should exist")

  // Check it's valid JSON
  let jsonStr = Js.Json.stringify(result)
  Assert.assertExists(jsonStr, ~msg="Should serialize to JSON")
})

// Test: Tools list response structure
let _ = test("e2e - tools list response", async () => {
  let result = Mcp.Protocol.makeToolsListResult()

  Assert.assertExists(result, ~msg="Tools list result should exist")
})

// Test: Resources list response structure
let _ = test("e2e - resources list response", async () => {
  let result = Mcp.Protocol.makeResourcesListResult()

  Assert.assertExists(result, ~msg="Resources list result should exist")
})

// Test: Success response creation
let _ = test("e2e - success response", async () => {
  let id = Js.Json.number(1.0)
  let result = Js.Json.object_(Js.Dict.empty())

  let response = Mcp.Protocol.makeSuccessResponse(id, result)

  Assert.assertEquals(response.jsonrpc, "2.0", ~msg="jsonrpc should be 2.0")
  Assert.assertExists(response.result, ~msg="result should exist")
  Assert.assertEquals(response.error, None, ~msg="error should be None")
})

// Test: Error response creation
let _ = test("e2e - error response", async () => {
  let id = Js.Json.number(1.0)

  let response = Mcp.Protocol.makeErrorResponse(id, -32601, "Method not found")

  Assert.assertEquals(response.jsonrpc, "2.0", ~msg="jsonrpc should be 2.0")
  Assert.assertEquals(response.result, None, ~msg="result should be None")
  Assert.assertExists(response.error, ~msg="error should exist")
})

// Test: Response serialization
let _ = test("e2e - response serialization", async () => {
  let id = Js.Json.number(1.0)
  let result = Js.Json.string("test")

  let response = Mcp.Protocol.makeSuccessResponse(id, result)
  let serialized = Mcp.Protocol.serializeResponse(response)

  Assert.assertExists(serialized, ~msg="Serialized response should exist")

  // Should be valid JSON
  let parsed = Js.Json.parseExn(serialized)
  Assert.assertExists(parsed, ~msg="Should parse back to JSON")
})

// Test: Tool call param parsing - valid
let _ = test("e2e - tool call param parsing valid", async () => {
  let params = Js.Dict.fromArray([
    ("name", Js.Json.string("zola_init")),
    ("arguments", Js.Json.object_(Js.Dict.empty())),
  ])

  let result = Mcp.ToolCall.parseParams(Some(params))

  switch result {
  | Ok(parsed) => Assert.assertEquals(parsed.name, "zola_init", ~msg="Name should match")
  | Error(_) => Assert.assertEquals(true, false, ~msg="Should parse successfully")
  }
})

// Test: Tool call param parsing - missing name
let _ = test("e2e - tool call param parsing missing name", async () => {
  let params = Js.Dict.fromArray([("arguments", Js.Json.object_(Js.Dict.empty()))])

  let result = Mcp.ToolCall.parseParams(Some(params))

  switch result {
  | Ok(_) => Assert.assertEquals(true, false, ~msg="Should fail without name")
  | Error(msg) => Assert.assertExists(msg, ~msg="Error message should exist")
  }
})

// Test: Handler - unknown method
let _ = test("e2e - handler unknown method", async () => {
  let request: Types.mcpRequest = {
    jsonrpc: "2.0",
    id: Js.Json.number(1.0),
    method: "unknown/method",
    params: None,
  }

  let response = await Mcp.Handler.handle(request)

  Assert.assertExists(response.error, ~msg="Should return error for unknown method")
})

// Test: Handler - initialize
let _ = test("e2e - handler initialize", async () => {
  let request: Types.mcpRequest = {
    jsonrpc: "2.0",
    id: Js.Json.number(1.0),
    method: "initialize",
    params: None,
  }

  let response = await Mcp.Handler.handle(request)

  Assert.assertExists(response.result, ~msg="Initialize should return result")
  Assert.assertEquals(response.error, None, ~msg="Initialize should not error")
})

// Test: Handler - tools/list
let _ = test("e2e - handler tools list", async () => {
  let request: Types.mcpRequest = {
    jsonrpc: "2.0",
    id: Js.Json.number(1.0),
    method: "tools/list",
    params: None,
  }

  let response = await Mcp.Handler.handle(request)

  Assert.assertExists(response.result, ~msg="tools/list should return result")
})

// Test: Handler - resources/list
let _ = test("e2e - handler resources list", async () => {
  let request: Types.mcpRequest = {
    jsonrpc: "2.0",
    id: Js.Json.number(1.0),
    method: "resources/list",
    params: None,
  }

  let response = await Mcp.Handler.handle(request)

  Assert.assertExists(response.result, ~msg="resources/list should return result")
})
