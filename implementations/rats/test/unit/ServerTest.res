// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// ServerTest.res - Unit tests for MCP server

// Deno test bindings
@scope("Deno") @val
external test: (string, @uncurry (unit => promise<unit>)) => unit = "test"

// Assertions
module Assert = {
  @module("@std/assert") external assertEquals: ('a, 'a, ~msg: string=?) => unit = "assertEquals"
  @module("@std/assert") external assertExists: ('a, ~msg: string=?) => unit = "assertExists"
}

// Test: Config module exports
let _ = test("server - config module", async () => {
  let config = Config.make()

  Assert.assertExists(config.port, ~msg="port should exist")
  Assert.assertExists(config.host, ~msg="host should exist")
  Assert.assertExists(config.logLevel, ~msg="logLevel should exist")
})

// Test: Config defaults
let _ = test("server - config defaults", async () => {
  let config = Config.make()

  Assert.assertEquals(config.port, 3000, ~msg="Default port should be 3000")
  Assert.assertEquals(config.host, "127.0.0.1", ~msg="Default host should be 127.0.0.1")
  Assert.assertEquals(config.logLevel, "info", ~msg="Default logLevel should be info")
})

// Test: Logger levels
let _ = test("server - logger levels", async () => {
  Assert.assertEquals(Types.logLevelToInt(Types.Debug), 0, ~msg="Debug should be 0")
  Assert.assertEquals(Types.logLevelToInt(Types.Info), 1, ~msg="Info should be 1")
  Assert.assertEquals(Types.logLevelToInt(Types.Warn), 2, ~msg="Warn should be 2")
  Assert.assertEquals(Types.logLevelToInt(Types.Error), 3, ~msg="Error should be 3")
})

// Test: Logger level parsing
let _ = test("server - logger level parsing", async () => {
  Assert.assertEquals(
    Types.logLevelFromString("debug"),
    Types.Debug,
    ~msg="debug string should parse",
  )
  Assert.assertEquals(
    Types.logLevelFromString("invalid"),
    Types.Info,
    ~msg="invalid should default to Info",
  )
})

// Test: Adapter registry initialization
let _ = test("server - adapter registry", async () => {
  Assert.assertEquals(AdapterRegistry.adapterCount(), 0, ~msg="Initial adapter count should be 0")
  Assert.assertEquals(AdapterRegistry.toolCount(), 0, ~msg="Initial tool count should be 0")
})

// Test: MCP protocol version
let _ = test("server - MCP protocol version", async () => {
  Assert.assertEquals(
    Mcp.Protocol.protocolVersion,
    "2024-11-05",
    ~msg="Protocol version should be 2024-11-05",
  )
  Assert.assertEquals(
    Mcp.Protocol.serverName,
    "rats-ssg",
    ~msg="Server name should be rats-ssg",
  )
  Assert.assertEquals(
    Mcp.Protocol.serverVersion,
    "0.2.0",
    ~msg="Server version should be 0.2.0",
  )
})
