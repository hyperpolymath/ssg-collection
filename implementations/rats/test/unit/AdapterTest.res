// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// AdapterTest.res - Unit tests for SSG adapters

// Deno test bindings
@scope("Deno") @val
external test: (string, @uncurry (unit => promise<unit>)) => unit = "test"

@scope("Deno") @val
external testWithSteps: (string, @uncurry ('t => promise<unit>)) => unit = "test"

// Test step
type testContext
@send
external step: (testContext, string, @uncurry (unit => promise<unit>)) => promise<unit> = "step"

// Assertions
module Assert = {
  @module("@std/assert") external assertEquals: ('a, 'a, ~msg: string=?) => unit = "assertEquals"
  @module("@std/assert") external assertExists: ('a, ~msg: string=?) => unit = "assertExists"
}

// External: Read adapters directory
@val
external readAdaptersDir: unit => promise<array<string>> = "readAdaptersDir"

// External: Load adapter
@val
external loadAdapter: string => promise<Types.adapter> = "loadAdapter"

// External: Read file content
@val
external readAdapterContent: string => promise<string> = "readAdapterContent"

// Test: Required exports
let _ = test("adapters - registry functions", async () => {
  Assert.assertExists(AdapterRegistry.getAdapter, ~msg="getAdapter should exist")
  Assert.assertExists(AdapterRegistry.getTool, ~msg="getTool should exist")
  Assert.assertExists(AdapterRegistry.registerAdapter, ~msg="registerAdapter should exist")
})

// Test: Tool naming convention
let _ = test("adapters - tool naming pattern", async () => {
  let pattern = %re("/^[a-z]+_[a-z_]+$/")

  // Test valid patterns
  Assert.assertEquals(
    Js.Re.test_(pattern, "zola_init"),
    true,
    ~msg="zola_init should match",
  )
  Assert.assertEquals(
    Js.Re.test_(pattern, "hakyll_build"),
    true,
    ~msg="hakyll_build should match",
  )

  // Test invalid patterns
  Assert.assertEquals(
    Js.Re.test_(pattern, "ZolaInit"),
    false,
    ~msg="ZolaInit should not match",
  )
})

// Test: Input schema structure
let _ = test("adapters - input schema type", async () => {
  let schema: Types.inputSchema = {
    schemaType: "object",
    properties: Js.Dict.empty(),
    required: None,
  }

  Assert.assertEquals(schema.schemaType, "object", ~msg="Schema type should be object")
})

// Test: Command result structure
let _ = test("adapters - command result structure", async () => {
  let result: Types.commandResult = {
    success: true,
    stdout: "output",
    stderr: "",
    code: 0,
  }

  Assert.assertEquals(result.success, true, ~msg="Success should be true")
  Assert.assertEquals(result.code, 0, ~msg="Code should be 0")
})

// Test: Adapter structure
let _ = test("adapters - adapter interface", async () => {
  let mockAdapter: Types.adapter = {
    name: "test",
    language: "Test",
    description: "Test adapter",
    tools: [],
    connect: async () => true,
    disconnect: async () => (),
    isConnected: () => false,
  }

  Assert.assertEquals(mockAdapter.name, "test", ~msg="Name should match")
  Assert.assertEquals(mockAdapter.language, "Test", ~msg="Language should match")
  Assert.assertEquals(Array.length(mockAdapter.tools), 0, ~msg="Tools should be empty")
})
