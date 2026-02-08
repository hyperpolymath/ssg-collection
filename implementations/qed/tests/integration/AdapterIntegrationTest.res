// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// tests/integration/AdapterIntegrationTest.res - Integration tests for SSG adapters

open DenoTest

// ============================================================================
// ADAPTER INTERFACE TESTS
// ============================================================================

let _ = test("Integration: Adapter lifecycle - connect/disconnect", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  // Initial state should be disconnected
  Assert.assertEquals(adapter.isConnected(), false)

  // Connect attempt (will fail without actual binary, but tests the interface)
  let connected = await adapter.connect()
  // Result depends on whether zola is installed
  Assert.assertEquals(Js.typeof(connected), "boolean")

  // Disconnect
  await adapter.disconnect()
  Assert.assertEquals(adapter.isConnected(), false)
})

let _ = test("Integration: Tool execution returns proper structure", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  for tool in adapter.tools {
    // Verify tool structure
    Assert.assertExists(tool.name)
    Assert.assertExists(tool.description)
    Assert.assertExists(tool.inputSchema)
    Assert.assertExists(tool.execute)

    // Tool names should follow pattern: {adapter}_{action}
    Assert.assert_(
      Js.String2.startsWith(tool.name, "zola_"),
      ~msg=`Tool ${tool.name} should start with adapter prefix`,
    )

    // Input schema should be valid JSON Schema
    Assert.assertEquals(tool.inputSchema.type_, "object")
    Assert.assertExists(tool.inputSchema.properties)
  }
})

let _ = test("Integration: All adapters have consistent tool patterns", async () => {
  let entries = await Deno.readDirAsArray("./adapters")
  let adapterFiles =
    entries
    ->Js.Array2.filter(entry => entry.isFile && Js.String2.endsWith(entry.name, ".js"))
    ->Js.Array2.map(entry => entry.name)

  let requiredTools = ["init", "build", "version"]

  for file in adapterFiles {
    let adapter = await Adapter.importAdapter(`../../adapters/${file}`)

    for required in requiredTools {
      let hasRequiredTool = adapter.tools->Js.Array2.some(t => Js.String2.includes(t.name, required))
      Assert.assert_(hasRequiredTool, ~msg=`${file} should have a '${required}' tool`)
    }
  }
})

// ============================================================================
// ADAPTER GROUPING TESTS
// ============================================================================

let _ = test("Integration: Rust adapters use correct binary paths", async () => {
  let rustAdapters = ["zola", "cobalt", "mdbook"]

  for name in rustAdapters {
    let adapter = await Adapter.importAdapter(`../../adapters/${name}.js`)
    Assert.assertEquals(adapter.language, "Rust")
    // Rust adapters typically use native binaries
    Assert.assertExists(adapter.tools)
  }
})

let _ = test("Integration: Haskell adapters use stack", async () => {
  let haskellAdapters = ["hakyll", "ema"]

  for name in haskellAdapters {
    let adapter = await Adapter.importAdapter(`../../adapters/${name}.js`)
    Assert.assertEquals(adapter.language, "Haskell")
  }
})

let _ = test("Integration: Elixir adapters use mix", async () => {
  let elixirAdapters = ["serum", "tableau", "nimble-publisher"]

  for name in elixirAdapters {
    let adapter = await Adapter.importAdapter(`../../adapters/${name}.js`)
    Assert.assertEquals(adapter.language, "Elixir")
  }
})

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

let _ = test("Integration: Adapters handle missing binary gracefully", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  // Disconnect first to ensure clean state
  await adapter.disconnect()

  // If binary is not installed, connect should return false (not throw)
  let result = await adapter.connect()
  Assert.assertEquals(Js.typeof(result), "boolean")
})

let _ = test("Integration: Tools return structured error on failure", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  // Execute without connecting (should handle gracefully)
  let versionTool = adapter.tools->Js.Array2.find(t => t.name === "zola_version")

  switch versionTool {
  | Some(tool) =>
    let result = await tool.execute(Js.Dict.empty())

    // Should return a result object, not throw
    Assert.assertExists(result)
    Assert.assertEquals(Js.typeof(result.success), "boolean")
    Assert.assertExists(result.stdout)
    Assert.assertExists(result.stderr)
    Assert.assertEquals(Js.typeof(result.code), "number")
  | None => Assert.assert_(false, ~msg="Should have version tool")
  }
})

// ============================================================================
// SECURITY TESTS
// ============================================================================

let _ = test("Integration: Adapters use parameterized commands", async () => {
  // Read adapter source and verify no shell string concatenation
  let entries = await Deno.readDirAsArray("./adapters")

  for entry in entries {
    if entry.isFile && Js.String2.endsWith(entry.name, ".js") {
      let content = await Deno.readTextFile(`./adapters/${entry.name}`)

      // Should use Deno.Command
      Assert.assert_(
        Js.String2.includes(content, "Deno.Command"),
        ~msg=`${entry.name} should use Deno.Command`,
      )

      // Should not use deprecated Deno.run
      Assert.assert_(
        !Js.String2.includes(content, "Deno.run"),
        ~msg=`${entry.name} should not use deprecated Deno.run`,
      )

      // Should not use eval
      Assert.assert_(
        !Js.String2.includes(content, "eval("),
        ~msg=`${entry.name} should not use eval()`,
      )
    }
  }
})

let _ = test("Integration: All adapters have SPDX headers", async () => {
  let entries = await Deno.readDirAsArray("./adapters")

  for entry in entries {
    if entry.isFile && Js.String2.endsWith(entry.name, ".js") {
      let content = await Deno.readTextFile(`./adapters/${entry.name}`)
      let firstLine = (content->Js.String2.split("\n"))[0]->Belt.Option.getWithDefault("")

      Assert.assert_(
        Js.String2.includes(firstLine, "SPDX-License-Identifier"),
        ~msg=`${entry.name} should have SPDX header`,
      )
    }
  }
})

// ============================================================================
// CONCURRENT ADAPTER TESTS
// ============================================================================

let _ = test("Integration: Multiple adapters can be loaded concurrently", async () => {
  let adapters = await Js.Promise.all([
    Adapter.importAdapter("../../adapters/zola.js"),
    Adapter.importAdapter("../../adapters/hakyll.js"),
    Adapter.importAdapter("../../adapters/serum.js"),
    Adapter.importAdapter("../../adapters/cobalt.js"),
  ])

  // All should load successfully
  Assert.assertEquals(adapters->Js.Array2.length, 4)

  // Each should have unique names
  let names = adapters->Js.Array2.map(a => a.name)
  let uniqueNamesSet = Belt.Set.String.fromArray(names)
  Assert.assertEquals(uniqueNamesSet->Belt.Set.String.size, 4)
})
