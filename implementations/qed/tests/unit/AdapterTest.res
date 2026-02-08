// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// tests/unit/AdapterTest.res - Unit tests for SSG adapters

open DenoTest

// Test adapter module structure
let _ = test("adapters have required exports", async () => {
  let entries = await Deno.readDirAsArray("./adapters")
  let adapterFiles =
    entries->Js.Array2.filter(entry => entry.isFile && Adapter.isAdapterFile(entry.name))

  for file in adapterFiles {
    let adapter = await Adapter.importAdapter(`../adapters/${file.name}`)

    // Check required exports
    Assert.assertExists(adapter.name, ~msg=`${file.name} should export 'name'`)
    Assert.assertExists(adapter.language, ~msg=`${file.name} should export 'language'`)
    Assert.assertExists(adapter.description, ~msg=`${file.name} should export 'description'`)
    Assert.assertExists(adapter.tools, ~msg=`${file.name} should export 'tools'`)
    Assert.assertExists(adapter.connect, ~msg=`${file.name} should export 'connect'`)
    Assert.assertExists(adapter.disconnect, ~msg=`${file.name} should export 'disconnect'`)
    Assert.assertExists(adapter.isConnected, ~msg=`${file.name} should export 'isConnected'`)

    // Check types
    Assert.assertEquals(Js.typeof(adapter.name), "string", ~msg=`${file.name}.name should be a string`)
    Assert.assertEquals(
      Js.typeof(adapter.language),
      "string",
      ~msg=`${file.name}.language should be a string`,
    )
    Assert.assertEquals(
      Js.typeof(adapter.description),
      "string",
      ~msg=`${file.name}.description should be a string`,
    )
    Assert.assert_(Js.Array2.isArray(adapter.tools), ~msg=`${file.name}.tools should be an array`)
    Assert.assertEquals(
      Js.typeof(adapter.connect),
      "function",
      ~msg=`${file.name}.connect should be a function`,
    )
    Assert.assertEquals(
      Js.typeof(adapter.disconnect),
      "function",
      ~msg=`${file.name}.disconnect should be a function`,
    )
    Assert.assertEquals(
      Js.typeof(adapter.isConnected),
      "function",
      ~msg=`${file.name}.isConnected should be a function`,
    )
  }
})

// Test tool structure
let _ = test("adapter tools have correct structure", async () => {
  let adapter = await Adapter.importAdapter("../adapters/zola.js")

  for tool in adapter.tools {
    Assert.assertExists(tool.name, ~msg="Tool should have 'name'")
    Assert.assertExists(tool.description, ~msg="Tool should have 'description'")
    Assert.assertExists(tool.inputSchema, ~msg="Tool should have 'inputSchema'")
    Assert.assertExists(tool.execute, ~msg="Tool should have 'execute'")

    Assert.assertEquals(Js.typeof(tool.name), "string")
    Assert.assertEquals(Js.typeof(tool.description), "string")
    Assert.assertEquals(Js.typeof(tool.execute), "function")
  }
})

// Test adapter count
let _ = test("28 adapters are available", async () => {
  let entries = await Deno.readDirAsArray("./adapters")
  let count =
    entries->Js.Array2.filter(entry => entry.isFile && Js.String2.endsWith(entry.name, ".js"))
    ->Js.Array2.length

  Assert.assertEquals(count, 28, ~msg="Should have exactly 28 adapters")
})

// Test isConnected returns boolean
let _ = test("isConnected returns boolean", async () => {
  let adapter = await Adapter.importAdapter("../adapters/zola.js")
  let result = adapter.isConnected()
  Assert.assertEquals(Js.typeof(result), "boolean")
})
