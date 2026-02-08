// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// tests/e2e/FullWorkflowTest.res - End-to-end workflow tests

open DenoTest

// ============================================================================
// TEST UTILITIES
// ============================================================================

let testDir = "/tmp/qed-ssg-e2e-tests"

let setupTestDir = async (): string => {
  let timestamp = Js.Date.now()->Js.Float.toString
  let testPath = `${testDir}/${timestamp}`
  await Deno.mkdir(testPath, {"recursive": true})
  testPath
}

let cleanupTestDir = async (path: string): unit => {
  try {
    await Deno.remove(path, {"recursive": true})
  } catch {
  | _ => () // Ignore cleanup errors
  }
}

let checkBinaryExists = async (binary: string): bool => {
  try {
    let cmd = Deno.Command.make("which", {args: [binary]})
    let output = await cmd->Deno.Command.output
    output.success
  } catch {
  | _ => false
  }
}

// ============================================================================
// ADAPTER LOADING E2E TESTS
// ============================================================================

let _ = test("E2E: Load all 28 adapters successfully", async () => {
  let entries = await Deno.readDirAsArray("./adapters")
  let adapterFiles =
    entries
    ->Js.Array2.filter(entry => entry.isFile && Js.String2.endsWith(entry.name, ".js"))
    ->Js.Array2.map(entry => entry.name)

  Assert.assertEquals(adapterFiles->Js.Array2.length, 28, ~msg="Should have exactly 28 adapter files")

  // Load all adapters and check they all succeed
  let loadPromises = adapterFiles->Js.Array2.map(file => Adapter.importAdapter(`../../adapters/${file}`))

  let results = await Js.Promise.all(loadPromises)
  Assert.assertEquals(results->Js.Array2.length, 28, ~msg="All 28 adapters should load successfully")
})

let _ = test("E2E: Adapter tools are executable", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  // Get version tool
  let versionTool = adapter.tools->Js.Array2.find(t => t.name === "zola_version")

  switch versionTool {
  | Some(tool) =>
    // Execute the tool (will fail if binary not installed, but should not throw)
    let result = await tool.execute(Js.Dict.empty())

    Assert.assertExists(result)
    Assert.assertExists(result.success, ~msg="Result should have success property")
    Assert.assertExists(result.stdout, ~msg="Result should have stdout")
    Assert.assertExists(result.stderr, ~msg="Result should have stderr")
    Assert.assertExists(result.code, ~msg="Result should have exit code")
  | None => Assert.assert_(false, ~msg="Should have version tool")
  }
})

// ============================================================================
// ADAPTER REGISTRY E2E TESTS
// ============================================================================

let _ = test("E2E: Verify adapter language distribution", async () => {
  let entries = await Deno.readDirAsArray("./adapters")
  let languageCount = Js.Dict.empty()

  for entry in entries {
    if entry.isFile && Js.String2.endsWith(entry.name, ".js") {
      let adapter = await Adapter.importAdapter(`../../adapters/${entry.name}`)
      let lang = adapter.language
      let currentCount = switch languageCount->Js.Dict.get(lang) {
      | Some(n) => n
      | None => 0
      }
      languageCount->Js.Dict.set(lang, currentCount + 1)
    }
  }

  // Verify expected language distribution
  Assert.assertEquals(
    languageCount->Js.Dict.get("Rust")->Belt.Option.getWithDefault(0),
    3,
    ~msg="Should have 3 Rust adapters",
  )
  Assert.assertEquals(
    languageCount->Js.Dict.get("Haskell")->Belt.Option.getWithDefault(0),
    2,
    ~msg="Should have 2 Haskell adapters",
  )
  Assert.assertEquals(
    languageCount->Js.Dict.get("Elixir")->Belt.Option.getWithDefault(0),
    3,
    ~msg="Should have 3 Elixir adapters",
  )

  // Total should be 28
  let total =
    languageCount->Js.Dict.values->Js.Array2.fromIterator->Js.Array2.reduce((a, b) => a + b, 0)
  Assert.assertEquals(total, 28, ~msg="Total adapters should be 28")
})

let _ = test("E2E: All adapters have minimum required tools", async () => {
  let minimumTools = 4 // init, build, serve/version, at least one more

  let entries = await Deno.readDirAsArray("./adapters")

  for entry in entries {
    if entry.isFile && Js.String2.endsWith(entry.name, ".js") {
      let adapter = await Adapter.importAdapter(`../../adapters/${entry.name}`)

      Assert.assert_(
        adapter.tools->Js.Array2.length >= minimumTools,
        ~msg=`${entry.name} should have at least ${minimumTools->Js.Int.toString} tools, has ${adapter.tools->Js.Array2.length->Js.Int.toString}`,
      )
    }
  }
})

// ============================================================================
// CONCURRENT OPERATIONS E2E TESTS
// ============================================================================

let _ = test("E2E: Concurrent adapter operations", async () => {
  let adapters = await Js.Promise.all([
    Adapter.importAdapter("../../adapters/zola.js"),
    Adapter.importAdapter("../../adapters/cobalt.js"),
    Adapter.importAdapter("../../adapters/mdbook.js"),
  ])

  // Connect all concurrently
  let connections = await Js.Promise.all(adapters->Js.Array2.map(adapter => adapter.connect()))

  // All should complete (success depends on binary availability)
  Assert.assertEquals(connections->Js.Array2.length, 3)
  connections->Js.Array2.forEach(result => Assert.assertEquals(Js.typeof(result), "boolean"))

  // Disconnect all
  let _ = await Js.Promise.all(adapters->Js.Array2.map(adapter => adapter.disconnect()))

  // Verify all disconnected
  adapters->Js.Array2.forEach(adapter => Assert.assertEquals(adapter.isConnected(), false))
})

// ============================================================================
// ERROR RECOVERY E2E TESTS
// ============================================================================

let _ = test("E2E: Recover from failed connection", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  // First disconnect to ensure clean state
  await adapter.disconnect()
  Assert.assertEquals(adapter.isConnected(), false)

  // Try to connect (may fail if binary not installed)
  let firstAttempt = await adapter.connect()

  // Disconnect
  await adapter.disconnect()
  Assert.assertEquals(adapter.isConnected(), false)

  // Try again - should work the same way
  let secondAttempt = await adapter.connect()

  // Both attempts should behave consistently
  Assert.assertEquals(Js.typeof(firstAttempt), Js.typeof(secondAttempt))

  await adapter.disconnect()
})

let _ = test("E2E: Tool execution with invalid input", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  let buildTool = adapter.tools->Js.Array2.find(t => t.name === "zola_build")

  switch buildTool {
  | Some(tool) =>
    // Execute with non-existent path
    let args = Js.Dict.empty()
    args->Js.Dict.set("path", "/nonexistent/path/12345")
    let result = await tool.execute(args)

    // Should return error result, not throw
    Assert.assertExists(result)
    Assert.assertEquals(result.success, false)
  | None => Assert.assert_(false, ~msg="Should have build tool")
  }
})
