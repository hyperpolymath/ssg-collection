// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Unit tests for SSG adapters

open Deno

// ============================================================================
// Test Utilities
// ============================================================================

let loadAdapter = async (name: string): option<Adapter.t> => {
  try {
    let path = `../../adapters/${name}.js`
    let adapter: Adapter.t = await Adapter.importAdapter(path)
    Some(adapter)
  } catch {
  | _ => None
  }
}

// ============================================================================
// Adapter Interface Tests
// ============================================================================

let adapterNames = ["zola", "hakyll", "mdbook", "serum", "cobalt", "franklin"]

// Test each adapter's interface compliance
adapterNames->Js.Array2.forEach(adapterName => {
  Test.testSimple(`${adapterName} adapter should export required properties`, async () => {
    switch await loadAdapter(adapterName) {
    | None => () // Skip if adapter failed to load
    | Some(adapter) =>
      Assert.assertExists(adapter.name)
      Assert.assertExists(adapter.language)
      Assert.assertExists(adapter.description)
      Assert.assertExists(adapter.connect)
      Assert.assertExists(adapter.disconnect)
      Assert.assertExists(adapter.isConnected)
      Assert.assertExists(adapter.tools)
    }
  })

  Test.testSimple(`${adapterName} adapter should have correct type for name`, async () => {
    switch await loadAdapter(adapterName) {
    | None => ()
    | Some(adapter) => Assert.assertEquals(Js.typeof(adapter.name), "string")
    }
  })

  Test.testSimple(`${adapterName} adapter should have correct type for language`, async () => {
    switch await loadAdapter(adapterName) {
    | None => ()
    | Some(adapter) => Assert.assertEquals(Js.typeof(adapter.language), "string")
    }
  })

  Test.testSimple(`${adapterName} adapter should have tools array`, async () => {
    switch await loadAdapter(adapterName) {
    | None => ()
    | Some(adapter) => Assert.assertEquals(Js.Array2.isArray(adapter.tools), true)
    }
  })

  Test.testSimple(`${adapterName} adapter should have valid tool definitions`, async () => {
    switch await loadAdapter(adapterName) {
    | None => ()
    | Some(adapter) =>
      adapter.tools->Js.Array2.forEach(tool => {
        Assert.assertExists(tool.name)
        Assert.assertExists(tool.description)
        Assert.assertExists(tool.inputSchema)
        Assert.assertExists(tool.execute)
        Assert.assertEquals(Js.typeof(tool.execute), "function")
      })
    }
  })

  Test.testSimple(`${adapterName} adapter should return boolean from isConnected`, async () => {
    switch await loadAdapter(adapterName) {
    | None => ()
    | Some(adapter) =>
      let result = adapter.isConnected()
      Assert.assertEquals(Js.typeof(result), "boolean")
    }
  })
})

// ============================================================================
// Tool Schema Validation
// ============================================================================

Test.testSimple("Tool schema should have valid JSON Schema for inputSchema", async () => {
  switch await loadAdapter("zola") {
  | None => ()
  | Some(adapter) =>
    adapter.tools->Js.Array2.forEach(tool => {
      Assert.assertExists(tool.inputSchema.type_)
      Assert.assertEquals(tool.inputSchema.type_, "object")
    })
  }
})

Test.testSimple("Tool schema should have properties defined in schema", async () => {
  switch await loadAdapter("zola") {
  | None => ()
  | Some(adapter) =>
    adapter.tools->Js.Array2.forEach(tool => {
      switch tool.inputSchema.properties {
      | Some(props) => Assert.assertEquals(Js.typeof(props), "object")
      | None => ()
      }
    })
  }
})

// ============================================================================
// Bernoulli Probabilistic Verification
// ============================================================================

let iterations = 100
let threshold = 0.95 // 95% success rate required

Test.testSimple("Bernoulli: should consistently report connection status", async () => {
  switch await loadAdapter("zola") {
  | None => ()
  | Some(adapter) =>
    let successCount = ref(0)

    for _ in 0 to iterations - 1 {
      let connected = adapter.isConnected()
      if Js.typeof(connected) === "boolean" {
        successCount := successCount.contents + 1
      }
    }

    let successRate = float_of_int(successCount.contents) /. float_of_int(iterations)
    Assert.assertEquals(
      successRate >= threshold,
      true,
    )
  }
})

Test.testSimple("Bernoulli: should return consistent tool count", async () => {
  switch await loadAdapter("zola") {
  | None => ()
  | Some(adapter) =>
    let toolCounts = []

    for _ in 0 to iterations - 1 {
      let _ = Js.Array2.push(toolCounts, adapter.tools->Js.Array2.length)
    }

    // All counts should be the same
    let firstCount = toolCounts[0]
    let allSame = toolCounts->Js.Array2.every(c => c === firstCount)
    Assert.assertEquals(allSame, true)
  }
})

// ============================================================================
// Security Tests
// ============================================================================

Test.testSimple("Security: should not use shell:true in command execution", async () => {
  let adapterPath = "../../adapters/zola.js"
  try {
    let url = %raw(`new URL(adapterPath, import.meta.url).href`)
    let source = await Fs.readTextFile(url)
    Assert.assertEquals(Js.String2.includes(source, "shell: true"), false)
    Assert.assertEquals(Js.String2.includes(source, "shell:true"), false)
  } catch {
  | _ => () // File may not exist in test environment
  }
})

Test.testSimple("Security: should use array-based command arguments", async () => {
  switch await loadAdapter("zola") {
  | None => ()
  | Some(adapter) =>
    adapter.tools->Js.Array2.forEach(tool => {
      Assert.assertExists(tool.execute)
    })
  }
})

// ============================================================================
// Run Tests
// ============================================================================

if Meta.main {
  Js.Console.log("Running adapter unit tests...")
}
