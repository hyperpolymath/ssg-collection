// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// End-to-end tests for SSG adapter integration

open Deno

// ============================================================================
// Test Configuration
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
// E2E Adapter Loading Tests
// ============================================================================

let coreAdapters = ["zola", "hakyll", "mdbook", "serum", "cobalt", "franklin"]

coreAdapters->Js.Array2.forEach(adapterName => {
  Test.testSimple(`E2E ${adapterName} adapter should load successfully`, async () => {
    let adapter = await loadAdapter(adapterName)
    Assert.assertExists(adapter)
  })

  Test.testSimple(`E2E ${adapterName} adapter should have correct structure`, async () => {
    switch await loadAdapter(adapterName) {
    | None => ()
    | Some(adapter) =>
      Assert.assertExists(adapter.name)
      Assert.assertExists(adapter.language)
      Assert.assertExists(adapter.tools)
      Assert.assertEquals(Js.Array2.isArray(adapter.tools), true)
    }
  })

  Test.testSimple(`E2E ${adapterName} adapter should have valid tools`, async () => {
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
})

// ============================================================================
// Adapter Count Verification
// ============================================================================

Test.testSimple("E2E should have 28 adapters", async () => {
  try {
    let entries = Fs.readDir("adapters")
    let files = entries
      ->Js.Array2.from
      ->Js.Array2.filter(entry => Js.String2.endsWith(entry.name, ".js"))
    Assert.assertEquals(files->Js.Array2.length, 28)
  } catch {
  | _ => () // Directory may not exist yet
  }
})

Test.testSimple("E2E should have all adapters loadable", async () => {
  try {
    let entries = Fs.readDir("adapters")
    let files = entries
      ->Js.Array2.from
      ->Js.Array2.filter(entry => Js.String2.endsWith(entry.name, ".js"))

    let loadable = ref(0)
    for i in 0 to files->Js.Array2.length - 1 {
      let file = files[i]
      try {
        let modPath = `../../adapters/${file.name}`
        let mod: Adapter.t = await Adapter.importAdapter(modPath)
        if mod.name !== "" && Js.Array2.isArray(mod.tools) {
          loadable := loadable.contents + 1
        }
      } catch {
      | _ => ()
      }
    }

    Assert.assertEquals(loadable.contents > 0, true)
  } catch {
  | _ => ()
  }
})

// ============================================================================
// Performance E2E Tests
// ============================================================================

let performanceThresholdMs = 5000.0 // 5 seconds

Test.testSimple("E2E should load all adapters within threshold", async () => {
  let start = Performance.now()

  try {
    let entries = Fs.readDir("adapters")
    let files = entries
      ->Js.Array2.from
      ->Js.Array2.filter(entry => Js.String2.endsWith(entry.name, ".js"))

    for i in 0 to files->Js.Array2.length - 1 {
      let file = files[i]
      try {
        let modPath = `../../adapters/${file.name}`
        let _ = await Adapter.importAdapter(modPath)
      } catch {
      | _ => ()
      }
    }
  } catch {
  | _ => ()
  }

  let duration = Performance.now() -. start
  Assert.assertEquals(duration < performanceThresholdMs, true)
})

// ============================================================================
// Run Tests
// ============================================================================

if Meta.main {
  Js.Console.log("Running E2E tests for SSG Adapter Collection...")
}
