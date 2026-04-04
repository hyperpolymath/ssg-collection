// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// AdapterIntegrationTest.res — Integration tests for Eclipse SSG adapter chains
//
// Tests that multiple adapters can be composed and that the combined chain
// honours the lifecycle contract: connect -> use -> disconnect.
// All tests are self-contained and do not make network calls.

// ============================================================
// DENO TEST BINDINGS
// ============================================================

@scope("Deno") @val
external test: (string, unit => unit) => unit = "test"

// ============================================================
// ASSERTION HELPERS
// ============================================================

let assertEquals = (a, b, msg) => {
  if a != b {
    Js.Exn.raiseError(
      msg ++ ": expected " ++ Js.Json.stringify(Obj.magic(b)) ++ " but got " ++ Js.Json.stringify(Obj.magic(a)),
    )
  }
}

let assertTrue = (cond: bool, msg: string) => {
  if !cond {
    Js.Exn.raiseError("Assertion failed: " ++ msg)
  }
}

let assertNone = (opt: option<'a>, msg: string) => {
  switch opt {
  | Some(_) => Js.Exn.raiseError("Expected None but got Some: " ++ msg)
  | None => ()
  }
}

let assertSome = (opt: option<'a>, msg: string) => {
  switch opt {
  | None => Js.Exn.raiseError("Expected Some but got None: " ++ msg)
  | Some(_) => ()
  }
}

// ============================================================
// MOCK ADAPTER INFRASTRUCTURE
//
// A minimal in-process adapter model that mirrors the real adapter
// interface without requiring external binaries or network access.
// ============================================================

type toolResult = {
  success: bool,
  stdout: string,
  stderr: string,
  code: int,
}

type tool = {
  name: string,
  description: string,
  execute: Js.Dict.t<string> => toolResult,
}

type adapter = {
  name: string,
  language: string,
  description: string,
  mutable connected: bool,
  tools: array<tool>,
  connect: unit => bool,
  disconnect: unit => unit,
  isConnected: unit => bool,
}

let makeAdapter = (name: string, language: string, tools: array<tool>): adapter => {
  let connectedRef = ref(false)
  {
    name,
    language,
    description: "Mock adapter for " ++ name,
    connected: false,
    tools,
    connect: () => {
      connectedRef := true
      true
    },
    disconnect: () => {
      connectedRef := false
    },
    isConnected: () => connectedRef.contents,
  }
}

// Minimal tool factory
let makeTool = (prefix: string, action: string, willSucceed: bool): tool => {
  {
    name: prefix ++ "_" ++ action,
    description: "Mock " ++ action ++ " tool for " ++ prefix,
    execute: _args => {
      if willSucceed {
        {success: true, stdout: action ++ " completed", stderr: "", code: 0}
      } else {
        {success: false, stdout: "", stderr: action ++ " failed", code: 1}
      }
    },
  }
}

// Build a registry of named adapters
let buildRegistry = (): Js.Dict.t<adapter> => {
  let registry = Js.Dict.empty()

  // Rust-based SSG adapters
  let zolaTools = [
    makeTool("zola", "init", true),
    makeTool("zola", "build", true),
    makeTool("zola", "serve", true),
    makeTool("zola", "version", true),
  ]
  registry->Js.Dict.set("zola", makeAdapter("zola", "Rust", zolaTools))

  let cobaltTools = [
    makeTool("cobalt", "init", true),
    makeTool("cobalt", "build", true),
    makeTool("cobalt", "serve", true),
    makeTool("cobalt", "version", true),
  ]
  registry->Js.Dict.set("cobalt", makeAdapter("cobalt", "Rust", cobaltTools))

  // Haskell-based SSG adapters
  let hakyllTools = [
    makeTool("hakyll", "init", true),
    makeTool("hakyll", "build", true),
    makeTool("hakyll", "watch", true),
    makeTool("hakyll", "version", true),
  ]
  registry->Js.Dict.set("hakyll", makeAdapter("hakyll", "Haskell", hakyllTools))

  // Elixir-based SSG adapter
  let serumTools = [
    makeTool("serum", "init", true),
    makeTool("serum", "build", true),
    makeTool("serum", "server", true),
    makeTool("serum", "version", true),
  ]
  registry->Js.Dict.set("serum", makeAdapter("serum", "Elixir", serumTools))

  registry
}

// ============================================================
// INTEGRATION: Single-adapter lifecycle
// ============================================================

let _ = test("Integration - single adapter connect/use/disconnect lifecycle", () => {
  let registry = buildRegistry()

  switch Js.Dict.get(registry, "zola") {
  | None => Js.Exn.raiseError("zola adapter not found in registry")
  | Some(adapter) =>
    // Initial state: disconnected
    assertEquals(adapter.isConnected(), false, "Adapter must start disconnected")

    // Connect
    let connected = adapter.connect()
    assertEquals(connected, true, "connect() must return true for mock adapter")
    assertEquals(adapter.isConnected(), true, "Adapter must be connected after connect()")

    // Use a tool
    let buildTool = adapter.tools->Array.find(t => t.name == "zola_build")
    assertSome(buildTool, "zola must have a build tool")

    let result = switch buildTool {
    | Some(t) => t.execute(Js.Dict.empty())
    | None => {success: false, stdout: "", stderr: "no tool", code: 99}
    }
    assertEquals(result.success, true, "build tool must succeed")
    assertEquals(result.code, 0, "build tool exit code must be 0")

    // Disconnect
    adapter.disconnect()
    assertEquals(adapter.isConnected(), false, "Adapter must be disconnected after disconnect()")
  }
})

// ============================================================
// INTEGRATION: Multi-adapter chain
//
// A pipeline where content is processed by three adapters in sequence.
// Each adapter must connect, process, and disconnect without affecting
// the state of the other adapters.
// ============================================================

let _ = test("Integration - multi-adapter chain preserves independent state", () => {
  let registry = buildRegistry()

  let chain = ["zola", "hakyll", "serum"]

  // Resolve all adapters upfront
  let adapters = chain->Array.filterMap(name => Js.Dict.get(registry, name))
  assertEquals(Array.length(adapters), 3, "All three adapters must resolve from registry")

  // Connect all adapters
  adapters->Array.forEach(adapter => {
    let _ = adapter.connect()
    assertEquals(
      adapter.isConnected(),
      true,
      "Each adapter must be connected after connect(): " ++ adapter.name,
    )
  })

  // Verify mutual isolation: connecting one does not affect others
  adapters->Array.forEach(adapter => {
    assertEquals(
      adapter.isConnected(),
      true,
      "Adapter " ++ adapter.name ++ " must remain connected while others are active",
    )
  })

  // Execute the 'build' tool from each adapter and collect results
  let results = adapters->Array.map(adapter => {
    let buildTool = adapter.tools->Array.find(t => Js.String2.includes(t.name, "_build"))
    switch buildTool {
    | Some(t) => t.execute(Js.Dict.empty())
    | None => {success: false, stdout: "", stderr: "missing build tool", code: 99}
    }
  })

  assertEquals(Array.length(results), 3, "Must have a result for every adapter in the chain")
  results->Array.forEach(result => {
    assertEquals(result.success, true, "Build tool must succeed for each chain member")
  })

  // Disconnect all adapters in reverse order
  adapters->Array.reverse->Array.forEach(adapter => {
    adapter.disconnect()
    assertEquals(
      adapter.isConnected(),
      false,
      "Adapter " ++ adapter.name ++ " must be disconnected after disconnect()",
    )
  })
})

// ============================================================
// INTEGRATION: Registry lookup contract
//
// Looking up an unknown adapter name must return None rather than
// throwing or returning a garbage object.
// ============================================================

let _ = test("Integration - registry returns None for unknown adapter names", () => {
  let registry = buildRegistry()

  let unknownNames = ["nonexistent", "", "ZOLA", "zola ", " cobalt", "zola\x00"]

  unknownNames->Array.forEach(name => {
    let result = Js.Dict.get(registry, name)
    assertNone(result, "Registry must return None for unknown name: '" ++ name ++ "'")
  })
})

// ============================================================
// INTEGRATION: Tool names follow {adapter}_{action} convention
// ============================================================

let _ = test("Integration - all adapter tool names follow naming convention", () => {
  let registry = buildRegistry()

  Js.Dict.entries(registry)->Array.forEach(((adapterName, adapter)) => {
    adapter.tools->Array.forEach(tool => {
      assertTrue(
        Js.String2.startsWith(tool.name, adapterName ++ "_"),
        "Tool '" ++ tool.name ++ "' must start with adapter prefix '" ++ adapterName ++ "_'",
      )
    })
  })
})

// ============================================================
// INTEGRATION: All adapters expose minimum required tools
// ============================================================

let _ = test("Integration - every adapter has init, build, and version tools", () => {
  let registry = buildRegistry()
  let requiredActions = ["init", "build", "version"]

  Js.Dict.entries(registry)->Array.forEach(((adapterName, adapter)) => {
    requiredActions->Array.forEach(action => {
      let hasTool = adapter.tools->Array.some(t => Js.String2.includes(t.name, "_" ++ action))
      assertTrue(
        hasTool,
        "Adapter '" ++ adapterName ++ "' must have a '" ++ action ++ "' tool",
      )
    })
  })
})

// ============================================================
// INTEGRATION: Adapter language field is non-empty and known
// ============================================================

let _ = test("Integration - all adapters declare a known implementation language", () => {
  let registry = buildRegistry()
  let knownLanguages = ["Rust", "Haskell", "Elixir", "Julia", "OCaml", "Gleam", "Nim", "Crystal"]

  Js.Dict.entries(registry)->Array.forEach(((adapterName, adapter)) => {
    assertTrue(
      Js.String2.length(adapter.language) > 0,
      "Adapter '" ++ adapterName ++ "' must have a non-empty language field",
    )
    assertTrue(
      knownLanguages->Array.includes(adapter.language),
      "Adapter '" ++ adapterName ++ "' language '" ++ adapter.language ++ "' must be in the known list",
    )
  })
})

// ============================================================
// INTEGRATION: Tool execution is idempotent for read-only tools
// ============================================================

let _ = test("Integration - version tool produces identical results on repeated calls", () => {
  let registry = buildRegistry()

  switch Js.Dict.get(registry, "zola") {
  | None => Js.Exn.raiseError("zola adapter not in registry")
  | Some(adapter) =>
    let _ = adapter.connect()

    let versionTool = adapter.tools->Array.find(t => t.name == "zola_version")
    assertSome(versionTool, "zola must have a version tool")

    let results = switch versionTool {
    | Some(t) => Belt.Array.makeBy(5, _ => t.execute(Js.Dict.empty()))
    | None => []
    }

    assertEquals(Array.length(results), 5, "Must have 5 results")

    // All five invocations must agree on success and exit code
    let first = results[0]
    results->Array.forEach(r => {
      switch first {
      | Some(f) =>
        assertEquals(r.success, f.success, "Version tool idempotency: success must match")
        assertEquals(r.code, f.code, "Version tool idempotency: exit code must match")
      | None => Js.Exn.raiseError("No first result")
      }
    })

    adapter.disconnect()
  }
})
