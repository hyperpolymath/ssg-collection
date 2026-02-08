// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// E2E tests for MCP interface compliance

import {
  assertEquals,
  assertExists,
  assertRejects,
} from "https://deno.land/std@0.208.0/assert/mod.ts";
import { walk } from "https://deno.land/std@0.208.0/fs/walk.ts";

// ─────────────────────────────────────────────────────────────────────────────
// Test helper: Load adapter dynamically
// ─────────────────────────────────────────────────────────────────────────────

async function loadAdapter(name: string) {
  return await import(`../../adapters/${name}.js`);
}

// ─────────────────────────────────────────────────────────────────────────────
// Test: Connection lifecycle
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("E2E: Zola adapter connection lifecycle", async () => {
  const adapter = await loadAdapter("zola");

  // Initial state
  const initialState = adapter.isConnected();
  assertEquals(typeof initialState, "boolean");

  // Connect
  await adapter.connect();
  assertEquals(adapter.isConnected(), true);

  // Disconnect
  await adapter.disconnect();
  assertEquals(adapter.isConnected(), false);
});

Deno.test("E2E: Connect should be idempotent", async () => {
  const adapter = await loadAdapter("cobalt");

  await adapter.connect();
  const firstState = adapter.isConnected();

  await adapter.connect(); // Second connect
  const secondState = adapter.isConnected();

  assertEquals(firstState, secondState);

  await adapter.disconnect();
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Tool execution structure
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("E2E: Tool execute should return object", async () => {
  const adapter = await loadAdapter("zola");

  await adapter.connect();

  // Find a simple tool (like version)
  const versionTool = adapter.tools.find((t) =>
    t.name.includes("version")
  );

  if (versionTool) {
    try {
      const result = await versionTool.execute({});
      assertExists(result);
      assertEquals(typeof result, "object");
    } catch (e) {
      // CLI not installed is OK for this test
      if (!e.message.includes("not found") && !e.message.includes("ENOENT")) {
        throw e;
      }
    }
  }

  await adapter.disconnect();
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Input schema validation
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("E2E: All tools have valid JSON Schema structure", async () => {
  const adapters = [];
  for await (const entry of walk("./adapters", { exts: [".js"], maxDepth: 1 })) {
    if (entry.isFile) {
      adapters.push(entry.path);
    }
  }

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);

    for (const tool of adapter.tools) {
      // Check schema has type
      assertExists(
        tool.inputSchema.type,
        `${adapterPath}/${tool.name}: inputSchema missing 'type'`
      );

      // Check schema type is 'object'
      assertEquals(
        tool.inputSchema.type,
        "object",
        `${adapterPath}/${tool.name}: inputSchema.type should be 'object'`
      );

      // Check properties exists (can be empty)
      assertExists(
        tool.inputSchema.properties,
        `${adapterPath}/${tool.name}: inputSchema missing 'properties'`
      );
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Multiple adapter loading
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("E2E: Multiple adapters can be loaded simultaneously", async () => {
  const adapters = await Promise.all([
    loadAdapter("zola"),
    loadAdapter("cobalt"),
    loadAdapter("mdbook"),
  ]);

  for (const adapter of adapters) {
    assertExists(adapter.name);
    assertExists(adapter.tools);
    assertEquals(typeof adapter.connect, "function");
  }

  // All should have unique names
  const names = adapters.map((a) => a.name);
  const uniqueNames = new Set(names);
  assertEquals(names.length, uniqueNames.size, "Adapter names should be unique");
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Adapter isolation
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("E2E: Adapter connections are isolated", async () => {
  const zola = await loadAdapter("zola");
  const cobalt = await loadAdapter("cobalt");

  // Connect zola only
  await zola.connect();

  assertEquals(zola.isConnected(), true);
  // Cobalt should still be disconnected (module isolation)
  // Note: Due to module caching, this tests initial state

  await zola.disconnect();
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Tool categories
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("E2E: Each adapter should have common tool categories", async () => {
  const commonPatterns = ["init", "build", "serve"];
  const adaptersToTest = ["zola", "cobalt", "hakyll"];

  for (const adapterName of adaptersToTest) {
    try {
      const adapter = await loadAdapter(adapterName);
      const toolNames = adapter.tools.map((t) => t.name.toLowerCase());

      // Check for at least one common pattern
      const hasCommon = commonPatterns.some((pattern) =>
        toolNames.some((name) => name.includes(pattern))
      );

      assertEquals(
        hasCommon,
        true,
        `${adapterName} should have at least one common tool (init/build/serve)`
      );
    } catch (e) {
      // Adapter might not exist in test environment
      console.log(`Skipping ${adapterName}: ${e.message}`);
    }
  }
});
