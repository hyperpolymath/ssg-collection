// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Unit tests for adapter interface compliance

import {
  assertEquals,
  assertExists,
  assertInstanceOf,
} from "https://deno.land/std@0.208.0/assert/mod.ts";
import { walk } from "https://deno.land/std@0.208.0/fs/walk.ts";

// ─────────────────────────────────────────────────────────────────────────────
// Test helper: Load all adapters
// ─────────────────────────────────────────────────────────────────────────────

async function getAdapterFiles(): Promise<string[]> {
  const adapters: string[] = [];
  for await (const entry of walk("./adapters", {
    exts: [".js"],
    maxDepth: 1,
  })) {
    if (entry.isFile) {
      adapters.push(entry.path);
    }
  }
  return adapters.sort();
}

// ─────────────────────────────────────────────────────────────────────────────
// Test: Adapter count
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("should have exactly 28 adapters", async () => {
  const adapters = await getAdapterFiles();
  assertEquals(adapters.length, 28, `Expected 28 adapters, found ${adapters.length}`);
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Each adapter exports required properties
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("each adapter should export 'name' as string", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);
    assertExists(adapter.name, `${adapterPath}: missing 'name' export`);
    assertEquals(
      typeof adapter.name,
      "string",
      `${adapterPath}: 'name' should be string`
    );
    assertEquals(
      adapter.name.length > 0,
      true,
      `${adapterPath}: 'name' should not be empty`
    );
  }
});

Deno.test("each adapter should export 'language' as string", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);
    assertExists(adapter.language, `${adapterPath}: missing 'language' export`);
    assertEquals(
      typeof adapter.language,
      "string",
      `${adapterPath}: 'language' should be string`
    );
  }
});

Deno.test("each adapter should export 'tools' as array", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);
    assertExists(adapter.tools, `${adapterPath}: missing 'tools' export`);
    assertInstanceOf(
      adapter.tools,
      Array,
      `${adapterPath}: 'tools' should be array`
    );
    assertEquals(
      adapter.tools.length > 0,
      true,
      `${adapterPath}: 'tools' should not be empty`
    );
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Each adapter exports required functions
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("each adapter should export 'connect' function", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);
    assertExists(adapter.connect, `${adapterPath}: missing 'connect' export`);
    assertEquals(
      typeof adapter.connect,
      "function",
      `${adapterPath}: 'connect' should be function`
    );
  }
});

Deno.test("each adapter should export 'disconnect' function", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);
    assertExists(adapter.disconnect, `${adapterPath}: missing 'disconnect' export`);
    assertEquals(
      typeof adapter.disconnect,
      "function",
      `${adapterPath}: 'disconnect' should be function`
    );
  }
});

Deno.test("each adapter should export 'isConnected' function", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);
    assertExists(adapter.isConnected, `${adapterPath}: missing 'isConnected' export`);
    assertEquals(
      typeof adapter.isConnected,
      "function",
      `${adapterPath}: 'isConnected' should be function`
    );
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Tool interface compliance
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("each tool should have required properties", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);

    for (const tool of adapter.tools) {
      // Tool name
      assertExists(tool.name, `${adapterPath}: tool missing 'name'`);
      assertEquals(
        typeof tool.name,
        "string",
        `${adapterPath}: tool.name should be string`
      );

      // Tool description
      assertExists(tool.description, `${adapterPath}: tool '${tool.name}' missing 'description'`);
      assertEquals(
        typeof tool.description,
        "string",
        `${adapterPath}: tool.description should be string`
      );

      // Tool inputSchema
      assertExists(
        tool.inputSchema,
        `${adapterPath}: tool '${tool.name}' missing 'inputSchema'`
      );
      assertEquals(
        typeof tool.inputSchema,
        "object",
        `${adapterPath}: tool.inputSchema should be object`
      );

      // Tool execute function
      assertExists(tool.execute, `${adapterPath}: tool '${tool.name}' missing 'execute'`);
      assertEquals(
        typeof tool.execute,
        "function",
        `${adapterPath}: tool.execute should be function`
      );
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Tool naming convention
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("tool names should follow snake_case convention", async () => {
  const adapters = await getAdapterFiles();
  const snakeCaseRegex = /^[a-z][a-z0-9]*(_[a-z0-9]+)*$/;

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);

    for (const tool of adapter.tools) {
      assertEquals(
        snakeCaseRegex.test(tool.name),
        true,
        `${adapterPath}: tool name '${tool.name}' should be snake_case`
      );
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Connection lifecycle
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("isConnected should return false initially", async () => {
  const adapters = await getAdapterFiles();

  for (const adapterPath of adapters) {
    // Import fresh module
    const adapter = await import(`../${adapterPath}?t=${Date.now()}`);

    // Should be disconnected initially
    const connected = adapter.isConnected();
    assertEquals(
      typeof connected,
      "boolean",
      `${adapterPath}: isConnected() should return boolean`
    );
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Language coverage
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("should cover all 17 expected languages", async () => {
  const expectedLanguages = new Set([
    "Rust",
    "Haskell",
    "Elixir",
    "Clojure",
    "Julia",
    "Scala",
    "Common Lisp",
    "F#",
    "Racket",
    "Kotlin",
    "Crystal",
    "Nim",
    "Swift",
    "D",
    "Tcl",
    "OCaml",
    "Erlang",
  ]);

  const adapters = await getAdapterFiles();
  const foundLanguages = new Set<string>();

  for (const adapterPath of adapters) {
    const adapter = await import(`../${adapterPath}`);
    foundLanguages.add(adapter.language);
  }

  for (const lang of expectedLanguages) {
    assertEquals(
      foundLanguages.has(lang),
      true,
      `Missing adapter for language: ${lang}`
    );
  }
});
