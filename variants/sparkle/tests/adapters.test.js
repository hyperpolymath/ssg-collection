// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// adapters.test.js — Unit tests for adapter module

import { assertEquals, assert, assertExists } from "@std/assert";
import { listAdapters, adapterCount, metadata, getAdapter } from "../adapters/mod.js";

// ═══════════════════════════════════════════════════════════════════════════════
// Module Entry Point Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("listAdapters - returns array of adapter names", () => {
  const adapters = listAdapters();
  assert(Array.isArray(adapters));
  assert(adapters.length > 0);
  assert(adapters.includes("zola"));
  assert(adapters.includes("hakyll"));
});

Deno.test("adapterCount - returns correct count", () => {
  const count = adapterCount();
  assertEquals(count, 28);
});

Deno.test("metadata - contains required fields", () => {
  assertExists(metadata.name);
  assertExists(metadata.version);
  assertExists(metadata.description);
  assertExists(metadata.license);
  assertExists(metadata.ecosystem);
  assertExists(metadata.adapters);
  assertExists(metadata.count);

  assertEquals(metadata.name, "sparkle-ssg");
  assertEquals(metadata.ecosystem, "hyperpolymath");
  assertEquals(metadata.count, 28);
});

// ═══════════════════════════════════════════════════════════════════════════════
// Adapter Loading Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("getAdapter - loads zola adapter", async () => {
  const zola = await getAdapter("zola");
  assertExists(zola.name);
  assertExists(zola.language);
  assertExists(zola.tools);
  assertEquals(zola.name, "Zola");
  assertEquals(zola.language, "Rust");
});

Deno.test("getAdapter - throws for unknown adapter", async () => {
  try {
    await getAdapter("nonexistent");
    assert(false, "Should have thrown");
  } catch (e) {
    assert(e.message.includes("Unknown adapter"));
  }
});

// ═══════════════════════════════════════════════════════════════════════════════
// Adapter Structure Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("adapters - all have required exports", async () => {
  const requiredExports = ["name", "language", "description", "tools", "connect", "disconnect", "isConnected"];
  const adaptersToTest = ["zola", "hakyll", "cobalt"];

  for (const adapterName of adaptersToTest) {
    const adapter = await getAdapter(adapterName);
    for (const exportName of requiredExports) {
      assertExists(adapter[exportName], `${adapterName} missing ${exportName}`);
    }
  }
});

Deno.test("adapters - tools have required schema", async () => {
  const zola = await getAdapter("zola");

  for (const tool of zola.tools) {
    assertExists(tool.name, "Tool missing name");
    assertExists(tool.description, "Tool missing description");
    assertExists(tool.inputSchema, "Tool missing inputSchema");
    assertExists(tool.execute, "Tool missing execute function");
    assertEquals(typeof tool.execute, "function");
  }
});

// ═══════════════════════════════════════════════════════════════════════════════
// Security Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("adapters - zola validates path input", async () => {
  const zola = await getAdapter("zola");
  const initTool = zola.tools.find((t) => t.name === "zola_init");

  // Test with malicious path
  const result = await initTool.execute({ path: "../../../etc/passwd" });
  assertEquals(result.success, false);
  assert(result.stderr.includes("Invalid"));
});

Deno.test("adapters - zola validates URL input", async () => {
  const zola = await getAdapter("zola");
  const buildTool = zola.tools.find((t) => t.name === "zola_build");

  // Test with malicious URL
  const result = await buildTool.execute({ baseUrl: "javascript:alert(1)" });
  assertEquals(result.success, false);
  assert(result.stderr.includes("Invalid"));
});
