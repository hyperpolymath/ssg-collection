// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

import { assertEquals, assertExists } from "https://deno.land/std@0.210.0/assert/mod.ts";

// Test adapter structure
Deno.test("adapters have required exports", async () => {
  const adapterFiles = [];
  for await (const entry of Deno.readDir("./adapters")) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      adapterFiles.push(entry.name);
    }
  }

  for (const file of adapterFiles) {
    const adapter = await import(`../adapters/${file}`);

    // Check required exports
    assertExists(adapter.name, `${file} should export 'name'`);
    assertExists(adapter.language, `${file} should export 'language'`);
    assertExists(adapter.tools, `${file} should export 'tools'`);
    assertExists(adapter.connect, `${file} should export 'connect'`);
    assertExists(adapter.disconnect, `${file} should export 'disconnect'`);
    assertExists(adapter.isConnected, `${file} should export 'isConnected'`);

    // Check types
    assertEquals(typeof adapter.name, "string", `${file} name should be string`);
    assertEquals(typeof adapter.language, "string", `${file} language should be string`);
    assertEquals(Array.isArray(adapter.tools), true, `${file} tools should be array`);
    assertEquals(typeof adapter.connect, "function", `${file} connect should be function`);
  }
});

// Test tool structure
Deno.test("adapter tools have correct structure", async () => {
  const adapter = await import("../adapters/zola.js");

  for (const tool of adapter.tools) {
    assertExists(tool.name, "Tool should have name");
    assertExists(tool.description, "Tool should have description");
    assertExists(tool.inputSchema, "Tool should have inputSchema");
    assertExists(tool.execute, "Tool should have execute function");

    assertEquals(typeof tool.name, "string");
    assertEquals(typeof tool.description, "string");
    assertEquals(typeof tool.execute, "function");
    assertEquals(typeof tool.inputSchema, "object");
  }
});

// Test adapter count
Deno.test("adapter count is at least 28", async () => {
  let count = 0;
  for await (const entry of Deno.readDir("./adapters")) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      count++;
    }
  }

  assertEquals(count >= 28, true, `Expected at least 28 adapters, found ${count}`);
});
