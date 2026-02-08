// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Unit tests for adapter validation and structure

import { assertEquals, assertExists, assertThrows } from "@std/assert";
import { describe, it } from "@std/testing/bdd";

// Test helper to dynamically import adapters
async function loadAdapter(name) {
  return await import(`../../adapters/${name}.js`);
}

describe("Adapter Structure", () => {
  const adapterFiles = [
    "zola", "hakyll", "cobalt", "mdbook", "franklin",
    "documenter", "staticwebpages", "coleslaw", "wub", "scalatex"
  ];

  for (const name of adapterFiles) {
    describe(name, () => {
      it("should export required properties", async () => {
        const adapter = await loadAdapter(name);

        assertExists(adapter.name, `${name} should export 'name'`);
        assertExists(adapter.language, `${name} should export 'language'`);
        assertExists(adapter.description, `${name} should export 'description'`);
        assertExists(adapter.tools, `${name} should export 'tools'`);
      });

      it("should have valid tool definitions", async () => {
        const adapter = await loadAdapter(name);

        for (const tool of adapter.tools) {
          assertExists(tool.name, `Tool should have 'name'`);
          assertExists(tool.description, `Tool ${tool.name} should have 'description'`);
          assertExists(tool.inputSchema, `Tool ${tool.name} should have 'inputSchema'`);
          assertExists(tool.execute, `Tool ${tool.name} should have 'execute'`);
          assertEquals(typeof tool.execute, "function", `${tool.name}.execute should be a function`);
        }
      });

      it("should export lifecycle functions", async () => {
        const adapter = await loadAdapter(name);

        assertExists(adapter.connect, `${name} should export 'connect'`);
        assertExists(adapter.disconnect, `${name} should export 'disconnect'`);
        assertExists(adapter.isConnected, `${name} should export 'isConnected'`);

        assertEquals(typeof adapter.connect, "function");
        assertEquals(typeof adapter.disconnect, "function");
        assertEquals(typeof adapter.isConnected, "function");
      });
    });
  }
});

describe("Adapter Tool Count", () => {
  it("should have at least 1 tool per adapter", async () => {
    const files = [...Deno.readDirSync("adapters")]
      .filter(f => f.name.endsWith(".js"))
      .map(f => f.name.replace(".js", ""));

    for (const name of files) {
      const adapter = await loadAdapter(name);
      assertEquals(
        adapter.tools.length >= 1,
        true,
        `${name} should have at least 1 tool`
      );
    }
  });
});
