// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Unit tests for adapter interface validation

import { assertEquals, assertExists, assertInstanceOf } from "@std/assert";
import { describe, it } from "@std/testing/bdd";

// Get list of all adapter files
const adapterFiles = [];
for await (const entry of Deno.readDir("./adapters")) {
  if (entry.isFile && entry.name.endsWith(".js") && entry.name !== "README.md") {
    adapterFiles.push(entry.name);
  }
}

describe("Adapter Interface", () => {
  it("should have exactly 28 adapters", () => {
    assertEquals(adapterFiles.length, 28, `Expected 28 adapters, found ${adapterFiles.length}`);
  });
});

describe("Adapter Exports", () => {
  for (const file of adapterFiles) {
    const adapterName = file.replace(".js", "");

    describe(adapterName, () => {
      let adapter;

      it("should import successfully", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter);
      });

      it("should export 'name' as string", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter.name, "Missing 'name' export");
        assertEquals(typeof adapter.name, "string");
      });

      it("should export 'language' as string", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter.language, "Missing 'language' export");
        assertEquals(typeof adapter.language, "string");
      });

      it("should export 'description' as string", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter.description, "Missing 'description' export");
        assertEquals(typeof adapter.description, "string");
      });

      it("should export 'connect' as function", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter.connect, "Missing 'connect' export");
        assertEquals(typeof adapter.connect, "function");
      });

      it("should export 'disconnect' as function", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter.disconnect, "Missing 'disconnect' export");
        assertEquals(typeof adapter.disconnect, "function");
      });

      it("should export 'isConnected' as function", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter.isConnected, "Missing 'isConnected' export");
        assertEquals(typeof adapter.isConnected, "function");
      });

      it("should export 'tools' as array", async () => {
        adapter = await import(`../adapters/${file}`);
        assertExists(adapter.tools, "Missing 'tools' export");
        assertInstanceOf(adapter.tools, Array);
      });

      it("should have at least one tool", async () => {
        adapter = await import(`../adapters/${file}`);
        assertEquals(
          adapter.tools.length > 0,
          true,
          `Adapter ${adapterName} has no tools`,
        );
      });
    });
  }
});

describe("Tool Schema", () => {
  for (const file of adapterFiles) {
    const adapterName = file.replace(".js", "");

    describe(`${adapterName} tools`, () => {
      it("should have valid tool structure", async () => {
        const adapter = await import(`../adapters/${file}`);

        for (const tool of adapter.tools) {
          // Check required properties
          assertExists(tool.name, `Tool missing 'name' in ${adapterName}`);
          assertEquals(typeof tool.name, "string");

          assertExists(tool.description, `Tool ${tool.name} missing 'description'`);
          assertEquals(typeof tool.description, "string");

          assertExists(tool.inputSchema, `Tool ${tool.name} missing 'inputSchema'`);
          assertEquals(typeof tool.inputSchema, "object");

          assertExists(tool.execute, `Tool ${tool.name} missing 'execute'`);
          assertEquals(typeof tool.execute, "function");
        }
      });

      it("should have unique tool names", async () => {
        const adapter = await import(`../adapters/${file}`);
        const names = adapter.tools.map((t) => t.name);
        const uniqueNames = new Set(names);
        assertEquals(
          names.length,
          uniqueNames.size,
          `Duplicate tool names in ${adapterName}`,
        );
      });

      it("should follow naming convention", async () => {
        const adapter = await import(`../adapters/${file}`);
        const prefix = adapterName.replace(/-/g, "_");

        for (const tool of adapter.tools) {
          assertEquals(
            tool.name.startsWith(prefix) || tool.name.startsWith(adapterName.replace(/-/g, "")),
            true,
            `Tool ${tool.name} should start with ${prefix}`,
          );
        }
      });
    });
  }
});
