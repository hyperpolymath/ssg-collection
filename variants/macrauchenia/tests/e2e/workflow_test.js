// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// E2E tests for adapter workflows

import { assertEquals, assertExists } from "@std/assert";
import { describe, it, beforeAll, afterAll } from "@std/testing/bdd";

// Test helper to dynamically import adapters
async function loadAdapter(name) {
  return await import(`../../adapters/${name}.js`);
}

describe("Adapter Lifecycle", () => {
  const adapterNames = ["zola", "hakyll", "mdbook"];

  for (const name of adapterNames) {
    describe(name, () => {
      let adapter;

      beforeAll(async () => {
        adapter = await loadAdapter(name);
      });

      it("should start disconnected", () => {
        assertEquals(adapter.isConnected(), false);
      });

      it("should handle connect gracefully when binary missing", async () => {
        // This may return false if the binary isn't installed
        const connected = await adapter.connect();
        assertEquals(typeof connected, "boolean");
      });

      it("should disconnect cleanly", async () => {
        await adapter.disconnect();
        assertEquals(adapter.isConnected(), false);
      });
    });
  }
});

describe("Tool Execution", () => {
  describe("Version Tools", () => {
    const adapters = ["zola", "hakyll", "mdbook", "cobalt"];

    for (const name of adapters) {
      it(`${name} version tool should return structured result`, async () => {
        const adapter = await loadAdapter(name);
        const versionTool = adapter.tools.find(t => t.name.includes("version"));

        if (versionTool) {
          const result = await versionTool.execute({});

          assertExists(result.success);
          assertExists(result.stdout);
          assertExists(result.stderr);
          assertEquals(typeof result.success, "boolean");
          assertEquals(typeof result.stdout, "string");
          assertEquals(typeof result.stderr, "string");
        }
      });
    }
  });
});

describe("Tool Input Schema", () => {
  it("all tools should have valid JSON Schema", async () => {
    const files = [...Deno.readDirSync("adapters")]
      .filter(f => f.name.endsWith(".js"));

    for (const file of files) {
      const adapter = await loadAdapter(file.name.replace(".js", ""));

      for (const tool of adapter.tools) {
        assertExists(tool.inputSchema, `${file.name}:${tool.name} should have inputSchema`);
        assertExists(tool.inputSchema.type, `${file.name}:${tool.name} inputSchema should have type`);
        assertEquals(tool.inputSchema.type, "object",
          `${file.name}:${tool.name} inputSchema.type should be "object"`);
      }
    }
  });
});

describe("Adapter Count", () => {
  it("should have exactly 28 adapters", async () => {
    const files = [...Deno.readDirSync("adapters")]
      .filter(f => f.name.endsWith(".js") && f.name !== "README.md");

    assertEquals(files.length, 28, "Should have 28 adapter files");
  });
});
