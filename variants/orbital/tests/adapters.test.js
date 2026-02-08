// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

import { assertEquals, assertExists } from "@std/assert";

// List of all adapters to test
const adapterNames = [
  "babashka",
  "cobalt",
  "coleslaw",
  "cryogen",
  "documenter",
  "ema",
  "fornax",
  "franklin",
  "frog",
  "hakyll",
  "laika",
  "marmot",
  "mdbook",
  "nimble-publisher",
  "nimrod",
  "orchid",
  "perun",
  "pollen",
  "publish",
  "reggae",
  "scalatex",
  "serum",
  "staticwebpages",
  "tableau",
  "wub",
  "yocaml",
  "zola",
  "zotonic",
];

Deno.test("all adapters export required symbols", async () => {
  for (const name of adapterNames) {
    const adapter = await import(`../adapters/${name}.js`);

    assertExists(adapter.name, `${name} missing 'name' export`);
    assertExists(adapter.language, `${name} missing 'language' export`);
    assertExists(adapter.description, `${name} missing 'description' export`);
    assertExists(adapter.connect, `${name} missing 'connect' export`);
    assertExists(adapter.disconnect, `${name} missing 'disconnect' export`);
    assertExists(adapter.isConnected, `${name} missing 'isConnected' export`);
    assertExists(adapter.tools, `${name} missing 'tools' export`);
  }
});

Deno.test("all adapters have valid tool definitions", async () => {
  for (const name of adapterNames) {
    const adapter = await import(`../adapters/${name}.js`);

    assertEquals(Array.isArray(adapter.tools), true, `${name} tools is not an array`);

    for (const tool of adapter.tools) {
      assertExists(tool.name, `${name} tool missing 'name'`);
      assertExists(tool.description, `${name} tool missing 'description'`);
      assertExists(tool.inputSchema, `${name} tool missing 'inputSchema'`);
      assertExists(tool.execute, `${name} tool missing 'execute'`);

      assertEquals(
        tool.inputSchema.type,
        "object",
        `${name} tool ${tool.name} inputSchema.type is not 'object'`
      );

      assertEquals(
        typeof tool.execute,
        "function",
        `${name} tool ${tool.name} execute is not a function`
      );
    }
  }
});

Deno.test("all adapters have SPDX headers", async () => {
  for (const name of adapterNames) {
    const code = await Deno.readTextFile(`adapters/${name}.js`);
    const lines = code.split("\n").slice(0, 3);

    let hasSPDX = false;
    for (const line of lines) {
      if (line.includes("SPDX-License-Identifier")) {
        hasSPDX = true;
        break;
      }
    }

    assertEquals(hasSPDX, true, `${name} missing SPDX header`);
  }
});

Deno.test("adapter count is 28", () => {
  assertEquals(adapterNames.length, 28, "Expected 28 adapters");
});

Deno.test("all adapters isConnected returns boolean", async () => {
  for (const name of adapterNames) {
    const adapter = await import(`../adapters/${name}.js`);
    const result = adapter.isConnected();
    assertEquals(typeof result, "boolean", `${name} isConnected did not return boolean`);
  }
});
