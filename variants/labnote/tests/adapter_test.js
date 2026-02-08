// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Basic adapter tests for labnote-ssg

import { assertEquals, assertExists } from "https://deno.land/std@0.220.0/assert/mod.ts";

// List of all adapters to test
const ADAPTERS = [
  "babashka", "cobalt", "coleslaw", "cryogen", "documenter", "ema",
  "fornax", "franklin", "frog", "hakyll", "laika", "marmot", "mdbook",
  "nimble-publisher", "nimrod", "orchid", "perun", "pollen", "publish",
  "reggae", "scalatex", "serum", "staticwebpages", "tableau", "wub",
  "yocaml", "zola", "zotonic"
];

Deno.test("All adapters have required exports", async () => {
  for (const name of ADAPTERS) {
    const adapter = await import(`../adapters/${name}.js`);

    // Check required exports exist
    assertExists(adapter.name, `${name}: missing 'name' export`);
    assertExists(adapter.language, `${name}: missing 'language' export`);
    assertExists(adapter.description, `${name}: missing 'description' export`);
    assertExists(adapter.connect, `${name}: missing 'connect' function`);
    assertExists(adapter.disconnect, `${name}: missing 'disconnect' function`);
    assertExists(adapter.isConnected, `${name}: missing 'isConnected' function`);
    assertExists(adapter.tools, `${name}: missing 'tools' array`);
  }
});

Deno.test("All adapters have valid tool definitions", async () => {
  for (const name of ADAPTERS) {
    const adapter = await import(`../adapters/${name}.js`);

    // Check tools array
    assertEquals(Array.isArray(adapter.tools), true, `${name}: tools should be an array`);

    for (const tool of adapter.tools) {
      assertExists(tool.name, `${name}: tool missing 'name'`);
      assertExists(tool.description, `${name}: tool missing 'description'`);
      assertExists(tool.inputSchema, `${name}: tool missing 'inputSchema'`);
      assertExists(tool.execute, `${name}: tool missing 'execute' function`);
      assertEquals(typeof tool.execute, "function", `${name}: tool.execute should be a function`);
    }
  }
});

Deno.test("Adapter count is 28", async () => {
  assertEquals(ADAPTERS.length, 28, "Expected 28 adapters");
});

Deno.test("All adapters have SPDX headers", async () => {
  for (const name of ADAPTERS) {
    const content = await Deno.readTextFile(`adapters/${name}.js`);
    assertEquals(
      content.includes("SPDX-License-Identifier"),
      true,
      `${name}: missing SPDX license header`
    );
  }
});

Deno.test("No adapters use eval or shell execution", async () => {
  for (const name of ADAPTERS) {
    const content = await Deno.readTextFile(`adapters/${name}.js`);
    assertEquals(
      content.includes("eval("),
      false,
      `${name}: contains eval() - security risk`
    );
    assertEquals(
      content.includes("new Function"),
      false,
      `${name}: contains Function constructor - security risk`
    );
  }
});

Deno.test("All adapters use Deno.Command for execution", async () => {
  for (const name of ADAPTERS) {
    const content = await Deno.readTextFile(`adapters/${name}.js`);
    assertEquals(
      content.includes("Deno.Command"),
      true,
      `${name}: should use Deno.Command for safe execution`
    );
  }
});
