// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

import { assertEquals, assertExists } from "@std/assert";
import * as zola from "../adapters/zola.js";

Deno.test("zola adapter exports required symbols", () => {
  assertExists(zola.name);
  assertExists(zola.language);
  assertExists(zola.description);
  assertExists(zola.connect);
  assertExists(zola.disconnect);
  assertExists(zola.isConnected);
  assertExists(zola.tools);
});

Deno.test("zola adapter has correct metadata", () => {
  assertEquals(zola.name, "Zola");
  assertEquals(zola.language, "Rust");
  assertEquals(typeof zola.description, "string");
});

Deno.test("zola tools array has expected tools", () => {
  const toolNames = zola.tools.map((t) => t.name);
  assertEquals(toolNames.includes("zola_init"), true);
  assertEquals(toolNames.includes("zola_build"), true);
  assertEquals(toolNames.includes("zola_serve"), true);
  assertEquals(toolNames.includes("zola_check"), true);
  assertEquals(toolNames.includes("zola_version"), true);
});

Deno.test("zola tools have valid schema", () => {
  for (const tool of zola.tools) {
    assertExists(tool.name);
    assertExists(tool.description);
    assertExists(tool.inputSchema);
    assertExists(tool.execute);
    assertEquals(tool.inputSchema.type, "object");
  }
});

Deno.test("zola isConnected returns boolean", () => {
  const result = zola.isConnected();
  assertEquals(typeof result, "boolean");
});
