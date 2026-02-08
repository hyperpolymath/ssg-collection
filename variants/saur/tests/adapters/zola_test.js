// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Unit tests for Zola adapter

import {
  assertEquals,
  assertExists,
} from "https://deno.land/std@0.208.0/assert/mod.ts";

import * as zola from "../../adapters/zola.js";

// ─────────────────────────────────────────────────────────────────────────────
// Metadata tests
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Zola adapter: should have correct name", () => {
  assertEquals(zola.name, "Zola");
});

Deno.test("Zola adapter: should have correct language", () => {
  assertEquals(zola.language, "Rust");
});

Deno.test("Zola adapter: should export tools array", () => {
  assertExists(zola.tools);
  assertEquals(Array.isArray(zola.tools), true);
});

// ─────────────────────────────────────────────────────────────────────────────
// Connection tests
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Zola adapter: should export connect function", () => {
  assertEquals(typeof zola.connect, "function");
});

Deno.test("Zola adapter: should export disconnect function", () => {
  assertEquals(typeof zola.disconnect, "function");
});

Deno.test("Zola adapter: should export isConnected function", () => {
  assertEquals(typeof zola.isConnected, "function");
});

// ─────────────────────────────────────────────────────────────────────────────
// Tools tests
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Zola adapter: should have expected tools", () => {
  const toolNames = zola.tools.map((t) => t.name);

  // Check for common tools
  const expectedTools = ["zola_init", "zola_build", "zola_serve"];

  for (const expected of expectedTools) {
    assertEquals(
      toolNames.includes(expected),
      true,
      `Missing expected tool: ${expected}`
    );
  }
});

Deno.test("Zola adapter: zola_init tool should have correct schema", () => {
  const initTool = zola.tools.find((t) => t.name === "zola_init");
  assertExists(initTool, "zola_init tool not found");
  assertExists(initTool.inputSchema, "zola_init missing inputSchema");
  assertEquals(initTool.inputSchema.type, "object");
});

Deno.test("Zola adapter: zola_build tool should have correct schema", () => {
  const buildTool = zola.tools.find((t) => t.name === "zola_build");
  assertExists(buildTool, "zola_build tool not found");
  assertExists(buildTool.inputSchema, "zola_build missing inputSchema");
  assertEquals(buildTool.inputSchema.type, "object");
});

Deno.test("Zola adapter: all tools should have execute function", () => {
  for (const tool of zola.tools) {
    assertEquals(
      typeof tool.execute,
      "function",
      `Tool ${tool.name} missing execute function`
    );
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Tool name convention tests
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Zola adapter: tool names should start with 'zola_'", () => {
  for (const tool of zola.tools) {
    assertEquals(
      tool.name.startsWith("zola_"),
      true,
      `Tool name ${tool.name} should start with 'zola_'`
    );
  }
});
