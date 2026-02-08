// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Unit tests for Cobalt adapter

import {
  assertEquals,
  assertExists,
} from "https://deno.land/std@0.208.0/assert/mod.ts";

import * as cobalt from "../../adapters/cobalt.js";

// ─────────────────────────────────────────────────────────────────────────────
// Metadata tests
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Cobalt adapter: should have correct name", () => {
  assertEquals(cobalt.name, "Cobalt");
});

Deno.test("Cobalt adapter: should have correct language", () => {
  assertEquals(cobalt.language, "Rust");
});

Deno.test("Cobalt adapter: should export tools array", () => {
  assertExists(cobalt.tools);
  assertEquals(Array.isArray(cobalt.tools), true);
});

// ─────────────────────────────────────────────────────────────────────────────
// Connection tests
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Cobalt adapter: should export connect function", () => {
  assertEquals(typeof cobalt.connect, "function");
});

Deno.test("Cobalt adapter: should export disconnect function", () => {
  assertEquals(typeof cobalt.disconnect, "function");
});

Deno.test("Cobalt adapter: should export isConnected function", () => {
  assertEquals(typeof cobalt.isConnected, "function");
});

// ─────────────────────────────────────────────────────────────────────────────
// Tools tests
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Cobalt adapter: should have at least one tool", () => {
  assertEquals(cobalt.tools.length > 0, true);
});

Deno.test("Cobalt adapter: all tools should have required properties", () => {
  for (const tool of cobalt.tools) {
    assertExists(tool.name, `Tool missing name`);
    assertExists(tool.description, `Tool ${tool.name} missing description`);
    assertExists(tool.inputSchema, `Tool ${tool.name} missing inputSchema`);
    assertExists(tool.execute, `Tool ${tool.name} missing execute`);
  }
});

Deno.test("Cobalt adapter: tool names should start with 'cobalt_'", () => {
  for (const tool of cobalt.tools) {
    assertEquals(
      tool.name.startsWith("cobalt_"),
      true,
      `Tool name ${tool.name} should start with 'cobalt_'`
    );
  }
});
