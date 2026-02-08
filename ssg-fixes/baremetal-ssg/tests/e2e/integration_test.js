// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

import { assertEquals, assertExists } from "https://deno.land/std@0.210.0/assert/mod.ts";
import { loadAdapters, getAdapter, health } from "../../main.js";

// Test main module loading
Deno.test("main module loads adapters", async () => {
  await loadAdapters();
  const healthCheck = health();

  assertExists(healthCheck.status);
  assertEquals(healthCheck.status, "healthy");
  assertEquals(healthCheck.adapters >= 28, true);
});

// Test adapter retrieval
Deno.test("can retrieve adapter by name", async () => {
  await loadAdapters();

  const zola = getAdapter("zola");
  assertExists(zola);
  assertEquals(zola.name, "Zola");
  assertEquals(zola.language, "Rust");
});

// Test adapter connectivity (mock)
Deno.test("adapter connect returns boolean", async () => {
  const adapter = await import("../../adapters/zola.js");

  // Connect will fail without the binary, but should return false gracefully
  const connected = await adapter.connect();
  assertEquals(typeof connected, "boolean");
});
