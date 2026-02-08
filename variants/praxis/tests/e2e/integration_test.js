// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// End-to-end integration tests

import { assertEquals, assertExists } from "https://deno.land/std@0.208.0/assert/mod.ts";

const adaptersDir = new URL("../../adapters/", import.meta.url);

Deno.test("E2E: Can load all adapters", async () => {
  const adapters = [];

  for await (const entry of Deno.readDir(adaptersDir)) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);
      adapters.push({
        name: adapter.name,
        language: adapter.language,
        toolCount: adapter.tools.length
      });
    }
  }

  assertEquals(adapters.length, 28, "Should have 28 adapters");
});

Deno.test("E2E: All adapters have unique names", async () => {
  const names = new Set();

  for await (const entry of Deno.readDir(adaptersDir)) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);
      names.add(adapter.name);
    }
  }

  assertEquals(names.size, 28, "All adapter names should be unique");
});

Deno.test("E2E: Language distribution", async () => {
  const languages = new Map();

  for await (const entry of Deno.readDir(adaptersDir)) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);
      const count = languages.get(adapter.language) || 0;
      languages.set(adapter.language, count + 1);
    }
  }

  // Should have at least 10 different languages
  assertEquals(languages.size >= 10, true, `Expected at least 10 languages, got ${languages.size}`);

  console.log("\nLanguage distribution:");
  for (const [lang, count] of languages.entries()) {
    console.log(`  ${lang}: ${count}`);
  }
});

Deno.test("E2E: Tool count per adapter", async () => {
  let totalTools = 0;

  for await (const entry of Deno.readDir(adaptersDir)) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);
      totalTools += adapter.tools.length;

      // Each adapter should have at least 1 tool
      assertEquals(
        adapter.tools.length >= 1,
        true,
        `Adapter ${adapter.name} should have at least 1 tool`
      );
    }
  }

  console.log(`\nTotal tools across all adapters: ${totalTools}`);
});

Deno.test("E2E: Adapters start disconnected", async () => {
  for await (const entry of Deno.readDir(adaptersDir)) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      // Fresh import should be disconnected
      assertEquals(
        adapter.isConnected(),
        false,
        `Adapter ${adapter.name} should start disconnected`
      );
    }
  }
});
