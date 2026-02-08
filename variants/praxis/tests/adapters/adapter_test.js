// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Adapter interface compliance tests

import { assertEquals, assertExists } from "https://deno.land/std@0.208.0/assert/mod.ts";

// Get list of all adapters
const adaptersDir = new URL("../../adapters/", import.meta.url);

// Required exports for adapter interface
const REQUIRED_EXPORTS = [
  "name",
  "language",
  "description",
  "connect",
  "disconnect",
  "isConnected",
  "tools"
];

// Test each adapter file
for await (const entry of Deno.readDir(adaptersDir)) {
  if (entry.isFile && entry.name.endsWith(".js")) {
    const adapterName = entry.name.replace(".js", "");

    Deno.test(`Adapter ${adapterName}: has required exports`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      for (const exportName of REQUIRED_EXPORTS) {
        assertExists(
          adapter[exportName],
          `Adapter ${adapterName} missing required export: ${exportName}`
        );
      }
    });

    Deno.test(`Adapter ${adapterName}: name is a string`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      assertEquals(typeof adapter.name, "string");
      assertEquals(adapter.name.length > 0, true, "name should not be empty");
    });

    Deno.test(`Adapter ${adapterName}: language is a string`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      assertEquals(typeof adapter.language, "string");
    });

    Deno.test(`Adapter ${adapterName}: description is a string`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      assertEquals(typeof adapter.description, "string");
    });

    Deno.test(`Adapter ${adapterName}: connect is async function`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      assertEquals(typeof adapter.connect, "function");
    });

    Deno.test(`Adapter ${adapterName}: disconnect is async function`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      assertEquals(typeof adapter.disconnect, "function");
    });

    Deno.test(`Adapter ${adapterName}: isConnected is function`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      assertEquals(typeof adapter.isConnected, "function");
    });

    Deno.test(`Adapter ${adapterName}: tools is array`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      assertEquals(Array.isArray(adapter.tools), true);
    });

    Deno.test(`Adapter ${adapterName}: tools have required properties`, async () => {
      const adapterPath = new URL(`../../adapters/${entry.name}`, import.meta.url);
      const adapter = await import(adapterPath.href);

      for (const tool of adapter.tools) {
        assertExists(tool.name, `Tool missing name in ${adapterName}`);
        assertExists(tool.description, `Tool ${tool.name} missing description in ${adapterName}`);
        assertExists(tool.inputSchema, `Tool ${tool.name} missing inputSchema in ${adapterName}`);
        assertExists(tool.execute, `Tool ${tool.name} missing execute in ${adapterName}`);
        assertEquals(typeof tool.execute, "function", `Tool ${tool.name} execute should be function`);
      }
    });
  }
}
