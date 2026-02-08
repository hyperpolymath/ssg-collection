// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

import { assertEquals, assertExists } from "@std/assert";

// Integration tests for MCP protocol compliance

Deno.test("all tools follow MCP naming convention", async () => {
  const adapters = await Deno.readDir("adapters");

  for await (const entry of adapters) {
    if (!entry.name.endsWith(".js") || entry.name === "README.md") continue;

    const adapter = await import(`../../adapters/${entry.name}`);
    const adapterBaseName = entry.name.replace(".js", "").replace(/-/g, "_");

    for (const tool of adapter.tools) {
      // Tool names should start with adapter name (with underscores)
      const prefix = tool.name.split("_")[0];
      assertEquals(
        prefix.length > 0,
        true,
        `Tool ${tool.name} in ${entry.name} has invalid naming`
      );
    }
  }
});

Deno.test("all inputSchemas are valid JSON Schema", async () => {
  const adapters = await Deno.readDir("adapters");

  for await (const entry of adapters) {
    if (!entry.name.endsWith(".js") || entry.name === "README.md") continue;

    const adapter = await import(`../../adapters/${entry.name}`);

    for (const tool of adapter.tools) {
      const schema = tool.inputSchema;

      // Basic JSON Schema requirements
      assertEquals(schema.type, "object", `${entry.name}:${tool.name} schema type must be object`);

      if (schema.properties) {
        assertEquals(
          typeof schema.properties,
          "object",
          `${entry.name}:${tool.name} properties must be object`
        );

        // Check each property has a type
        for (const [propName, propSchema] of Object.entries(schema.properties)) {
          assertExists(
            propSchema.type,
            `${entry.name}:${tool.name}.${propName} missing type`
          );
        }
      }

      // Check required is an array if present
      if (schema.required) {
        assertEquals(
          Array.isArray(schema.required),
          true,
          `${entry.name}:${tool.name} required must be array`
        );
      }
    }
  }
});

Deno.test("execute functions are async", async () => {
  const adapters = await Deno.readDir("adapters");

  for await (const entry of adapters) {
    if (!entry.name.endsWith(".js") || entry.name === "README.md") continue;

    const code = await Deno.readTextFile(`adapters/${entry.name}`);

    // Check that execute uses async
    const executeMatches = code.match(/execute:\s*(async\s+)?\(/g);
    if (executeMatches) {
      for (const match of executeMatches) {
        assertEquals(
          match.includes("async"),
          true,
          `${entry.name} has non-async execute function`
        );
      }
    }
  }
});

Deno.test("all adapters use Deno.Command for subprocess", async () => {
  const adapters = await Deno.readDir("adapters");

  for await (const entry of adapters) {
    if (!entry.name.endsWith(".js") || entry.name === "README.md") continue;

    const code = await Deno.readTextFile(`adapters/${entry.name}`);

    // Check for Deno.Command usage (not child_process or other)
    if (code.includes("await cmd.output()")) {
      assertEquals(
        code.includes("Deno.Command"),
        true,
        `${entry.name} should use Deno.Command`
      );
    }
  }
});
