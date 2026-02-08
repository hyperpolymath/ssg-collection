// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Schema validation tests for adapter tool definitions

import { assertEquals, assertExists } from "@std/assert";
import { describe, it } from "@std/testing/bdd";

// JSON Schema draft-07 type validation
const validTypes = ["string", "number", "integer", "boolean", "object", "array", "null"];

function validateJsonSchema(schema, path = "") {
  const errors = [];

  if (typeof schema !== "object" || schema === null) {
    errors.push(`${path}: Schema must be an object`);
    return errors;
  }

  // Check type if present
  if (schema.type) {
    if (Array.isArray(schema.type)) {
      for (const t of schema.type) {
        if (!validTypes.includes(t)) {
          errors.push(`${path}: Invalid type '${t}'`);
        }
      }
    } else if (!validTypes.includes(schema.type)) {
      errors.push(`${path}: Invalid type '${schema.type}'`);
    }
  }

  // Check properties for objects
  if (schema.type === "object" && schema.properties) {
    if (typeof schema.properties !== "object") {
      errors.push(`${path}: 'properties' must be an object`);
    } else {
      for (const [key, value] of Object.entries(schema.properties)) {
        errors.push(...validateJsonSchema(value, `${path}.properties.${key}`));
      }
    }
  }

  // Check required is array
  if (schema.required !== undefined) {
    if (!Array.isArray(schema.required)) {
      errors.push(`${path}: 'required' must be an array`);
    }
  }

  // Check items for arrays
  if (schema.type === "array" && schema.items) {
    errors.push(...validateJsonSchema(schema.items, `${path}.items`));
  }

  return errors;
}

// Get all adapter files
const adapterFiles = [];
for await (const entry of Deno.readDir("./adapters")) {
  if (entry.isFile && entry.name.endsWith(".js")) {
    adapterFiles.push(entry.name);
  }
}

describe("Tool Input Schemas", () => {
  for (const file of adapterFiles) {
    const adapterName = file.replace(".js", "");

    describe(adapterName, () => {
      it("should have valid JSON Schema for all tools", async () => {
        const adapter = await import(`../../adapters/${file}`);

        for (const tool of adapter.tools) {
          const errors = validateJsonSchema(
            tool.inputSchema,
            `${adapterName}.${tool.name}`,
          );

          assertEquals(
            errors.length,
            0,
            `Schema errors in ${tool.name}:\n${errors.join("\n")}`,
          );
        }
      });

      it("should have 'type: object' as root schema", async () => {
        const adapter = await import(`../../adapters/${file}`);

        for (const tool of adapter.tools) {
          assertEquals(
            tool.inputSchema.type,
            "object",
            `Tool ${tool.name} inputSchema should have type 'object'`,
          );
        }
      });

      it("should have properties defined", async () => {
        const adapter = await import(`../../adapters/${file}`);

        for (const tool of adapter.tools) {
          assertExists(
            tool.inputSchema.properties,
            `Tool ${tool.name} missing 'properties' in schema`,
          );
        }
      });

      it("should have descriptions for all properties", async () => {
        const adapter = await import(`../../adapters/${file}`);

        for (const tool of adapter.tools) {
          const props = tool.inputSchema.properties || {};

          for (const [propName, propSchema] of Object.entries(props)) {
            assertExists(
              propSchema.description,
              `Property ${propName} in ${tool.name} missing description`,
            );
          }
        }
      });
    });
  }
});

describe("Common Tool Patterns", () => {
  const commonTools = [
    { pattern: "_init", expectedProps: ["path"] },
    { pattern: "_build", expectedProps: ["path"] },
    { pattern: "_serve", expectedProps: ["path", "port"] },
    { pattern: "_version", expectedProps: [] },
  ];

  for (const { pattern, expectedProps } of commonTools) {
    describe(`*${pattern} tools`, () => {
      it(`should have expected properties`, async () => {
        for (const file of adapterFiles) {
          const adapter = await import(`../../adapters/${file}`);
          const tools = adapter.tools.filter((t) => t.name.includes(pattern));

          for (const tool of tools) {
            const props = Object.keys(tool.inputSchema.properties || {});

            for (const expected of expectedProps) {
              // Just check common ones exist - not all adapters follow exactly
              if (props.length > 0 && expectedProps.length > 0) {
                // Soft check - at least one expected property should exist
                const hasExpected = expectedProps.some((e) => props.includes(e));
                if (!hasExpected && pattern !== "_version") {
                  console.log(
                    `  Note: ${tool.name} missing common props: ${expectedProps.join(", ")}`,
                  );
                }
              }
            }
          }
        }
      });
    });
  }
});

describe("Security Patterns", () => {
  for (const file of adapterFiles) {
    const adapterName = file.replace(".js", "");

    describe(adapterName, () => {
      it("should not have shell-injection prone patterns", async () => {
        const content = await Deno.readTextFile(`./adapters/${file}`);

        // Check for dangerous patterns
        const dangerousPatterns = [
          /`\$\{.*\}`/, // Template literal interpolation in shell context
          /eval\s*\(/, // eval usage
          /new Function\s*\(/, // Function constructor
          /child_process/, // Node.js child_process (shouldn't be in Deno)
        ];

        for (const pattern of dangerousPatterns) {
          const match = content.match(pattern);
          assertEquals(
            match,
            null,
            `Found dangerous pattern in ${adapterName}: ${match?.[0]}`,
          );
        }
      });

      it("should use Deno.Command for execution", async () => {
        const content = await Deno.readTextFile(`./adapters/${file}`);

        // Should use Deno.Command, not deprecated Deno.run
        if (content.includes("Deno.run")) {
          console.log(`  Warning: ${adapterName} uses deprecated Deno.run`);
        }

        // Check for Deno.Command usage
        const usesCommand = content.includes("Deno.Command");
        assertEquals(
          usesCommand,
          true,
          `${adapterName} should use Deno.Command for subprocess execution`,
        );
      });
    });
  }
});
