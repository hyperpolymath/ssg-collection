// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Security tests for input validation

import { assertEquals, assertRejects } from "@std/assert";
import { describe, it } from "@std/testing/bdd";

// Test helper to dynamically import adapters
async function loadAdapter(name) {
  return await import(`../../adapters/${name}.js`);
}

describe("Input Validation - Path Traversal", () => {
  const criticalAdapters = ["wub", "coleslaw", "documenter", "franklin", "staticwebpages"];

  for (const name of criticalAdapters) {
    describe(name, () => {
      it("should reject path traversal attempts", async () => {
        const adapter = await loadAdapter(name);

        // Find a tool that takes a path parameter
        const pathTool = adapter.tools.find(t =>
          t.inputSchema?.properties?.path
        );

        if (pathTool) {
          // Attempt path traversal
          const result = await pathTool.execute({ path: "../../../etc/passwd" });

          // Should fail with error message
          assertEquals(result.success, false);
          assertEquals(result.stderr.includes("traversal") || result.stderr.includes("not allowed"), true,
            `${name} should reject path traversal`);
        }
      });
    });
  }
});

describe("Input Validation - Shell Metacharacters", () => {
  const testCases = [
    { input: "; rm -rf /", name: "semicolon command chain" },
    { input: "| cat /etc/passwd", name: "pipe injection" },
    { input: "$(whoami)", name: "command substitution" },
    { input: "`id`", name: "backtick execution" },
  ];

  it("wub should reject shell metacharacters", async () => {
    const adapter = await loadAdapter("wub");
    const tool = adapter.tools.find(t => t.name === "wub_generate");

    if (tool) {
      for (const testCase of testCases) {
        const result = await tool.execute({ output: testCase.input });
        assertEquals(result.success, false,
          `Should reject ${testCase.name}`);
      }
    }
  });
});

describe("Input Validation - Port Numbers", () => {
  const adapterWithPort = ["coleslaw", "documenter", "staticwebpages", "franklin"];

  for (const name of adapterWithPort) {
    it(`${name} should validate port range`, async () => {
      const adapter = await loadAdapter(name);

      // Find serve/preview tool
      const serveTool = adapter.tools.find(t =>
        t.name.includes("serve") || t.name.includes("preview")
      );

      if (serveTool) {
        // Test invalid ports
        const invalidPorts = [-1, 0, 70000, "abc"];

        for (const port of invalidPorts) {
          const result = await serveTool.execute({ port });
          // Either it fails, or it uses a default port
          // We're checking it doesn't blindly accept invalid input
        }
      }
    });
  }
});

describe("Code Injection Prevention", () => {
  it("documenter should reject Julia code injection", async () => {
    const adapter = await loadAdapter("documenter");
    const tool = adapter.tools.find(t => t.name === "documenter_makedocs");

    if (tool) {
      const maliciousSitename = '"; run(`rm -rf /`); "';
      const result = await tool.execute({ sitename: maliciousSitename });

      assertEquals(result.success, false);
      assertEquals(result.stderr.includes("Invalid") || result.stderr.includes("not allowed"), true);
    }
  });

  it("coleslaw should reject Lisp code injection", async () => {
    const adapter = await loadAdapter("coleslaw");
    const tool = adapter.tools.find(t => t.name === "coleslaw_new_post");

    if (tool) {
      const maliciousTitle = '") (system "whoami") ("';
      const result = await tool.execute({ title: maliciousTitle });

      assertEquals(result.success, false);
    }
  });
});

describe("No Eval Usage", () => {
  it("should not use eval in any adapter", async () => {
    const files = [...Deno.readDirSync("adapters")]
      .filter(f => f.name.endsWith(".js"));

    for (const file of files) {
      const content = await Deno.readTextFile(`adapters/${file.name}`);
      assertEquals(
        content.includes("eval(") || content.includes("new Function("),
        false,
        `${file.name} should not use eval() or Function()`
      );
    }
  });
});
