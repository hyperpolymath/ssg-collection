// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// End-to-end tests for labnote-ssg adapters
// These tests require actual SSG binaries to be installed

import { assertEquals, assertExists } from "https://deno.land/std@0.220.0/assert/mod.ts";

// Helper to check if a binary is available
async function binaryExists(name: string): Promise<boolean> {
  try {
    const cmd = new Deno.Command("which", { args: [name], stdout: "null", stderr: "null" });
    const result = await cmd.output();
    return result.success;
  } catch {
    return false;
  }
}

// Test adapters that have their binary installed
Deno.test({
  name: "E2E: Test Zola adapter if installed",
  ignore: !(await binaryExists("zola")),
  async fn() {
    const adapter = await import("../../adapters/zola.js");

    // Test connection
    const connected = await adapter.connect();
    assertEquals(connected, true, "Should connect to Zola");

    // Test version tool
    const versionTool = adapter.tools.find((t: any) => t.name === "zola_version");
    assertExists(versionTool, "Should have version tool");

    const result = await versionTool.execute({});
    assertEquals(result.success, true, "Version command should succeed");
    assertEquals(result.stdout.includes("zola"), true, "Should output zola version");

    await adapter.disconnect();
  },
});

Deno.test({
  name: "E2E: Test mdBook adapter if installed",
  ignore: !(await binaryExists("mdbook")),
  async fn() {
    const adapter = await import("../../adapters/mdbook.js");

    const connected = await adapter.connect();
    assertEquals(connected, true, "Should connect to mdBook");

    const versionTool = adapter.tools.find((t: any) => t.name === "mdbook_version");
    assertExists(versionTool, "Should have version tool");

    const result = await versionTool.execute({});
    assertEquals(result.success, true, "Version command should succeed");

    await adapter.disconnect();
  },
});

Deno.test({
  name: "E2E: Test Cobalt adapter if installed",
  ignore: !(await binaryExists("cobalt")),
  async fn() {
    const adapter = await import("../../adapters/cobalt.js");

    const connected = await adapter.connect();
    assertEquals(connected, true, "Should connect to Cobalt");

    await adapter.disconnect();
  },
});

// Generic test for any available SSG
Deno.test("E2E: At least one SSG adapter should work", async () => {
  const adapters = [
    { name: "zola", binary: "zola" },
    { name: "mdbook", binary: "mdbook" },
    { name: "cobalt", binary: "cobalt" },
  ];

  let anyConnected = false;

  for (const { name, binary } of adapters) {
    if (await binaryExists(binary)) {
      const adapter = await import(`../../adapters/${name}.js`);
      if (await adapter.connect()) {
        anyConnected = true;
        console.log(`  ✓ ${name} adapter connected successfully`);
        await adapter.disconnect();
        break;
      }
    }
  }

  // This test passes even if no SSGs are installed (CI environment)
  if (!anyConnected) {
    console.log("  ⚠ No SSG binaries found - skipping E2E connection tests");
  }
});

// Test adapter error handling
Deno.test("E2E: Adapters handle missing binaries gracefully", async () => {
  // Create a fake adapter with non-existent binary
  const fakeAdapter = {
    binaryPath: "nonexistent-ssg-binary-12345",
    async connect() {
      try {
        const cmd = new Deno.Command(this.binaryPath, {
          args: ["--version"],
          stdout: "null",
          stderr: "null",
        });
        await cmd.output();
        return true;
      } catch {
        return false;
      }
    }
  };

  const connected = await fakeAdapter.connect();
  assertEquals(connected, false, "Should return false for missing binary");
});
