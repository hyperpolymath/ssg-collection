// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// E2E tests for adapter connection lifecycle

import { assertEquals, assertExists } from "@std/assert";
import { describe, it, beforeEach, afterEach } from "@std/testing/bdd";

// Test adapters that are likely to be available in CI
// These use common tools that might be installed
const testableAdapters = [
  { file: "zola.js", binary: "zola" },
  { file: "mdbook.js", binary: "mdbook" },
];

// Check if a binary is available
async function binaryExists(name) {
  try {
    const cmd = new Deno.Command("which", { args: [name], stdout: "null", stderr: "null" });
    const { success } = await cmd.output();
    return success;
  } catch {
    return false;
  }
}

describe("Adapter Connection Lifecycle", () => {
  for (const { file, binary } of testableAdapters) {
    const adapterName = file.replace(".js", "");

    describe(adapterName, () => {
      let adapter;
      let binaryAvailable;

      beforeEach(async () => {
        adapter = await import(`../../adapters/${file}`);
        binaryAvailable = await binaryExists(binary);
      });

      afterEach(async () => {
        if (adapter && adapter.isConnected()) {
          await adapter.disconnect();
        }
      });

      it("should start disconnected", () => {
        assertEquals(adapter.isConnected(), false);
      });

      it("should handle connect when binary not available", async () => {
        if (binaryAvailable) {
          // Skip if binary is available - test the unavailable case
          return;
        }

        const connected = await adapter.connect();
        assertEquals(connected, false);
        assertEquals(adapter.isConnected(), false);
      });

      it("should connect successfully when binary is available", async () => {
        if (!binaryAvailable) {
          console.log(`  Skipping: ${binary} not installed`);
          return;
        }

        const connected = await adapter.connect();
        assertEquals(connected, true);
        assertEquals(adapter.isConnected(), true);
      });

      it("should disconnect cleanly", async () => {
        if (!binaryAvailable) {
          return;
        }

        await adapter.connect();
        await adapter.disconnect();
        assertEquals(adapter.isConnected(), false);
      });

      it("should handle multiple connect/disconnect cycles", async () => {
        if (!binaryAvailable) {
          return;
        }

        for (let i = 0; i < 3; i++) {
          const connected = await adapter.connect();
          assertEquals(connected, true);
          await adapter.disconnect();
          assertEquals(adapter.isConnected(), false);
        }
      });
    });
  }
});

describe("Tool Execution", () => {
  for (const { file, binary } of testableAdapters) {
    const adapterName = file.replace(".js", "");

    describe(`${adapterName} version tool`, () => {
      it("should have a version tool", async () => {
        const adapter = await import(`../../adapters/${file}`);
        const versionTool = adapter.tools.find(
          (t) => t.name.includes("version"),
        );
        assertExists(versionTool, `No version tool found for ${adapterName}`);
      });

      it("should execute version tool when connected", async () => {
        const binaryAvailable = await binaryExists(binary);
        if (!binaryAvailable) {
          console.log(`  Skipping: ${binary} not installed`);
          return;
        }

        const adapter = await import(`../../adapters/${file}`);
        const connected = await adapter.connect();
        if (!connected) return;

        try {
          const versionTool = adapter.tools.find(
            (t) => t.name.includes("version"),
          );
          const result = await versionTool.execute({});

          assertExists(result);
          assertEquals(typeof result, "object");
          assertExists(result.success);
        } finally {
          await adapter.disconnect();
        }
      });
    });
  }
});
