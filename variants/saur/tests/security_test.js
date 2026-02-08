// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Security tests for adapters

import { assertEquals } from "https://deno.land/std@0.208.0/assert/mod.ts";
import { walk } from "https://deno.land/std@0.208.0/fs/walk.ts";

// ─────────────────────────────────────────────────────────────────────────────
// Test helper: Read file content
// ─────────────────────────────────────────────────────────────────────────────

async function readAdapterContent(path: string): Promise<string> {
  return await Deno.readTextFile(path);
}

// ─────────────────────────────────────────────────────────────────────────────
// Test: No eval usage
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Security: No eval() usage in adapters", async () => {
  for await (const entry of walk("./adapters", { exts: [".js"], maxDepth: 1 })) {
    if (entry.isFile) {
      const content = await readAdapterContent(entry.path);
      const hasEval = /\beval\s*\(/.test(content);
      assertEquals(
        hasEval,
        false,
        `${entry.path}: Contains eval() - security risk!`
      );
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: No Function constructor
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Security: No Function() constructor in adapters", async () => {
  for await (const entry of walk("./adapters", { exts: [".js"], maxDepth: 1 })) {
    if (entry.isFile) {
      const content = await readAdapterContent(entry.path);
      const hasFunction = /new\s+Function\s*\(/.test(content);
      assertEquals(
        hasFunction,
        false,
        `${entry.path}: Contains new Function() - security risk!`
      );
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: No hardcoded credentials
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Security: No hardcoded passwords in adapters", async () => {
  const dangerousPatterns = [
    /password\s*[:=]\s*["'][^"']+["']/i,
    /secret\s*[:=]\s*["'][^"']+["']/i,
    /api_key\s*[:=]\s*["'][^"']+["']/i,
    /token\s*[:=]\s*["'][^"']+["']/i,
  ];

  for await (const entry of walk("./adapters", { exts: [".js"], maxDepth: 1 })) {
    if (entry.isFile) {
      const content = await readAdapterContent(entry.path);

      for (const pattern of dangerousPatterns) {
        const matches = content.match(pattern);
        // Allow if it's clearly a variable or placeholder
        if (matches) {
          const match = matches[0];
          const isPlaceholder =
            match.includes("${") ||
            match.includes("process.env") ||
            match.includes("Deno.env") ||
            match.includes("undefined") ||
            match.includes("null") ||
            match.includes("''") ||
            match.includes('""');

          assertEquals(
            isPlaceholder,
            true,
            `${entry.path}: Potential hardcoded credential: ${match}`
          );
        }
      }
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: SPDX headers present
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Security: All adapters have SPDX headers", async () => {
  for await (const entry of walk("./adapters", { exts: [".js"], maxDepth: 1 })) {
    if (entry.isFile) {
      const content = await readAdapterContent(entry.path);
      const hasSPDX = content.includes("SPDX-License-Identifier");
      assertEquals(
        hasSPDX,
        true,
        `${entry.path}: Missing SPDX-License-Identifier header`
      );
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: Safe command execution pattern
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Security: Commands use array args (not string interpolation)", async () => {
  for await (const entry of walk("./adapters", { exts: [".js"], maxDepth: 1 })) {
    if (entry.isFile) {
      const content = await readAdapterContent(entry.path);

      // Check for dangerous pattern: shell command with string interpolation
      const dangerousShell = /new\s+Deno\.Command\s*\(\s*["']sh["']\s*,\s*\{\s*args:\s*\[\s*["']-c["']/.test(content);
      assertEquals(
        dangerousShell,
        false,
        `${entry.path}: Uses 'sh -c' pattern - potential command injection risk`
      );
    }
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// Test: No absolute paths to sensitive locations
// ─────────────────────────────────────────────────────────────────────────────

Deno.test("Security: No hardcoded paths to sensitive directories", async () => {
  const sensitivePaths = [
    /["']\/etc\//,
    /["']\/root\//,
    /["']\/home\/[^"']+\/\./,  // Hidden files in home dirs
    /["']~\/\./,
  ];

  for await (const entry of walk("./adapters", { exts: [".js"], maxDepth: 1 })) {
    if (entry.isFile) {
      const content = await readAdapterContent(entry.path);

      for (const pattern of sensitivePaths) {
        const hasSensitive = pattern.test(content);
        assertEquals(
          hasSensitive,
          false,
          `${entry.path}: Contains hardcoded path to sensitive location`
        );
      }
    }
  }
});
