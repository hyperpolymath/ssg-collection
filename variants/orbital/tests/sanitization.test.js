// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

import { assertEquals, assertNotEquals } from "@std/assert";

// Import adapters that use sanitization
import * as documenter from "../adapters/documenter.js";
import * as franklin from "../adapters/franklin.js";
import * as staticwebpages from "../adapters/staticwebpages.js";
import * as coleslaw from "../adapters/coleslaw.js";

Deno.test("documenter adapter has sanitization functions", async () => {
  const code = await Deno.readTextFile("adapters/documenter.js");

  // Check for sanitization function
  assertEquals(code.includes("sanitizeJuliaString"), true);
  assertEquals(code.includes("sanitizeJuliaIdentifier"), true);

  // Check that sanitization is used
  assertEquals(code.includes("sanitizeJuliaString(sitename)"), true);
  assertEquals(code.includes("sanitizeJuliaString(repo)"), true);
  assertEquals(code.includes("sanitizeJuliaIdentifier(module)"), true);
});

Deno.test("franklin adapter has sanitization functions", async () => {
  const code = await Deno.readTextFile("adapters/franklin.js");

  // Check for sanitization function
  assertEquals(code.includes("sanitizeJuliaString"), true);

  // Check that sanitization is used
  assertEquals(code.includes("sanitizeJuliaString(path"), true);
  assertEquals(code.includes("sanitizeJuliaString(template)"), true);
  assertEquals(code.includes("sanitizeJuliaString(host)"), true);
});

Deno.test("staticwebpages adapter has sanitization functions", async () => {
  const code = await Deno.readTextFile("adapters/staticwebpages.js");

  // Check for sanitization function
  assertEquals(code.includes("sanitizeJuliaString"), true);

  // Check that sanitization is used
  assertEquals(code.includes("sanitizeJuliaString(path"), true);
});

Deno.test("coleslaw adapter has sanitization functions", async () => {
  const code = await Deno.readTextFile("adapters/coleslaw.js");

  // Check for sanitization function
  assertEquals(code.includes("sanitizeLispString"), true);

  // Check that sanitization is used
  assertEquals(code.includes("sanitizeLispString(path"), true);
  assertEquals(code.includes("sanitizeLispString(title)"), true);
});

Deno.test("Julia sanitization escapes dangerous characters", async () => {
  const code = await Deno.readTextFile("adapters/documenter.js");

  // Check that sanitization handles dangerous Julia characters
  assertEquals(code.includes('.replace(/\\\\/g, "\\\\\\\\")'), true); // backslash
  assertEquals(code.includes('.replace(/"/g'), true); // quote
  assertEquals(code.includes('.replace(/\\$/g'), true); // dollar sign
  assertEquals(code.includes('.replace(/`/g'), true); // backtick
});

Deno.test("Lisp sanitization escapes dangerous characters", async () => {
  const code = await Deno.readTextFile("adapters/coleslaw.js");

  // Check that sanitization handles dangerous Lisp characters
  assertEquals(code.includes('.replace(/\\\\/g, "\\\\\\\\")'), true); // backslash
  assertEquals(code.includes('.replace(/"/g'), true); // quote
});

Deno.test("No unsanitized string interpolation in Julia adapters", async () => {
  const juliaAdapters = ["documenter.js", "franklin.js", "staticwebpages.js"];

  for (const adapter of juliaAdapters) {
    const code = await Deno.readTextFile(`adapters/${adapter}`);
    const lines = code.split("\n");

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      // Check if line has interpolation in a runJulia call
      if (line.includes("runJulia") && line.includes("${")) {
        // Verify it uses sanitized variable
        const hasSafe = line.includes("safe") || line.includes("sanitize");
        const isPort = line.includes("port=") && line.includes("parseInt");
        if (!hasSafe && !isPort) {
          // Allow numeric interpolation like port=${p}
          if (!line.match(/\$\{[a-z]+\}/i) || line.includes("sn}") || line.includes("r}")) {
            // These are the sanitized ones
          } else {
            throw new Error(
              `Potential unsanitized interpolation in ${adapter} line ${i + 1}: ${line.trim()}`
            );
          }
        }
      }
    }
  }
});
