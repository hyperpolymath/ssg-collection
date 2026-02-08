// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
// Language Policy Enforcement Script
//
// This script enforces the Hyperpolymath Language Policy:
// - TypeScript is BANNED (use ReScript instead)
// - Node.js/npm is BANNED (use Deno instead)
// - Go is BANNED (use Rust instead)
// - Python is BANNED except for SaltStack

const BANNED_EXTENSIONS = [
  ".ts",   // TypeScript - use ReScript
  ".tsx",  // TypeScript JSX - use ReScript
  ".go",   // Go - use Rust
];

const BANNED_FILES = [
  "package-lock.json",  // npm - use Deno
  "yarn.lock",          // yarn - use Deno
  "pnpm-lock.yaml",     // pnpm - use Deno
  "bun.lockb",          // bun - use Deno
];

const ALLOWED_PYTHON_PATHS = [
  "salt/",
  "saltstack/",
  "_salt/",
];

async function* walkDir(dir) {
  for await (const entry of Deno.readDir(dir)) {
    const path = `${dir}/${entry.name}`;

    // Skip hidden dirs and common non-source dirs
    if (entry.name.startsWith(".") ||
        entry.name === "node_modules" ||
        entry.name === "_site" ||
        entry.name === "target" ||
        entry.name === "_build") {
      continue;
    }

    if (entry.isDirectory) {
      yield* walkDir(path);
    } else {
      yield path;
    }
  }
}

async function checkPolicy() {
  const violations = [];
  const cwd = Deno.cwd();

  console.log("[POLICY] Checking language policy compliance...");
  console.log("[POLICY] Working directory:", cwd);

  for await (const file of walkDir(cwd)) {
    const relPath = file.replace(cwd + "/", "");

    // Check banned extensions
    for (const ext of BANNED_EXTENSIONS) {
      if (file.endsWith(ext)) {
        // Special case: .d.ts files are allowed for type declarations
        if (file.endsWith(".d.ts")) {
          continue;
        }

        const replacement = ext === ".ts" || ext === ".tsx"
          ? "ReScript (.res)"
          : ext === ".go"
            ? "Rust (.rs)"
            : "approved language";

        violations.push({
          file: relPath,
          reason: `Banned extension ${ext} - use ${replacement} instead`,
        });
      }
    }

    // Check banned files
    for (const banned of BANNED_FILES) {
      if (file.endsWith(banned)) {
        violations.push({
          file: relPath,
          reason: `Banned file ${banned} - use Deno (deno.json) instead`,
        });
      }
    }

    // Check Python files
    if (file.endsWith(".py")) {
      const isAllowed = ALLOWED_PYTHON_PATHS.some(p => relPath.startsWith(p));
      if (!isAllowed) {
        violations.push({
          file: relPath,
          reason: "Python is only allowed for SaltStack - use ReScript or Rust instead",
        });
      }
    }
  }

  if (violations.length > 0) {
    console.log("\n[POLICY] VIOLATIONS FOUND:");
    console.log("=".repeat(60));

    for (const v of violations) {
      console.log(`\n  File: ${v.file}`);
      console.log(`  Reason: ${v.reason}`);
    }

    console.log("\n" + "=".repeat(60));
    console.log(`[POLICY] Total violations: ${violations.length}`);
    console.log("[POLICY] See CLAUDE.md for the Language Policy");

    Deno.exit(1);
  }

  console.log("[POLICY] All checks passed - no violations found");
}

await checkPolicy();
