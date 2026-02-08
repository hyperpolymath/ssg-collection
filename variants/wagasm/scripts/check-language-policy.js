#!/usr/bin/env -S deno run --allow-read
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
// wagasm-ssg Language Policy Enforcement
//
// ============================================================================
// CORE LANGUAGE: Pure WebAssembly Text (.wat)
// ============================================================================
// ALL SSG logic MUST be hand-written WAT in src/. NO exceptions.
// - NO AssemblyScript (explicitly forbidden)
// - NO Rust-compiled WASM
// - NO TypeScript
// The host runtime may use ReScript for I/O ONLY (runtime/).
//
// Standard Hyperpolymath bans also apply.

const CORE_LANGUAGE = "WebAssembly Text (WAT)";
const CORE_EXTENSIONS = [".wat"];
const CORE_DIRECTORIES = ["src/"];

const BANNED_EXTENSIONS = [".ts", ".tsx", ".go", ".as"]; // .as = AssemblyScript
const BANNED_FILES = new Set([
  "package-lock.json", "yarn.lock", "pnpm-lock.yaml", "bun.lockb",
  "asconfig.json", // AssemblyScript config - explicitly banned
]);

const ALLOWED_PYTHON_PATHS = ["salt/", "saltstack/", "_salt/"];
const RESCRIPT_ALLOWED_PATHS = ["runtime/", "adapters/", "tests/"];

async function* walkDir(dir) {
  for await (const entry of Deno.readDir(dir)) {
    const path = `${dir}/${entry.name}`;
    if (entry.name.startsWith(".") ||
        ["node_modules", "_site", "target", "_build"].includes(entry.name)) {
      continue;
    }
    if (entry.isDirectory) yield* walkDir(path);
    else yield path;
  }
}

async function checkPolicy() {
  const violations = [];
  const cwd = Deno.cwd();
  let coreFilesFound = false;

  console.log("=".repeat(60));
  console.log(`WAGASM-SSG LANGUAGE POLICY CHECK`);
  console.log(`Core Language: ${CORE_LANGUAGE}`);
  console.log("=".repeat(60) + "\n");

  for await (const file of walkDir(cwd)) {
    const relPath = file.replace(cwd + "/", "");
    const filename = file.split("/").pop();

    // Check for core language files
    for (const ext of CORE_EXTENSIONS) {
      if (filename.endsWith(ext)) {
        for (const dir of CORE_DIRECTORIES) {
          if (relPath.startsWith(dir)) coreFilesFound = true;
        }
      }
    }

    // Check banned extensions
    for (const ext of BANNED_EXTENSIONS) {
      if (file.endsWith(ext) && !file.endsWith(".d.ts")) {
        let fix = "Use ReScript instead";
        if (ext === ".go") fix = "Use Rust instead";
        if (ext === ".as") fix = "FORBIDDEN: AssemblyScript not allowed. Write pure WAT.";
        if (ext === ".ts") fix = "Write pure WAT for SSG logic, ReScript for host only";

        violations.push({
          file: relPath,
          reason: `Banned extension ${ext} - wagasm-ssg is PURE WAT only`,
          fix,
        });
      }
    }

    // Check banned files
    if (BANNED_FILES.has(filename)) {
      violations.push({
        file: relPath,
        reason: "Node.js/npm/AssemblyScript artifact (banned)",
        fix: "Remove. No AssemblyScript. Use Deno for runtime.",
      });
    }

    // Check Python
    if (filename.endsWith(".py")) {
      if (!ALLOWED_PYTHON_PATHS.some(p => relPath.startsWith(p))) {
        violations.push({
          file: relPath,
          reason: "Python outside SaltStack (banned)",
          fix: "Use ReScript or Rust instead",
        });
      }
    }

    // Check ReScript is only in host paths
    if (filename.endsWith(".res")) {
      if (!RESCRIPT_ALLOWED_PATHS.some(p => relPath.startsWith(p))) {
        violations.push({
          file: relPath,
          reason: "ReScript outside runtime/adapters",
          fix: `Core SSG logic must be in ${CORE_LANGUAGE}. ReScript is ONLY for host I/O.`,
        });
      }
    }

    // Check WASM files have WAT source
    if (filename.endsWith(".wasm")) {
      const watPath = file.replace(/\.wasm$/, ".wat");
      try {
        await Deno.stat(watPath);
      } catch {
        violations.push({
          file: relPath,
          reason: "WASM without WAT source",
          fix: "All WASM must be compiled from hand-written WAT. No Rust/AssemblyScript.",
        });
      }
    }
  }

  // CRITICAL: Core language files MUST exist
  if (!coreFilesFound) {
    violations.push({
      file: "src/",
      reason: `CRITICAL: No ${CORE_LANGUAGE} files found in src/`,
      fix: `Add .wat files to src/ - the SSG MUST be pure WAT`,
    });
  }

  if (violations.length === 0) {
    console.log(`✓ Core language (${CORE_LANGUAGE}) files present in src/`);
    console.log("✓ No AssemblyScript or TypeScript detected");
    console.log("✓ All policy checks passed!");
    Deno.exit(0);
  }

  console.log(`✗ ${violations.length} violation(s) found:\n`);
  for (const v of violations) {
    console.log(`  File: ${v.file}`);
    console.log(`  Issue: ${v.reason}`);
    console.log(`  Fix: ${v.fix}\n`);
  }
  Deno.exit(1);
}

await checkPolicy();
