#!/usr/bin/env -S deno run --allow-read
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
// pharos-ssg Language Policy Enforcement
//
// ============================================================================
// CORE LANGUAGE: Pharo Smalltalk (.st, .class.st)
// ============================================================================
// ALL SSG logic MUST be in Smalltalk in engine/.
// The adapter may use ReScript for MCP integration only.
// Ada was incorrectly used before - it is BANNED for this repo.
//
// Standard Hyperpolymath bans also apply.

const CORE_LANGUAGE = "Pharo Smalltalk";
const CORE_EXTENSIONS = [".st", ".class.st"];
const CORE_DIRECTORIES = ["engine/"];

const BANNED_EXTENSIONS = [".ts", ".tsx", ".go"];
const WRONG_LANGUAGE_EXTENSIONS = [".adb", ".ads", ".gpr"]; // Ada - wrong for this repo
const BANNED_FILES = new Set([
  "package-lock.json", "yarn.lock", "pnpm-lock.yaml", "bun.lockb",
]);

const ALLOWED_PYTHON_PATHS = ["salt/", "saltstack/", "_salt/"];
const RESCRIPT_ALLOWED_PATHS = ["adapters/", "tests/"];

async function* walkDir(dir) {
  for await (const entry of Deno.readDir(dir)) {
    const path = `${dir}/${entry.name}`;
    if (entry.name.startsWith(".") ||
        ["node_modules", "_site", "target", "_build"].includes(entry.name) ||
        entry.name.endsWith(".image")) { // Skip Pharo image files
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
  console.log(`PHAROS-SSG LANGUAGE POLICY CHECK`);
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

    // Check for WRONG language (Ada was incorrectly used)
    for (const ext of WRONG_LANGUAGE_EXTENSIONS) {
      if (filename.endsWith(ext)) {
        violations.push({
          file: relPath,
          reason: `WRONG LANGUAGE: Ada file found (${ext})`,
          fix: `pharos-ssg must use Smalltalk (.st), NOT Ada. Remove and replace.`,
        });
      }
    }

    // Check banned extensions
    for (const ext of BANNED_EXTENSIONS) {
      if (file.endsWith(ext) && !file.endsWith(".d.ts")) {
        violations.push({
          file: relPath,
          reason: `Banned extension ${ext}`,
          fix: ext === ".go" ? "Use Rust instead" : "Use ReScript instead",
        });
      }
    }

    // Check banned files
    if (BANNED_FILES.has(filename)) {
      violations.push({
        file: relPath,
        reason: "Node.js/npm artifact (banned)",
        fix: "Remove and use Deno (deno.json) instead",
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

    // Check ReScript is only in allowed paths
    if (filename.endsWith(".res")) {
      if (!RESCRIPT_ALLOWED_PATHS.some(p => relPath.startsWith(p))) {
        violations.push({
          file: relPath,
          reason: "ReScript outside adapter directories",
          fix: `Core SSG logic must be in ${CORE_LANGUAGE}. Move to adapters/.`,
        });
      }
    }
  }

  // CRITICAL: Core language files MUST exist
  if (!coreFilesFound) {
    violations.push({
      file: "engine/",
      reason: `CRITICAL: No ${CORE_LANGUAGE} files found in engine/`,
      fix: `Add .st or .class.st files to engine/src/`,
    });
  }

  if (violations.length === 0) {
    console.log(`✓ Core language (${CORE_LANGUAGE}) files present in engine/`);
    console.log("✓ No Ada files detected (previously incorrect)");
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
