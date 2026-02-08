#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// 60-ssg Language Policy Enforcement
// This SSG MUST be written in ALGOL 60 - no exceptions

const fs = require("fs");
const path = require("path");

const CORE_LANGUAGE = "ALGOL 60";
const CORE_EXTENSIONS = [".alg", ".a60", ".algol", ".algol60"];
const CORE_DIRECTORIES = ["engine/"];

// Banned extensions that should NOT appear in this repo
const BANNED_EXTENSIONS = [".ts", ".tsx", ".go", ".py", ".java", ".kt"];

// ReScript is allowed ONLY in adapter directories
const RESCRIPT_ALLOWED_PATHS = ["adapters/", "runtime/", "mcp/"];

function findFiles(dir, extensions, results = []) {
  if (!fs.existsSync(dir)) return results;
  const files = fs.readdirSync(dir);
  for (const file of files) {
    const filePath = path.join(dir, file);
    const stat = fs.statSync(filePath);
    if (stat.isDirectory() && !file.startsWith(".") && file !== "node_modules") {
      findFiles(filePath, extensions, results);
    } else if (extensions.some((ext) => file.endsWith(ext))) {
      results.push(filePath);
    }
  }
  return results;
}

function checkCoreLanguage() {
  console.log(`\n🔍 Checking ${CORE_LANGUAGE} core implementation...\n`);
  console.log("   Note: ALGOL 60 is the 1960 version of the ALGOL programming language");

  let hasCore = false;
  for (const dir of CORE_DIRECTORIES) {
    const files = findFiles(dir, CORE_EXTENSIONS);
    if (files.length > 0) {
      hasCore = true;
      console.log(`✅ Found ${files.length} ${CORE_LANGUAGE} files in ${dir}`);
      files.forEach((f) => console.log(`   - ${f}`));
    }
  }

  if (!hasCore) {
    console.error(`\n❌ ERROR: No ${CORE_LANGUAGE} core files found!`);
    console.error(`   This SSG MUST be written in ${CORE_LANGUAGE}.`);
    console.error(`   Required extensions: ${CORE_EXTENSIONS.join(", ")}`);
    console.error(`   Required directories: ${CORE_DIRECTORIES.join(", ")}`);
    return false;
  }

  return true;
}

function checkBannedLanguages() {
  console.log("\n🔍 Checking for banned languages...\n");

  const banned = findFiles(".", BANNED_EXTENSIONS);
  const violations = banned.filter(
    (f) => !f.includes("node_modules") && !f.includes(".git")
  );

  if (violations.length > 0) {
    console.error("❌ ERROR: Found files in banned languages:");
    violations.forEach((f) => console.error(`   - ${f}`));
    console.error("\n   These languages are NOT allowed in this repo.");
    return false;
  }

  console.log("✅ No banned language files found");
  return true;
}

function checkReScriptPlacement() {
  console.log("\n🔍 Checking ReScript file placement...\n");

  const resFiles = findFiles(".", [".res", ".resi"]);
  const misplaced = resFiles.filter((f) => {
    return !RESCRIPT_ALLOWED_PATHS.some((allowed) => f.includes(allowed));
  });

  if (misplaced.length > 0) {
    console.warn("⚠️  WARNING: ReScript files outside adapter directories:");
    misplaced.forEach((f) => console.warn(`   - ${f}`));
    console.warn(`\n   ReScript should only be in: ${RESCRIPT_ALLOWED_PATHS.join(", ")}`);
  } else if (resFiles.length > 0) {
    console.log("✅ ReScript files are properly placed in adapter directories");
  } else {
    console.log("ℹ️  No ReScript files found (adapters not yet implemented)");
  }

  return true;
}

function main() {
  console.log("═".repeat(60));
  console.log(`60-ssg Language Policy Check`);
  console.log(`Core Language: ${CORE_LANGUAGE} (MANDATORY)`);
  console.log("═".repeat(60));

  const checks = [checkCoreLanguage(), checkBannedLanguages(), checkReScriptPlacement()];

  console.log("\n" + "═".repeat(60));
  if (checks.every(Boolean)) {
    console.log("✅ All language policy checks passed!");
    console.log("═".repeat(60) + "\n");
    process.exit(0);
  } else {
    console.error("❌ Language policy violations detected!");
    console.error("═".repeat(60) + "\n");
    process.exit(1);
  }
}

main();
