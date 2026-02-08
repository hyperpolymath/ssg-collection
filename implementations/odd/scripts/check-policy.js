#!/usr/bin/env -S deno run --allow-read
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Language Policy Enforcement Script for odd-ssg

const BANNED_PATTERNS = {
  typescript: {
    extensions: [".ts", ".tsx", ".mts", ".cts"],
    message: "TypeScript files are banned. Use ReScript instead.",
  },
  nodejs: {
    files: ["package.json", "package-lock.json"],
    dirs: ["node_modules"],
    message: "Node.js package management is banned. Use Deno instead.",
  },
  bun: {
    files: ["bun.lockb", "bunfig.toml"],
    message: "Bun is banned. Use Deno instead.",
  },
  npm: {
    files: ["npm-shrinkwrap.json", ".npmrc"],
    message: "npm configuration is banned. Use Deno instead.",
  },
  makefile: {
    files: ["Makefile", "makefile", "GNUmakefile"],
    extensions: [".mk"],
    message: "Makefiles are banned. Use justfile instead.",
  },
  go: {
    extensions: [".go"],
    files: ["go.mod", "go.sum"],
    message: "Go is banned. Use Rust instead.",
  },
};

const ALLOWED_JS_LOCATIONS = [
  "src/ssg/deno_fs.js",
  "src/adapter_loader.js",
  "src/noteg-lang/lsp/main.js",
  "src/noteg-mcp/main.js",
  "src/tests/test_utils.js",
  "scripts/check-policy.js",
  "scripts/gen-docs.js",
  "scripts/serve-docs.js",
  "adapters/", // Adapters can be JS (FFI glue for external SSGs)
];

const IGNORE_DIRS = [
  ".git",
  "node_modules",
  "dist",
  "coverage",
  ".cache",
  "lib",
];

let violations = [];
let warnings = [];

async function walkDir(dir) {
  const entries = [];
  try {
    for await (const entry of Deno.readDir(dir)) {
      const path = `${dir}/${entry.name}`;

      // Skip ignored directories
      if (entry.isDirectory && IGNORE_DIRS.includes(entry.name)) {
        continue;
      }

      if (entry.isDirectory) {
        entries.push(...await walkDir(path));
      } else {
        entries.push({ path, name: entry.name });
      }
    }
  } catch {
    // Directory doesn't exist or not readable
  }
  return entries;
}

function checkFile(file) {
  const { path, name } = file;
  const relativePath = path.replace(/^\.\//, "");

  // Check banned patterns
  for (const [category, rules] of Object.entries(BANNED_PATTERNS)) {
    // Check extensions
    if (rules.extensions) {
      for (const ext of rules.extensions) {
        if (name.endsWith(ext)) {
          violations.push({
            category,
            path: relativePath,
            message: rules.message,
          });
          return;
        }
      }
    }

    // Check specific files
    if (rules.files) {
      for (const bannedFile of rules.files) {
        if (name === bannedFile) {
          violations.push({
            category,
            path: relativePath,
            message: rules.message,
          });
          return;
        }
      }
    }
  }

  // Check JavaScript files - only allowed in specific locations
  if (name.endsWith(".js") && !name.endsWith(".res.js")) {
    const isAllowed = ALLOWED_JS_LOCATIONS.some(allowed => {
      if (allowed.endsWith("/")) {
        return relativePath.startsWith(allowed);
      }
      return relativePath === allowed;
    });

    if (!isAllowed) {
      warnings.push({
        category: "javascript",
        path: relativePath,
        message: "JavaScript file outside allowed locations. Consider if this is necessary FFI glue.",
      });
    }
  }
}

async function checkSPDXHeaders(file) {
  const { path } = file;
  const relativePath = path.replace(/^\.\//, "");

  // Only check source files
  const sourceExtensions = [".res", ".resi", ".js", ".ncl", ".sh"];
  const hasSourceExt = sourceExtensions.some(ext => path.endsWith(ext));

  if (!hasSourceExt) return;

  try {
    const content = await Deno.readTextFile(path);
    const lines = content.split("\n").slice(0, 5).join("\n");

    if (!lines.includes("SPDX-License-Identifier:")) {
      warnings.push({
        category: "spdx",
        path: relativePath,
        message: "Missing SPDX-License-Identifier header.",
      });
    }
  } catch {
    // Can't read file
  }
}

async function main() {
  console.log("🔍 Checking language policy compliance...\n");

  const cwd = Deno.cwd();
  const files = await walkDir(cwd);

  for (const file of files) {
    checkFile(file);
    await checkSPDXHeaders(file);
  }

  // Check for banned directories
  for (const [category, rules] of Object.entries(BANNED_PATTERNS)) {
    if (rules.dirs) {
      for (const dir of rules.dirs) {
        try {
          const stat = await Deno.stat(`${cwd}/${dir}`);
          if (stat.isDirectory) {
            violations.push({
              category,
              path: dir,
              message: rules.message,
            });
          }
        } catch {
          // Directory doesn't exist - good!
        }
      }
    }
  }

  // Report results
  if (violations.length > 0) {
    console.log("❌ POLICY VIOLATIONS:\n");
    for (const v of violations) {
      console.log(`  [${v.category}] ${v.path}`);
      console.log(`    → ${v.message}\n`);
    }
  }

  if (warnings.length > 0) {
    console.log("⚠️  WARNINGS:\n");
    for (const w of warnings) {
      console.log(`  [${w.category}] ${w.path}`);
      console.log(`    → ${w.message}\n`);
    }
  }

  if (violations.length === 0 && warnings.length === 0) {
    console.log("✅ All policy checks passed!\n");
    console.log("Language Policy Summary:");
    console.log("  • Primary language: ReScript");
    console.log("  • Runtime: Deno");
    console.log("  • Build system: justfile + Mustfile.ncl");
    console.log("  • No banned languages detected");
  }

  // Exit with error if violations found
  if (violations.length > 0) {
    console.log(`\n❌ ${violations.length} violation(s) found. Please fix before committing.`);
    Deno.exit(1);
  }

  if (warnings.length > 0) {
    console.log(`\n⚠️  ${warnings.length} warning(s) found. Review recommended.`);
  }
}

main();
