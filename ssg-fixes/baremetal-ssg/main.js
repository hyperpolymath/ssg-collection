// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Baremetal SSG - Main entry point
 * Bare-metal SSG adapters for 28 static site generators
 */

const adapters = new Map();

// Dynamically load all adapters
async function loadAdapters() {
  const adapterFiles = [];
  for await (const entry of Deno.readDir("./adapters")) {
    if (entry.isFile && entry.name.endsWith(".js")) {
      adapterFiles.push(entry.name);
    }
  }

  for (const file of adapterFiles) {
    try {
      const adapter = await import(`./adapters/${file}`);
      if (adapter.name) {
        adapters.set(adapter.name.toLowerCase(), adapter);
        console.log(`Loaded adapter: ${adapter.name}`);
      }
    } catch (error) {
      console.error(`Failed to load adapter ${file}:`, error.message);
    }
  }

  console.log(`\nLoaded ${adapters.size} adapters`);
}

// List all available adapters
function listAdapters() {
  console.log("\nAvailable adapters:");
  for (const [name, adapter] of adapters) {
    console.log(`  - ${adapter.name} (${adapter.language})`);
  }
}

// Get adapter by name
function getAdapter(name) {
  return adapters.get(name.toLowerCase());
}

// Health check endpoint
function health() {
  return {
    status: "healthy",
    adapters: adapters.size,
    version: "1.0.0",
  };
}

// Main entry point
async function main() {
  console.log("Baremetal SSG v1.0.0");
  console.log("====================\n");

  await loadAdapters();
  listAdapters();

  console.log("\nHealth check:", health());
}

// Run if executed directly
if (import.meta.main) {
  main();
}

export { adapters, getAdapter, health, listAdapters, loadAdapters };
