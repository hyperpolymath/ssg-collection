// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// deno_glue.js - Minimal Deno runtime interop for ReScript
// This is the ONLY JavaScript file needed - provides Deno API bridges

/**
 * Load adapters from directory
 * @param {string} dir - Directory path
 * @returns {Promise<Array<[string, object]>>} - Array of [name, adapter] tuples
 */
globalThis.loadAdaptersFromDir = async function (dir) {
  const adapters = [];

  try {
    for await (const entry of Deno.readDir(dir)) {
      if (entry.isFile && entry.name.endsWith(".js")) {
        try {
          const adapter = await import(`${dir}/${entry.name}`);
          const name = entry.name.replace(".js", "");
          adapters.push([name, adapter]);
        } catch (err) {
          console.warn(`[WARN] Failed to load adapter ${entry.name}: ${err}`);
        }
      }
    }
  } catch (_err) {
    // Directory doesn't exist or is empty
  }

  return adapters;
};

/**
 * Start HTTP server using Deno.serve
 * @param {number} port - Port number
 * @param {string} hostname - Host name
 * @param {function} handler - Request handler
 */
globalThis.startHttpServer = function (port, hostname, handler) {
  Deno.serve({ port, hostname }, handler);
};

/**
 * Start stdio server for MCP
 * @param {function} handleLine - Line handler function
 * @returns {Promise<void>}
 */
globalThis.startStdioServer = async function (handleLine) {
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();

  for await (const chunk of Deno.stdin.readable) {
    const lines = decoder.decode(chunk).split("\n").filter((l) => l.trim());

    for (const line of lines) {
      try {
        const response = await handleLine(line);
        await Deno.stdout.write(encoder.encode(response + "\n"));
      } catch (err) {
        console.error(`[ERROR] MCP error: ${err}`);
      }
    }
  }
};
