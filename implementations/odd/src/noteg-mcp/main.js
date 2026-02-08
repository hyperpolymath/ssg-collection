// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Minimal Deno FFI glue for MCP server - required for ReScript interop

import { createServer, handleRequest } from "./Server.res.js";

async function main() {
  const server = createServer();
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();
  const buffer = new Uint8Array(65536);
  let messageBuffer = "";
  let contentLength = 0;
  let headerComplete = false;

  console.error("odd-ssg MCP Server started");

  while (true) {
    const n = await Deno.stdin.read(buffer);
    if (n === null) break;

    messageBuffer += decoder.decode(buffer.subarray(0, n));

    while (true) {
      if (!headerComplete) {
        const headerEnd = messageBuffer.indexOf("\r\n\r\n");
        if (headerEnd === -1) break;

        const headers = messageBuffer.substring(0, headerEnd);
        const match = headers.match(/Content-Length:\s*(\d+)/i);
        if (match) {
          contentLength = parseInt(match[1]);
        }
        messageBuffer = messageBuffer.substring(headerEnd + 4);
        headerComplete = true;
      }

      if (headerComplete && messageBuffer.length >= contentLength) {
        const content = messageBuffer.substring(0, contentLength);
        messageBuffer = messageBuffer.substring(contentLength);
        headerComplete = false;

        try {
          const request = JSON.parse(content);
          const response = await handleRequest(server, request);

          const responseStr = JSON.stringify(response);
          const responseBytes = encoder.encode(responseStr);
          const header = `Content-Length: ${responseBytes.length}\r\n\r\n`;
          await Deno.stdout.write(encoder.encode(header));
          await Deno.stdout.write(responseBytes);
        } catch (e) {
          console.error("Error:", e);
        }
      } else {
        break;
      }
    }
  }
}

if (import.meta.main) {
  main();
}
