// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Main.res - rats-ssg MCP Server entry point

open Types

// Check if running in stdio mode
let isStdioMode = (): bool => {
  Bindings.Deno.args->Array.some(arg => arg === "--stdio")
}

// Main entry point
let main = async () => {
  let config = Config.make()

  // Set log level from config
  Logger.setLevelFromString(config.logLevel)

  Logger.info("rats-ssg MCP Server v0.2.0")
  Logger.info(`Port: ${Belt.Int.toString(config.port)}, Host: ${config.host}`)

  // Load adapters
  let _ = await AdapterLoader.loadAll()

  // Start appropriate server
  if isStdioMode() {
    await StdioServer.start()
  } else {
    HttpServer.start(config)
  }
}

// Run if main module
let _ = main()->Promise.catch(err => {
  Logger.error(`Fatal error: ${Obj.magic(err)}`)
  Bindings.Deno.exit(1)
  Promise.resolve()
})
