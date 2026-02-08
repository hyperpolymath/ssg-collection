// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// AdapterLoader.res - Dynamic adapter loading from ./adapters directory

open Types

// External: Read directory entries (JS interop needed for async iteration)
@val
external loadAdaptersFromDir: string => promise<array<(string, adapter)>> = "loadAdaptersFromDir"

// Load all adapters
let loadAll = async (): int => {
  Logger.info("Loading adapters...")

  try {
    let loaded = await loadAdaptersFromDir("./adapters")

    loaded->Array.forEach(((name, adapter)) => {
      AdapterRegistry.registerAdapter(name, adapter)
      Logger.debug(`Loaded adapter: ${adapter.name} (${adapter.language})`)
    })

    let count = AdapterRegistry.adapterCount()
    let toolCount = AdapterRegistry.toolCount()
    Logger.info(`Loaded ${Belt.Int.toString(count)} adapters with ${Belt.Int.toString(toolCount)} tools`)
    count
  } catch {
  | _ =>
    Logger.warn("No adapters directory found or empty")
    0
  }
}
