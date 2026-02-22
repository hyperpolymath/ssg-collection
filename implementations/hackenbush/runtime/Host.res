// SPDX-License-Identifier: AGPL-3.0-or-later
// hackenbush-ssg Host Runtime (ReScript)
//
// This is the execution environment for the Hackenbush SSG. 
// 
// PHILOSOPHY: The host is "dumb". It only knows how to simulate the 
// Game of Life. All site generation logic (routing, templating, 
// markdown parsing) is implemented via patterns within the simulation grid.

// CONFIGURATION: Designated coordinates for the "Memory Map".
// The host reads bytes from these specific cells to produce output files.
let outputRegionX = 1000
let outputRegionY = 0
let outputWidth = 128
let outputHeight = 32

// STORAGE: The grid is represented as a Set of coordinate strings ("x,y").
// This allows for an "infinite" sparse grid without pre-allocating large arrays.
module Grid = Belt.Set.String

// SIMULATION ENGINE: Applies the B3/S23 (Conway) rules to the grid.
let evolve = (grid: Grid.t): Grid.t => {
  let candidates = ref(Grid.empty)

  // OPTIMIZATION: Only check cells that could possibly change state
  // (currently alive cells and their immediate neighbors).
  Grid.forEach(grid, cell => {
    let (x, y) = parseKey(cell)
    for dy in -1 to 1 {
      for dx in -1 to 1 {
        candidates := Grid.add(candidates.contents, cellKey(x + dx, y + dy))
      }
    }
  })

  let newGrid = ref(Grid.empty)

  Grid.forEach(candidates.contents, cell => {
    let (x, y) = parseKey(cell)
    let alive = isAlive(grid, x, y)
    let neighbors = countNeighbors(grid, x, y)

    // CONWAY RULES:
    // 1. Any live cell with 2 or 3 neighbors survives.
    // 2. Any dead cell with exactly 3 neighbors becomes alive.
    if alive && (neighbors == 2 || neighbors == 3) {
      newGrid := Grid.add(newGrid.contents, cell)
    } else if !alive && neighbors == 3 {
      newGrid := Grid.add(newGrid.contents, cell)
    }
  })

  newGrid.contents
}

// IO BRIDGE: Converts the state of specific grid regions into byte sequences.
// This allows the "Life Computer" to "write" to the filesystem.
let readOutput = (grid: Grid.t): Js.TypedArray2.Uint8Array.t => {
  let bytes = []
  // ... [bit-packing logic]
  Js.TypedArray2.Uint8Array.make(bytes)
}
