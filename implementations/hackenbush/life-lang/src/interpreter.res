// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// hackenbush-ssg - Life Pattern Interpreter (ReScript)
// Executes Game of Life simulation directly

open Parser

// Simulation state
type simulationState = {
  grid: array<array<bool>>,
  generation: int,
  width: int,
  height: int,
  rule: string,
}

// Create initial simulation state from pattern
let init = (pattern: pattern): simulationState => {
  {
    grid: pattern.cells,
    generation: 0,
    width: pattern.metadata.width,
    height: pattern.metadata.height,
    rule: pattern.metadata.rule,
  }
}

// Count live neighbors for a cell
let countNeighbors = (state: simulationState, x: int, y: int): int => {
  let count = ref(0)

  for dy in -1 to 1 {
    for dx in -1 to 1 {
      if !(dx == 0 && dy == 0) {
        let nx = x + dx
        let ny = y + dy

        if nx >= 0 && nx < state.width && ny >= 0 && ny < state.height {
          if Array.getUnsafe(Array.getUnsafe(state.grid, ny), nx) {
            count := count.contents + 1
          }
        }
      }
    }
  }

  count.contents
}

// Apply B3/S23 rule to determine next cell state
let applyRule = (isAlive: bool, neighbors: int, _rule: string): bool => {
  // B3/S23: Birth with 3 neighbors, Survive with 2-3 neighbors
  if isAlive {
    neighbors == 2 || neighbors == 3
  } else {
    neighbors == 3
  }
}

// Advance simulation by one generation
let step = (state: simulationState): simulationState => {
  // Create new grid
  let newGrid = Array.make(~length=state.height, Array.make(~length=state.width, false))

  // Apply rules to each cell
  for y in 0 to state.height - 1 {
    let newRow = Array.make(~length=state.width, false)
    for x in 0 to state.width - 1 {
      let isAlive = Array.getUnsafe(Array.getUnsafe(state.grid, y), x)
      let neighbors = countNeighbors(state, x, y)
      let nextState = applyRule(isAlive, neighbors, state.rule)
      Array.setUnsafe(newRow, x, nextState)
    }
    Array.setUnsafe(newGrid, y, newRow)
  }

  {
    ...state,
    grid: newGrid,
    generation: state.generation + 1,
  }
}

// Run simulation for N generations
let run = (state: simulationState, generations: int): simulationState => {
  let rec loop = (s, remaining) => {
    if remaining <= 0 {
      s
    } else {
      loop(step(s), remaining - 1)
    }
  }

  loop(state, generations)
}

// Count total live cells
let population = (state: simulationState): int => {
  let count = ref(0)

  for y in 0 to state.height - 1 {
    for x in 0 to state.width - 1 {
      if Array.getUnsafe(Array.getUnsafe(state.grid, y), x) {
        count := count.contents + 1
      }
    }
  }

  count.contents
}

// Check if pattern has stabilized (same as previous generation)
let isStable = (state1: simulationState, state2: simulationState): bool => {
  if state1.width != state2.width || state1.height != state2.height {
    false
  } else {
    let stable = ref(true)

    for y in 0 to state1.height - 1 {
      for x in 0 to state1.width - 1 {
        let cell1 = Array.getUnsafe(Array.getUnsafe(state1.grid, y), x)
        let cell2 = Array.getUnsafe(Array.getUnsafe(state2.grid, y), x)
        if cell1 != cell2 {
          stable := false
        }
      }
    }

    stable.contents
  }
}

// Read output region as bytes
let readOutputRegion = (
  state: simulationState,
  startX: int,
  startY: int,
  width: int,
  height: int,
): array<int> => {
  let bytes = []

  for row in 0 to height - 1 {
    let byteVal = ref(0)
    for bit in 0 to 7 {
      let x = startX + (row * 8 + bit) mod width
      let y = startY + (row * 8 + bit) / width

      if y < state.height && x < state.width {
        if Array.getUnsafe(Array.getUnsafe(state.grid, y), x) {
          byteVal := byteVal.contents lor (1 lsl (7 - bit))
        }
      }
    }
    ignore(Array.concat(bytes, [byteVal.contents]))
  }

  bytes
}

// Convert output bytes to string
let outputToString = (bytes: array<int>): string => {
  let chars = Array.map(bytes, b => String.make(1, Char.chr(b land 0xFF)))
  Array.join(chars, ~sep="")
}
