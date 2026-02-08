// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// hackenbush-ssg Host Runtime
//
// This host provides ONLY:
//   - Load and parse RLE pattern files
//   - Run the Game of Life simulation
//   - Read output cells at designated locations
//   - Write resulting bytes to files
//
// The host does NOT contain any SSG logic.
// ALL SSG logic is encoded in the Life pattern.

// Constants for the simulation
let outputRegionX = 1000
let outputRegionY = 0
let outputWidth = 128
let outputHeight = 32
let defaultGenerations = 10000

// Grid represented as a Set of "x,y" strings
module Grid = Belt.Set.String

// Make a cell key from coordinates
let cellKey = (x: int, y: int): string => {
  Belt.Int.toString(x) ++ "," ++ Belt.Int.toString(y)
}

// Parse cell key back to coordinates
let parseKey = (key: string): (int, int) => {
  switch Js.String2.split(key, ",") {
  | [xStr, yStr] => (
      Belt.Int.fromString(xStr)->Belt.Option.getWithDefault(0),
      Belt.Int.fromString(yStr)->Belt.Option.getWithDefault(0),
    )
  | _ => (0, 0)
  }
}

// Parse RLE (Run Length Encoded) pattern format
type rleResult = {
  width: int,
  height: int,
  cells: Grid.t,
}

let parseRLE = (content: string): rleResult => {
  let lines = Js.String2.split(content, "\n")
  let cells = ref(Grid.empty)
  let width = ref(0)
  let height = ref(0)
  let x = ref(0)
  let y = ref(0)
  let patternStarted = ref(false)

  lines->Belt.Array.forEach(line => {
    // Skip comments
    if !(Js.String2.startsWith(line, "#")) {
      // Parse header
      if Js.String2.startsWith(line, "x") {
        let xMatch = Js.String2.match_(line, %re("/x\s*=\s*(\d+)/"))
        let yMatch = Js.String2.match_(line, %re("/y\s*=\s*(\d+)/"))

        switch xMatch {
        | Some(arr) =>
          switch arr[1] {
          | Some(v) => width := Belt.Int.fromString(v)->Belt.Option.getWithDefault(0)
          | None => ()
          }
        | None => ()
        }

        switch yMatch {
        | Some(arr) =>
          switch arr[1] {
          | Some(v) => height := Belt.Int.fromString(v)->Belt.Option.getWithDefault(0)
          | None => ()
          }
        | None => ()
        }

        patternStarted := true
      } else if patternStarted.contents {
        // Parse pattern data
        let count = ref(0)

        for i in 0 to Js.String2.length(line) - 1 {
          let char = Js.String2.charAt(line, i)

          if char >= "0" && char <= "9" {
            count :=
              count.contents * 10 + Belt.Int.fromString(char)->Belt.Option.getWithDefault(0)
          } else if char == "b" {
            // Dead cells
            x := x.contents + (count.contents > 0 ? count.contents : 1)
            count := 0
          } else if char == "o" {
            // Live cells
            let n = count.contents > 0 ? count.contents : 1
            for _ in 0 to n - 1 {
              cells := Grid.add(cells.contents, cellKey(x.contents, y.contents))
              x := x.contents + 1
            }
            count := 0
          } else if char == "$" {
            // End of row
            y := y.contents + (count.contents > 0 ? count.contents : 1)
            x := 0
            count := 0
          } else if char == "!" {
            // End of pattern - stop processing
            ()
          }
        }
      }
    }
  })

  {width: width.contents, height: height.contents, cells: cells.contents}
}

// Check if a cell is alive
let isAlive = (grid: Grid.t, x: int, y: int): bool => {
  Grid.has(grid, cellKey(x, y))
}

// Count live neighbors
let countNeighbors = (grid: Grid.t, x: int, y: int): int => {
  let count = ref(0)

  for dy in -1 to 1 {
    for dx in -1 to 1 {
      if !(dx == 0 && dy == 0) {
        if isAlive(grid, x + dx, y + dy) {
          count := count.contents + 1
        }
      }
    }
  }

  count.contents
}

// Run one generation
let evolve = (grid: Grid.t): Grid.t => {
  let candidates = ref(Grid.empty)

  // Collect all cells to check (alive cells + their neighbors)
  Grid.forEach(grid, cell => {
    let (x, y) = parseKey(cell)
    for dy in -1 to 1 {
      for dx in -1 to 1 {
        candidates := Grid.add(candidates.contents, cellKey(x + dx, y + dy))
      }
    }
  })

  // Apply B3/S23 rules
  let newGrid = ref(Grid.empty)

  Grid.forEach(candidates.contents, cell => {
    let (x, y) = parseKey(cell)
    let alive = isAlive(grid, x, y)
    let neighbors = countNeighbors(grid, x, y)

    if alive && (neighbors == 2 || neighbors == 3) {
      newGrid := Grid.add(newGrid.contents, cell)
    } else if !alive && neighbors == 3 {
      newGrid := Grid.add(newGrid.contents, cell)
    }
  })

  newGrid.contents
}

// Read output region as bytes
let readOutput = (grid: Grid.t): Js.TypedArray2.Uint8Array.t => {
  let bytes = []

  for row in 0 to outputHeight - 1 {
    let byte = ref(0)
    for bit in 0 to 7 {
      let x = outputRegionX + row * 8 + bit
      let y = outputRegionY
      if isAlive(grid, x, y) {
        byte := lor(byte.contents, lsl(1, 7 - bit))
      }
    }
    if byte.contents > 0 {
      let _ = Js.Array2.push(bytes, byte.contents)
    }
  }

  Js.TypedArray2.Uint8Array.make(bytes)
}

// Main entry point
let main = async () => {
  let patternFile = switch Deno.args[0] {
  | Some(f) => f
  | None => "src/hackenbush.rle"
  }

  let generations = switch Deno.args[1] {
  | Some(g) => Belt.Int.fromString(g)->Belt.Option.getWithDefault(defaultGenerations)
  | None => defaultGenerations
  }

  let outputFile = switch Deno.args[2] {
  | Some(o) => o
  | None => "_site/index.html"
  }

  Deno.log("[LIFE HOST] hackenbush-ssg runtime")
  Deno.log("[LIFE HOST] Pattern: " ++ patternFile)
  Deno.log("[LIFE HOST] Generations: " ++ Belt.Int.toString(generations))

  // Load pattern
  let content = await Deno.readTextFile(patternFile)
  let {cells} = parseRLE(content)
  let grid = ref(cells)

  Deno.log("[LIFE HOST] Initial population: " ++ Belt.Int.toString(Grid.size(grid.contents)))

  // Run simulation
  for gen in 0 to generations - 1 {
    grid := evolve(grid.contents)

    if mod(gen, 1000) == 0 {
      Deno.log(
        "[LIFE HOST] Generation " ++
        Belt.Int.toString(gen) ++
        ", population: " ++
        Belt.Int.toString(Grid.size(grid.contents)),
      )
    }
  }

  Deno.log("[LIFE HOST] Final population: " ++ Belt.Int.toString(Grid.size(grid.contents)))

  // Read output
  let output = readOutput(grid.contents)

  // Ensure output directory exists
  try {
    await Deno.mkdir("_site", {recursive: true})
  } catch {
  | _ => ()
  }

  // Write output
  await Deno.writeFile(outputFile, output)

  Deno.log("[LIFE HOST] Output written to: " ++ outputFile)
  Deno.log(
    "[LIFE HOST] Output size: " ++ Belt.Int.toString(Js.TypedArray2.Uint8Array.length(output)) ++ " bytes",
  )

  // Preview output
  let decoder = %raw(`new TextDecoder()`)
  let text: string = %raw(`decoder.decode(output)`)
  if Js.String2.length(text) > 0 {
    Deno.log("[LIFE HOST] Output preview:")
    Deno.log(Js.String2.slice(text, ~from=0, ~to_=200))
  } else {
    Deno.log("[LIFE HOST] No output generated (pattern needs more generations or different setup)")
  }
}

// Run main
let _ = main()
