// SPDX-License-Identifier: AGPL-3.0-or-later
// hackenbush-ssg - RLE Pattern Parser (ReScript)
//
// This module consumes tokens from the Lexer and constructs a logical 
// representation of a Life pattern, including its metadata and initial cell grid.

open Lexer

// SCHEMA: Metadata extracted from RLE headers.
type patternMetadata = {
  name: option<string>,   // From #N
  author: option<string>, // From #O
  comments: array<string>,// From #C
  rule: string,           // From 'rule = '
  width: int,             // From 'x = '
  height: int,            // From 'y = '
}

// SCHEMA: The resulting parsed pattern.
type pattern = {
  metadata: patternMetadata,
  cells: array<array<bool>>, // 2D grid of Booleans (true = alive)
}

// DATA LOADER: Consumes cell tokens and expands RunCounts.
// e.g., Token(RunCount(3)), Token(Alive) -> [true, true, true]
let parseCells = (state: parserState): array<array<bool>> => {
  let width = state.metadata.width
  let height = state.metadata.height

  // GRID INITIALIZATION
  let grid = Array.make(~length=height, Array.make(~length=width, false))

  let rec loop = (s, row, col) => {
    switch peek(s) {
    | Some(RunCount(n)) => {
        let nextState = advance(s)
        switch peek(nextState) {
        | Some(Alive) => {
            // EXPANSION: Fill 'n' consecutive cells as alive.
            for i in 0 to n - 1 {
              if row < height && col + i < width {
                Array.setUnsafe(Array.getUnsafe(grid, row), col + i, true)
              }
            }
            loop(advance(nextState), row, col + n)
          }
        | Some(Dead) => loop(advance(nextState), row, col + n)
        | _ => loop(nextState, row, col)
        }
      }
    | Some(Alive) => {
        if row < height && col < width {
          Array.setUnsafe(Array.getUnsafe(grid, row), col, true)
        }
        loop(advance(s), row, col + 1)
      }
    | Some(EndRow) => loop(advance(s), row + 1, 0)
    | Some(EndPattern) => grid
    | _ => grid
    }
  }

  loop(state, 0, 0)
}
