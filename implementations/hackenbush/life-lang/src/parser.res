// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// hackenbush-ssg - RLE Pattern Parser (ReScript)

// Import lexer types
open Lexer

// Pattern metadata
type patternMetadata = {
  name: option<string>,
  author: option<string>,
  comments: array<string>,
  rule: string,
  width: int,
  height: int,
}

// Parsed pattern
type pattern = {
  metadata: patternMetadata,
  cells: array<array<bool>>,
}

// Parser state
type parserState = {
  tokens: array<token>,
  position: int,
  metadata: patternMetadata,
}

// Default metadata
let defaultMetadata: patternMetadata = {
  name: None,
  author: None,
  comments: [],
  rule: "B3/S23",
  width: 0,
  height: 0,
}

// Create parser state
let make = (tokens: array<token>): parserState => {
  tokens,
  position: 0,
  metadata: defaultMetadata,
}

// Check if at end
let isAtEnd = (state: parserState): bool => {
  state.position >= Array.length(state.tokens)
}

// Peek current token
let peek = (state: parserState): option<token> => {
  if isAtEnd(state) {
    None
  } else {
    Some(Array.getUnsafe(state.tokens, state.position))
  }
}

// Advance parser
let advance = (state: parserState): parserState => {
  {...state, position: state.position + 1}
}

// Parse header information
let parseHeaders = (state: parserState): parserState => {
  let rec loop = (s) => {
    switch peek(s) {
    | Some(Header(content)) => {
        let newMeta = if String.startsWith(content, ~prefix="#N ") {
          {...s.metadata, name: Some(String.sliceToEnd(content, ~start=3))}
        } else if String.startsWith(content, ~prefix="#O ") {
          {...s.metadata, author: Some(String.sliceToEnd(content, ~start=3))}
        } else if String.startsWith(content, ~prefix="#C ") {
          let comment = String.sliceToEnd(content, ~start=3)
          {...s.metadata, comments: Array.concat(s.metadata.comments, [comment])}
        } else {
          s.metadata
        }
        loop({...advance(s), metadata: newMeta})
      }
    | Some(Dimensions(w, h)) => {
        let newMeta = {...s.metadata, width: w, height: h}
        loop({...advance(s), metadata: newMeta})
      }
    | Some(Rule(r)) => {
        let newMeta = {...s.metadata, rule: r}
        loop({...advance(s), metadata: newMeta})
      }
    | Some(Newline) => loop(advance(s))
    | _ => s
    }
  }

  loop(state)
}

// Parse cell data
let parseCells = (state: parserState): array<array<bool>> => {
  let width = state.metadata.width
  let height = state.metadata.height

  // Initialize grid
  let grid = Array.make(~length=height, Array.make(~length=width, false))

  let rec loop = (s, row, col) => {
    switch peek(s) {
    | Some(RunCount(n)) => {
        // Look ahead for cell type
        let nextState = advance(s)
        switch peek(nextState) {
        | Some(Alive) => {
            // Fill n cells with alive
            for i in 0 to n - 1 {
              if row < height && col + i < width {
                Array.setUnsafe(Array.getUnsafe(grid, row), col + i, true)
              }
            }
            loop(advance(nextState), row, col + n)
          }
        | Some(Dead) => {
            // Skip n dead cells (already false)
            loop(advance(nextState), row, col + n)
          }
        | _ => loop(nextState, row, col)
        }
      }
    | Some(Alive) => {
        if row < height && col < width {
          Array.setUnsafe(Array.getUnsafe(grid, row), col, true)
        }
        loop(advance(s), row, col + 1)
      }
    | Some(Dead) => {
        loop(advance(s), row, col + 1)
      }
    | Some(EndRow) => {
        loop(advance(s), row + 1, 0)
      }
    | Some(EndPattern) => grid
    | Some(Newline) => loop(advance(s), row, col)
    | Some(_) => loop(advance(s), row, col)
    | None => grid
    }
  }

  loop(state, 0, 0)
}

// Parse complete pattern
let parse = (tokens: array<token>): result<pattern, string> => {
  let state = make(tokens)
  let stateWithHeaders = parseHeaders(state)
  let cells = parseCells(stateWithHeaders)

  Ok({
    metadata: stateWithHeaders.metadata,
    cells,
  })
}

// Parse from RLE string
let parseRLE = (input: string): result<pattern, string> => {
  let tokens = Lexer.tokenize(input)
  parse(tokens)
}
