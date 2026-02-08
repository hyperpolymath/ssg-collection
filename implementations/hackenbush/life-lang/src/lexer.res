// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// hackenbush-ssg - RLE Pattern Lexer (ReScript)

// Token types for RLE format parsing
type token =
  | Header(string)        // #N, #C, #O, #R lines
  | Dimensions(int, int)  // x = W, y = H
  | Rule(string)          // rule = B3/S23
  | RunCount(int)         // Number prefix (e.g., "3" in "3o")
  | Alive                 // 'o' - alive cell
  | Dead                  // 'b' - dead cell
  | EndRow                // '$' - end of row
  | EndPattern            // '!' - end of pattern
  | Newline               // Line break in RLE data
  | Unknown(string)       // Unrecognized token

// Lexer state
type lexerState = {
  input: string,
  position: int,
  line: int,
  column: int,
}

// Create initial lexer state
let make = (input: string): lexerState => {
  input,
  position: 0,
  line: 1,
  column: 1,
}

// Check if at end of input
let isAtEnd = (state: lexerState): bool => {
  state.position >= String.length(state.input)
}

// Peek current character
let peek = (state: lexerState): option<string> => {
  if isAtEnd(state) {
    None
  } else {
    Some(String.sub(state.input, state.position, 1))
  }
}

// Advance lexer position
let advance = (state: lexerState): lexerState => {
  let char = peek(state)
  let (newLine, newColumn) = switch char {
  | Some("\n") => (state.line + 1, 1)
  | Some(_) => (state.line, state.column + 1)
  | None => (state.line, state.column)
  }

  {
    ...state,
    position: state.position + 1,
    line: newLine,
    column: newColumn,
  }
}

// Check if character is digit
let isDigit = (c: string): bool => {
  c >= "0" && c <= "9"
}

// Parse run count (sequence of digits)
let parseRunCount = (state: lexerState): (token, lexerState) => {
  let rec loop = (s, acc) => {
    switch peek(s) {
    | Some(c) if isDigit(c) => loop(advance(s), acc ++ c)
    | _ => (acc, s)
    }
  }

  let (digits, newState) = loop(state, "")
  let count = switch Int.fromString(digits) {
  | Some(n) => n
  | None => 1
  }

  (RunCount(count), newState)
}

// Parse header line (starts with #)
let parseHeader = (state: lexerState): (token, lexerState) => {
  let rec loop = (s, acc) => {
    switch peek(s) {
    | Some("\n") | None => (acc, s)
    | Some(c) => loop(advance(s), acc ++ c)
    }
  }

  let (content, newState) = loop(advance(state), "#")
  (Header(content), newState)
}

// Get next token
let nextToken = (state: lexerState): option<(token, lexerState)> => {
  if isAtEnd(state) {
    None
  } else {
    switch peek(state) {
    | Some("#") => Some(parseHeader(state))
    | Some("o") => Some((Alive, advance(state)))
    | Some("b") => Some((Dead, advance(state)))
    | Some("$") => Some((EndRow, advance(state)))
    | Some("!") => Some((EndPattern, advance(state)))
    | Some("\n") => Some((Newline, advance(state)))
    | Some(" ") | Some("\t") | Some("\r") => nextToken(advance(state))
    | Some(c) if isDigit(c) => Some(parseRunCount(state))
    | Some(c) => Some((Unknown(c), advance(state)))
    | None => None
    }
  }
}

// Tokenize entire input
let tokenize = (input: string): array<token> => {
  let state = make(input)
  let tokens = []

  let rec loop = (s, acc) => {
    switch nextToken(s) {
    | Some((token, newState)) => loop(newState, Array.concat(acc, [token]))
    | None => acc
    }
  }

  loop(state, tokens)
}
