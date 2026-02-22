// SPDX-License-Identifier: AGPL-3.0-or-later
// hackenbush-ssg - RLE Pattern Lexer (ReScript)
//
// This module tokenizes RLE (Run Length Encoded) pattern files used by the 
// Life-based SSG engine. It handles headers, dimensions, and the run-count 
// encoded cell data.

// TOKEN DEFINITIONS: Standard elements of an RLE file.
type token =
  | Header(string)        // Lines starting with # (Metadata/Comments)
  | Dimensions(int, int)  // The 'x = W, y = H' header line
  | Rule(string)          // The 'rule = ...' definition
  | RunCount(int)         // Digit sequence before a cell type (e.g., '3' in '3o')
  | Alive                 // The character 'o' (Life)
  | Dead                  // The character 'b' (Death)
  | EndRow                // The character '$'
  | EndPattern            // The character '!'
  | Newline               // Physical line breaks
  | Unknown(string)       // Fallback for unrecognized characters

// SCANNER STATE: Tracks physical position and coordinates.
type lexerState = {
  input: string,
  position: int,
  line: int,
  column: int,
}

// FACTORY: Initializes a new scanner for a given input string.
let make = (input: string): lexerState => {
  input,
  position: 0,
  line: 1,
  column: 1,
}

// LOOKAHEAD: Peeks at the current character without advancing.
let peek = (state: lexerState): option<string> => {
  if state.position >= String.length(state.input) {
    None
  } else {
    Some(String.sub(state.input, state.position, 1))
  }
}

// RUN-COUNT PARSER: Consumes a sequence of digits and returns a `RunCount` token.
let parseRunCount = (state: lexerState): (token, lexerState) => {
  let rec loop = (s, acc) => {
    switch peek(s) {
    | Some(c) if c >= "0" && c <= "9" => loop(advance(s), acc ++ c)
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

// DISPATCH: The main state transition function for the lexer.
let nextToken = (state: lexerState): option<(token, lexerState)> => {
  if state.position >= String.length(state.input) {
    None
  } else {
    switch peek(state) {
    | Some("#") => Some(parseHeader(state))
    | Some("o") => Some((Alive, advance(state)))
    | Some("b") => Some((Dead, advance(state)))
    | Some("$") => Some((EndRow, advance(state)))
    | Some("!") => Some((EndPattern, advance(state)))
    | Some(c) if c >= "0" && c <= "9" => Some(parseRunCount(state))
    | Some(c) => Some((Unknown(c), advance(state)))
    | None => None
    }
  }
}
