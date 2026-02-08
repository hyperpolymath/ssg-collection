// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// hackenbush-ssg - RLE Language Server (ReScript)
// Provides IDE support for RLE pattern files

// LSP Message types
type position = {
  line: int,
  character: int,
}

type range = {
  start: position,
  end_: position,
}

type diagnostic = {
  range: range,
  severity: int,  // 1=error, 2=warning, 3=info, 4=hint
  message: string,
  source: string,
}

type completionItem = {
  label: string,
  kind: int,  // 1=text, 2=method, 6=variable, etc.
  detail: option<string>,
  documentation: option<string>,
}

type hoverResult = {
  contents: string,
  range: option<range>,
}

// LSP Server state
type serverState = {
  documents: Js.Dict.t<string>,
  initialized: bool,
}

// Create server state
let createServer = (): serverState => {
  {
    documents: Js.Dict.empty(),
    initialized: false,
  }
}

// Validate RLE document
let validateDocument = (content: string): array<diagnostic> => {
  let diagnostics = []
  let lines = Js.String2.split(content, "\n")

  Array.forEachWithIndex(lines, (lineNum, line) => {
    // Check for valid header format
    if Js.String2.startsWith(line, "#") {
      let validHeaders = ["#N", "#C", "#O", "#R", "#r"]
      let hasValidHeader = Array.some(validHeaders, h =>
        Js.String2.startsWith(line, h)
      )
      if !hasValidHeader && String.length(line) > 1 {
        ignore(Array.concat(diagnostics, [{
          range: {
            start: {line: lineNum, character: 0},
            end_: {line: lineNum, character: 2},
          },
          severity: 2,  // Warning
          message: "Unknown header type. Valid headers: #N (name), #C (comment), #O (origin), #R (rule)",
          source: "hackenbush-lsp",
        }]))
      }
    }

    // Check for dimension line
    if Js.String2.includes(line, "x =") && !Js.String2.includes(line, "y =") {
      ignore(Array.concat(diagnostics, [{
        range: {
          start: {line: lineNum, character: 0},
          end_: {line: lineNum, character: String.length(line)},
        },
        severity: 1,  // Error
        message: "Dimension line must include both x and y values",
        source: "hackenbush-lsp",
      }]))
    }

    // Check for valid cell characters in pattern data
    if !Js.String2.startsWith(line, "#") && !Js.String2.includes(line, "=") {
      let invalidChars = Js.String2.replaceByRe(line, %re("/[0-9bo$!\s]/g"), "")
      if String.length(invalidChars) > 0 {
        ignore(Array.concat(diagnostics, [{
          range: {
            start: {line: lineNum, character: 0},
            end_: {line: lineNum, character: String.length(line)},
          },
          severity: 1,
          message: `Invalid characters in pattern data: ${invalidChars}`,
          source: "hackenbush-lsp",
        }]))
      }
    }
  })

  diagnostics
}

// Provide completions
let getCompletions = (_content: string, _position: position): array<completionItem> => {
  [
    {
      label: "#N",
      kind: 14,  // Keyword
      detail: Some("Pattern name"),
      documentation: Some("Specify the name of the pattern"),
    },
    {
      label: "#C",
      kind: 14,
      detail: Some("Comment"),
      documentation: Some("Add a comment line"),
    },
    {
      label: "#O",
      kind: 14,
      detail: Some("Origin/Author"),
      documentation: Some("Specify the pattern author or origin"),
    },
    {
      label: "x = , y = , rule = B3/S23",
      kind: 15,  // Snippet
      detail: Some("Dimension line"),
      documentation: Some("Specify pattern dimensions and rule"),
    },
    {
      label: "o",
      kind: 6,  // Variable
      detail: Some("Alive cell"),
      documentation: Some("Represents a live cell in the pattern"),
    },
    {
      label: "b",
      kind: 6,
      detail: Some("Dead cell"),
      documentation: Some("Represents a dead cell in the pattern"),
    },
    {
      label: "$",
      kind: 6,
      detail: Some("End of row"),
      documentation: Some("Marks the end of a row in the pattern"),
    },
    {
      label: "!",
      kind: 6,
      detail: Some("End of pattern"),
      documentation: Some("Marks the end of the pattern data"),
    },
  ]
}

// Provide hover information
let getHover = (content: string, position: position): option<hoverResult> => {
  let lines = Js.String2.split(content, "\n")
  let line = Array.get(lines, position.line)

  switch line {
  | Some(lineContent) => {
      if Js.String2.startsWith(lineContent, "#N") {
        Some({
          contents: "**Pattern Name**\n\nSpecifies the name of this Life pattern.",
          range: None,
        })
      } else if Js.String2.startsWith(lineContent, "#C") {
        Some({
          contents: "**Comment**\n\nA comment describing the pattern.",
          range: None,
        })
      } else if Js.String2.includes(lineContent, "rule = B3/S23") {
        Some({
          contents: "**Conway's Game of Life Rule**\n\n- B3: A dead cell with exactly 3 neighbors becomes alive\n- S23: A live cell with 2 or 3 neighbors survives",
          range: None,
        })
      } else if Js.String2.includes(lineContent, "x =") {
        Some({
          contents: "**Pattern Dimensions**\n\nSpecifies the bounding box of the pattern.\n\n- x: width in cells\n- y: height in cells",
          range: None,
        })
      } else {
        None
      }
    }
  | None => None
  }
}

// Format document
let formatDocument = (content: string): string => {
  let lines = Js.String2.split(content, "\n")

  let formatted = Array.map(lines, line => {
    // Trim whitespace
    let trimmed = Js.String2.trim(line)

    // Normalize header spacing
    if Js.String2.startsWith(trimmed, "#") {
      let parts = Js.String2.split(trimmed, " ")
      if Array.length(parts) > 1 {
        let header = Array.getUnsafe(parts, 0)
        let rest = Array.sliceToEnd(parts, ~start=1)
        header ++ " " ++ Array.join(rest, ~sep=" ")
      } else {
        trimmed
      }
    } else {
      trimmed
    }
  })

  Array.join(formatted, ~sep="\n")
}
