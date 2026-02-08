// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG Language Server Protocol Implementation

open Lexer
open Parser

// LSP Message Types
type position = {
  line: int,
  character: int,
}

type range = {
  start: position,
  end_: position,
}

type location = {
  uri: string,
  range: range,
}

type diagnostic = {
  range: range,
  severity: int, // 1=Error, 2=Warning, 3=Info, 4=Hint
  message: string,
  source: string,
}

type completionItem = {
  label: string,
  kind: int, // 1=Text, 2=Method, 3=Function, 6=Variable, etc.
  detail: option<string>,
  documentation: option<string>,
  insertText: option<string>,
}

type hoverResult = {
  contents: string,
  range: option<range>,
}

type documentSymbol = {
  name: string,
  kind: int, // 12=Function, 13=Variable, etc.
  range: range,
  selectionRange: range,
  children: option<array<documentSymbol>>,
}

// Server state
type documentState = {
  uri: string,
  content: string,
  version: int,
  tokens: array<token>,
  ast: option<program>,
  diagnostics: array<diagnostic>,
}

type serverState = {
  mutable documents: Js.Dict.t<documentState>,
  mutable initialized: bool,
}

let createServer = (): serverState => {
  documents: Js.Dict.empty(),
  initialized: false,
}

// Document management
let openDocument = (server: serverState, uri: string, content: string, version: int): unit => {
  let tokens = tokenize(content)
  let ast = switch parse(tokens) {
  | Ok(p) => Some(p)
  | Error(_) => None
  }

  let diagnostics = validateDocument(content, tokens, ast)

  Js.Dict.set(server.documents, uri, {
    uri,
    content,
    version,
    tokens,
    ast,
    diagnostics,
  })
}

and validateDocument = (content: string, tokens: array<token>, ast: option<program>): array<diagnostic> => {
  let diagnostics = ref([])

  // Check for lexer errors
  tokens->Array.forEach(token => {
    switch token.kind {
    | Error(msg) =>
      diagnostics :=
        diagnostics.contents->Array.concat([
          {
            range: {
              start: {line: token.start.line - 1, character: token.start.column - 1},
              end_: {line: token.end_.line - 1, character: token.end_.column - 1},
            },
            severity: 1,
            message: msg,
            source: "noteg",
          },
        ])
    | _ => ()
    }
  })

  // If no AST, add parse error
  if ast == None && Array.length(diagnostics.contents) == 0 {
    diagnostics :=
      diagnostics.contents->Array.concat([
        {
          range: {start: {line: 0, character: 0}, end_: {line: 0, character: 0}},
          severity: 1,
          message: "Failed to parse document",
          source: "noteg",
        },
      ])
  }

  diagnostics.contents
}

let updateDocument = (server: serverState, uri: string, content: string, version: int): unit => {
  openDocument(server, uri, content, version)
}

let closeDocument = (server: serverState, uri: string): unit => {
  let _ = Js.Dict.unsafeDeleteKey(. Obj.magic(server.documents), uri)
}

// Completion
let getCompletions = (server: serverState, uri: string, position: position): array<completionItem> => {
  let keywords = [
    ("let", 14, "Declare a variable"),
    ("const", 14, "Declare a constant"),
    ("fn", 3, "Define a function"),
    ("if", 14, "Conditional statement"),
    ("else", 14, "Alternative branch"),
    ("for", 14, "For loop"),
    ("in", 14, "Iteration keyword"),
    ("while", 14, "While loop"),
    ("return", 14, "Return from function"),
    ("import", 14, "Import module"),
    ("export", 14, "Export declaration"),
    ("template", 14, "Define a template"),
    ("content", 14, "Content block"),
    ("a11y", 14, "Accessibility metadata"),
    ("bsl", 14, "British Sign Language"),
    ("gsl", 14, "German Sign Language"),
    ("asl", 14, "American Sign Language"),
    ("makaton", 14, "Makaton symbols"),
  ]

  let builtins = [
    ("print", 3, "Print values to console"),
    ("len", 3, "Get length of array or string"),
    ("type", 3, "Get type of value"),
  ]

  let items = ref([])

  keywords->Array.forEach(((label, kind, doc)) => {
    items :=
      items.contents->Array.concat([
        {
          label,
          kind,
          detail: Some("keyword"),
          documentation: Some(doc),
          insertText: Some(label),
        },
      ])
  })

  builtins->Array.forEach(((label, kind, doc)) => {
    items :=
      items.contents->Array.concat([
        {
          label,
          kind,
          detail: Some("builtin"),
          documentation: Some(doc),
          insertText: Some(label),
        },
      ])
  })

  // Add symbols from current document
  switch Js.Dict.get(server.documents, uri) {
  | Some(doc) =>
    switch doc.ast {
    | Some(ast) => {
        ast->Array.forEach(stmt => {
          switch stmt {
          | LetStmt(name, _, _) =>
            items :=
              items.contents->Array.concat([
                {
                  label: name,
                  kind: 6,
                  detail: Some("variable"),
                  documentation: None,
                  insertText: Some(name),
                },
              ])
          | ConstStmt(name, _, _) =>
            items :=
              items.contents->Array.concat([
                {
                  label: name,
                  kind: 21,
                  detail: Some("constant"),
                  documentation: None,
                  insertText: Some(name),
                },
              ])
          | FnStmt(name, _, _, _) =>
            items :=
              items.contents->Array.concat([
                {
                  label: name,
                  kind: 3,
                  detail: Some("function"),
                  documentation: None,
                  insertText: Some(name ++ "()"),
                },
              ])
          | _ => ()
          }
        })
      }
    | None => ()
    }
  | None => ()
  }

  items.contents
}

// Hover
let getHover = (server: serverState, uri: string, position: position): option<hoverResult> => {
  switch Js.Dict.get(server.documents, uri) {
  | Some(doc) => {
      // Find token at position
      let targetToken = ref(None)
      doc.tokens->Array.forEach(token => {
        if token.start.line - 1 == position.line &&
           token.start.column - 1 <= position.character &&
           token.end_.column - 1 >= position.character {
          targetToken := Some(token)
        }
      })

      switch targetToken.contents {
      | Some(token) => {
          let info = switch token.kind {
          | Keyword(kw) => Some(`**Keyword**: \`${kw}\``)
          | Identifier(id) => Some(`**Identifier**: \`${id}\``)
          | String(_) => Some("**String literal**")
          | Number(n) => Some(`**Number**: ${Js.Float.toString(n)}`)
          | Boolean(b) => Some(`**Boolean**: ${b ? "true" : "false"}`)
          | _ => None
          }

          switch info {
          | Some(contents) =>
            Some({
              contents,
              range: Some({
                start: {line: token.start.line - 1, character: token.start.column - 1},
                end_: {line: token.end_.line - 1, character: token.end_.column - 1},
              }),
            })
          | None => None
          }
        }
      | None => None
      }
    }
  | None => None
  }
}

// Document symbols
let getDocumentSymbols = (server: serverState, uri: string): array<documentSymbol> => {
  switch Js.Dict.get(server.documents, uri) {
  | Some(doc) =>
    switch doc.ast {
    | Some(ast) => {
        let symbols = ref([])

        ast->Array.forEach(stmt => {
          switch stmt {
          | FnStmt(name, _, _, _) =>
            symbols :=
              symbols.contents->Array.concat([
                {
                  name,
                  kind: 12,
                  range: {start: {line: 0, character: 0}, end_: {line: 0, character: 0}},
                  selectionRange: {start: {line: 0, character: 0}, end_: {line: 0, character: 0}},
                  children: None,
                },
              ])
          | LetStmt(name, _, _) | ConstStmt(name, _, _) =>
            symbols :=
              symbols.contents->Array.concat([
                {
                  name,
                  kind: 13,
                  range: {start: {line: 0, character: 0}, end_: {line: 0, character: 0}},
                  selectionRange: {start: {line: 0, character: 0}, end_: {line: 0, character: 0}},
                  children: None,
                },
              ])
          | _ => ()
          }
        })

        symbols.contents
      }
    | None => []
    }
  | None => []
  }
}

// Formatting
let formatDocument = (server: serverState, uri: string): option<string> => {
  switch Js.Dict.get(server.documents, uri) {
  | Some(doc) => {
      // Simple formatting: fix indentation
      let lines = doc.content->Js.String2.split("\n")
      let indent = ref(0)
      let formatted =
        lines
        ->Array.map(line => {
          let trimmed = Js.String2.trim(line)
          if trimmed == "" {
            ""
          } else {
            // Decrease indent for closing braces
            if Js.String2.startsWith(trimmed, "}") {
              indent := max(0, indent.contents - 1)
            }
            let result = Js.String2.repeat("  ", indent.contents) ++ trimmed
            // Increase indent after opening braces
            if Js.String2.endsWith(trimmed, "{") {
              indent := indent.contents + 1
            }
            result
          }
        })
        ->Array.joinWith("\n")
      Some(formatted)
    }
  | None => None
  }
}

// Diagnostics
let getDiagnostics = (server: serverState, uri: string): array<diagnostic> => {
  switch Js.Dict.get(server.documents, uri) {
  | Some(doc) => doc.diagnostics
  | None => []
  }
}
