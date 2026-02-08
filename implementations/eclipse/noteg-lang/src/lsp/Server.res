// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Server.res — NoteG Language Server Protocol implementation

open Lexer
open Parser

// ============================================================
// LSP TYPES
// ============================================================

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
  message: string,
  severity: int, // 1: Error, 2: Warning, 3: Information, 4: Hint
  source: string,
}

type completionItem = {
  label: string,
  kind: int,
  detail: option<string>,
  documentation: option<string>,
}

type textDocumentItem = {
  uri: string,
  languageId: string,
  version: int,
  text: string,
}

// ============================================================
// DOCUMENT STORE
// ============================================================

type documentStore = {
  documents: Js.Dict.t<textDocumentItem>,
}

let createStore = (): documentStore => {
  documents: Js.Dict.empty(),
}

let openDocument = (store: documentStore, doc: textDocumentItem): unit => {
  Js.Dict.set(store.documents, doc.uri, doc)
}

let updateDocument = (store: documentStore, uri: string, text: string, version: int): unit => {
  switch Js.Dict.get(store.documents, uri) {
  | Some(doc) =>
    Js.Dict.set(store.documents, uri, {...doc, text, version})
  | None => ()
  }
}

let closeDocument = (store: documentStore, uri: string): unit => {
  // Remove from store - using JS interop
  %raw(`delete store.documents[uri]`)
}

let getDocument = (store: documentStore, uri: string): option<textDocumentItem> => {
  Js.Dict.get(store.documents, uri)
}

// ============================================================
// LANGUAGE SERVER
// ============================================================

type languageServer = {
  documents: documentStore,
}

let createServer = (): languageServer => {
  documents: createStore(),
}

let analyze = (server: languageServer, uri: string): array<diagnostic> => {
  switch getDocument(server.documents, uri) {
  | None => []
  | Some(doc) =>
    let diagnostics = ref([])
    try {
      let tokens = tokenize(doc.text)

      // Check for lexer errors
      tokens->Array.forEach(token => {
        if token.tokenType == Error {
          diagnostics := Array.concat(diagnostics.contents, [{
            range: {
              start: {line: token.line - 1, character: token.column - 1},
              end_: {line: token.line - 1, character: token.column + Js.String2.length(token.value) - 1},
            },
            message: "Unexpected character: " ++ token.value,
            severity: 1,
            source: "noteg",
          }])
        }
      })

      // Try to parse
      let _ = parse(tokens)
      diagnostics.contents
    } catch {
    | ParseError(msg) =>
      // Extract line number from message
      let lineMatch = Js.String2.match_(msg, %re("/at line (\d+)/"))
      let line = switch lineMatch {
      | Some(matches) =>
        switch matches[1] {
        | Some(lineStr) => Int.fromString(lineStr)->Option.getOr(1) - 1
        | None => 0
        }
      | None => 0
      }

      Array.concat(diagnostics.contents, [{
        range: {
          start: {line, character: 0},
          end_: {line, character: 100},
        },
        message: msg,
        severity: 1,
        source: "noteg",
      }])
    | _ => diagnostics.contents
    }
  }
}

let getCompletions = (_server: languageServer, _uri: string, _position: position): array<completionItem> => {
  // Keywords
  let keywords = [
    {label: "let", kind: 14, detail: Some("Variable declaration"), documentation: None},
    {label: "fn", kind: 14, detail: Some("Function definition"), documentation: None},
    {label: "if", kind: 14, detail: Some("Conditional statement"), documentation: None},
    {label: "else", kind: 14, detail: Some("Else branch"), documentation: None},
    {label: "for", kind: 14, detail: Some("For loop"), documentation: None},
    {label: "while", kind: 14, detail: Some("While loop"), documentation: None},
    {label: "return", kind: 14, detail: Some("Return statement"), documentation: None},
    {label: "true", kind: 14, detail: Some("Boolean true"), documentation: None},
    {label: "false", kind: 14, detail: Some("Boolean false"), documentation: None},
    {label: "and", kind: 14, detail: Some("Logical AND"), documentation: None},
    {label: "or", kind: 14, detail: Some("Logical OR"), documentation: None},
    {label: "not", kind: 14, detail: Some("Logical NOT"), documentation: None},
  ]

  // Built-in functions
  let builtins = [
    {label: "print", kind: 3, detail: Some("Print to console"), documentation: Some("print(value) - Outputs value to console")},
    {label: "len", kind: 3, detail: Some("Get length"), documentation: Some("len(array|string) - Returns the length")},
  ]

  Array.concat(keywords, builtins)
}

let getWordAtPosition = (line: string, character: int): string => {
  let startPos = ref(character)
  let endPos = ref(character)

  // Find start of word
  while startPos.contents > 0 {
    let char = Js.String2.charAt(line, startPos.contents - 1)
    if Js.Re.test_(%re("/\w/"), char) {
      startPos := startPos.contents - 1
    } else {
      break
    }
  }

  // Find end of word
  while endPos.contents < Js.String2.length(line) {
    let char = Js.String2.charAt(line, endPos.contents)
    if Js.Re.test_(%re("/\w/"), char) {
      endPos := endPos.contents + 1
    } else {
      break
    }
  }

  Js.String2.substring(line, ~from=startPos.contents, ~to_=endPos.contents)
}

let getHover = (server: languageServer, uri: string, position: position): option<string> => {
  switch getDocument(server.documents, uri) {
  | None => None
  | Some(doc) =>
    let lines = Js.String2.split(doc.text, "\n")
    switch lines[position.line] {
    | None => None
    | Some(line) =>
      let word = getWordAtPosition(line, position.character)

      // Documentation for keywords and built-ins
      let docs = Js.Dict.fromArray([
        ("let", "Variable declaration: `let name = value`"),
        ("fn", "Function definition: `fn name(params) { body }`"),
        ("if", "Conditional: `if condition { then } else { else }`"),
        ("for", "For loop: `for item in array { body }`"),
        ("while", "While loop: `while condition { body }`"),
        ("return", "Return from function: `return value`"),
        ("print", "Built-in function: `print(value)` - Output to console"),
        ("len", "Built-in function: `len(array|string)` - Get length"),
      ])

      Js.Dict.get(docs, word)
    }
  }
}

// ============================================================
// SERVER CAPABILITIES
// ============================================================

type serverCapabilities = {
  textDocumentSync: int,
  completionProvider: {triggerCharacters: array<string>},
  hoverProvider: bool,
  diagnosticProvider: {
    interFileDependencies: bool,
    workspaceDiagnostics: bool,
  },
}

type serverInfo = {
  name: string,
  version: string,
}

let getCapabilities = (): serverCapabilities => {
  textDocumentSync: 1, // Full sync
  completionProvider: {triggerCharacters: ["."]},
  hoverProvider: true,
  diagnosticProvider: {
    interFileDependencies: false,
    workspaceDiagnostics: false,
  },
}

let getServerInfo = (): serverInfo => {
  name: "NoteG Language Server",
  version: "0.2.0",
}
