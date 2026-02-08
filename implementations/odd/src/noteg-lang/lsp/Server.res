// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * NoteG Language Server Protocol (LSP) Implementation
 * Provides IDE support for .noteg files
 */

open Lexer
open Parser

// LSP types
type position = {
  line: int,
  character: int,
}

type range = {
  start: position,
  @as("end") end_: position,
}

type diagnostic = {
  range: range,
  severity: int,
  message: string,
  source: string,
}

type completionItem = {
  label: string,
  kind: int,
  detail: option<string>,
  documentation: option<string>,
  insertText: option<string>,
}

type lspMessage = {
  jsonrpc: string,
  id: option<Js.Json.t>,
  method: option<string>,
  params: option<Js.Json.t>,
  result: option<Js.Json.t>,
  error: option<{"code": int, "message": string}>,
}

type textDocumentItem = {
  uri: string,
  languageId: string,
  version: int,
  text: string,
}

type documentState = {
  text: string,
  version: int,
  ast: option<programNode>,
}

// Document store
let documents: Js.Dict.t<documentState> = Js.Dict.empty()

let getKeywordDoc = (keyword: string): string => {
  switch keyword {
  | "let" => "**let**\n\nDeclare a mutable variable.\n\n```noteg\nlet x = 42\n```"
  | "const" => "**const**\n\nDeclare an immutable constant.\n\n```noteg\nconst PI = 3.14159\n```"
  | "fn" =>
    "**fn**\n\nDeclare a function.\n\n```noteg\nfn add(a, b) {\n  return a + b\n}\n```"
  | "if" =>
    "**if**\n\nConditional statement.\n\n```noteg\nif (condition) {\n  // then\n} else {\n  // else\n}\n```"
  | "for" => "**for**\n\nLoop over iterable.\n\n```noteg\nfor item in items {\n  print(item)\n}\n```"
  | "return" => "**return**\n\nReturn value from function."
  | "import" => "**import**\n\nImport from module.\n\n```noteg\nimport { foo } from \"module\"\n```"
  | "export" => "**export**\n\nExport declaration."
  | "template" => "**template**\n\nDefine a template block with {{ variable }} interpolation."
  | kw => `**${kw}**`
  }
}

let validateDocument = (uri: string): array<diagnostic> => {
  switch Js.Dict.get(documents, uri) {
  | None => []
  | Some(doc) =>
    let diagnostics = []

    // Lex
    let lexResult = tokenizeSource(doc.text)

    lexResult["errors"]->Belt.Array.forEach(error => {
      Js.Array2.push(
        diagnostics,
        {
          range: {
            start: {line: error.line - 1, character: error.column - 1},
            end_: {line: error.line - 1, character: error.column},
          },
          severity: 1,
          message: error.message,
          source: "noteg",
        },
      )->ignore
    })

    // Parse
    let parseResult = parseSource(lexResult["tokens"])

    parseResult["errors"]->Belt.Array.forEach(error => {
      Js.Array2.push(
        diagnostics,
        {
          range: {
            start: {line: error.token.line - 1, character: error.token.column - 1},
            end_: {line: error.token.line - 1, character: error.token.column + error.token.length},
          },
          severity: 1,
          message: error.message,
          source: "noteg",
        },
      )->ignore
    })

    // Store AST
    Js.Dict.set(
      documents,
      uri,
      {
        ...doc,
        ast: Some(parseResult["ast"]),
      },
    )

    diagnostics
  }
}

let handleCompletion = (_params: Js.Json.t): array<completionItem> => {
  let keywords = [
    {
      label: "let",
      kind: 14,
      detail: Some("Variable declaration"),
      documentation: None,
      insertText: Some("let ${1:name} = ${2:value}"),
    },
    {
      label: "const",
      kind: 14,
      detail: Some("Constant declaration"),
      documentation: None,
      insertText: Some("const ${1:name} = ${2:value}"),
    },
    {
      label: "fn",
      kind: 3,
      detail: Some("Function declaration"),
      documentation: None,
      insertText: Some("fn ${1:name}(${2:params}) {\n\t${3}\n}"),
    },
    {
      label: "if",
      kind: 14,
      detail: Some("Conditional"),
      documentation: None,
      insertText: Some("if (${1:condition}) {\n\t${2}\n}"),
    },
    {
      label: "for",
      kind: 14,
      detail: Some("Loop"),
      documentation: None,
      insertText: Some("for ${1:item} in ${2:items} {\n\t${3}\n}"),
    },
    {
      label: "return",
      kind: 14,
      detail: Some("Return statement"),
      documentation: None,
      insertText: Some("return ${1:value}"),
    },
    {
      label: "import",
      kind: 14,
      detail: Some("Import module"),
      documentation: None,
      insertText: Some("import { ${1:name} } from \"${2:module}\""),
    },
    {label: "export", kind: 14, detail: Some("Export declaration"), documentation: None, insertText: None},
    {label: "template", kind: 14, detail: Some("Template block"), documentation: None, insertText: None},
    {label: "true", kind: 21, detail: Some("Boolean true"), documentation: None, insertText: None},
    {label: "false", kind: 21, detail: Some("Boolean false"), documentation: None, insertText: None},
    {label: "null", kind: 21, detail: Some("Null value"), documentation: None, insertText: None},
  ]

  let builtins = [
    {
      label: "print",
      kind: 3,
      detail: Some("Print to console"),
      documentation: None,
      insertText: Some("print(${1:value})"),
    },
    {
      label: "len",
      kind: 3,
      detail: Some("Get length"),
      documentation: None,
      insertText: Some("len(${1:value})"),
    },
    {
      label: "map",
      kind: 3,
      detail: Some("Transform array"),
      documentation: None,
      insertText: Some("map(${1:fn})"),
    },
    {
      label: "filter",
      kind: 3,
      detail: Some("Filter array"),
      documentation: None,
      insertText: Some("filter(${1:fn})"),
    },
    {
      label: "reduce",
      kind: 3,
      detail: Some("Reduce array"),
      documentation: None,
      insertText: Some("reduce(${1:fn}, ${2:initial})"),
    },
  ]

  Belt.Array.concat(keywords, builtins)
}

let handleHover = (params: Js.Json.t): option<{"contents": string}> => {
  switch Js.Json.decodeObject(params) {
  | None => None
  | Some(paramsObj) =>
    let textDocumentOpt =
      Js.Dict.get(paramsObj, "textDocument")->Belt.Option.flatMap(Js.Json.decodeObject)
    let positionOpt = Js.Dict.get(paramsObj, "position")->Belt.Option.flatMap(Js.Json.decodeObject)

    switch (textDocumentOpt, positionOpt) {
    | (Some(textDoc), Some(pos)) =>
      let uriOpt = Js.Dict.get(textDoc, "uri")->Belt.Option.flatMap(Js.Json.decodeString)
      let lineOpt = Js.Dict.get(pos, "line")->Belt.Option.flatMap(Js.Json.decodeNumber)
      let charOpt = Js.Dict.get(pos, "character")->Belt.Option.flatMap(Js.Json.decodeNumber)

      switch (uriOpt, lineOpt, charOpt) {
      | (Some(uri), Some(line), Some(char)) =>
        switch Js.Dict.get(documents, uri) {
        | None => None
        | Some(doc) =>
          let lexResult = tokenizeSource(doc.text)
          let lineInt = Belt.Float.toInt(line) + 1
          let charInt = Belt.Float.toInt(char) + 1

          let found = lexResult["tokens"]->Belt.Array.getBy(token => {
            token.line == lineInt &&
            token.column <= charInt &&
            token.column + token.length > charInt
          })

          switch found {
          | Some(token) =>
            if token.type_ == Keyword {
              Some({"contents": getKeywordDoc(token.value)})
            } else if token.type_ == Identifier {
              Some({"contents": `**${token.value}**\n\nIdentifier`})
            } else {
              None
            }
          | None => None
          }
        }
      | _ => None
      }
    | _ => None
    }
  }
}

let handleInitialize = (id: Js.Json.t): lspMessage => {
  {
    jsonrpc: "2.0",
    id: Some(id),
    method: None,
    params: None,
    error: None,
    result: Some(
      Js.Json.object_(
        Js.Dict.fromArray([
          (
            "capabilities",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("textDocumentSync", Js.Json.number(1.0)),
                (
                  "completionProvider",
                  Js.Json.object_(
                    Js.Dict.fromArray([
                      (
                        "triggerCharacters",
                        Js.Json.array([
                          Js.Json.string("."),
                          Js.Json.string("{"),
                          Js.Json.string("|"),
                        ]),
                      ),
                      ("resolveProvider", Js.Json.boolean(false)),
                    ]),
                  ),
                ),
                ("hoverProvider", Js.Json.boolean(true)),
                ("definitionProvider", Js.Json.boolean(true)),
                ("documentFormattingProvider", Js.Json.boolean(true)),
              ]),
            ),
          ),
          (
            "serverInfo",
            Js.Json.object_(
              Js.Dict.fromArray([
                ("name", Js.Json.string("noteg-lsp")),
                ("version", Js.Json.string("0.1.0")),
              ]),
            ),
          ),
        ]),
      ),
    ),
  }
}

let handleDidOpen = (params: Js.Json.t): unit => {
  switch Js.Json.decodeObject(params) {
  | None => ()
  | Some(paramsObj) =>
    switch Js.Dict.get(paramsObj, "textDocument")->Belt.Option.flatMap(Js.Json.decodeObject) {
    | None => ()
    | Some(textDoc) =>
      let uri = Js.Dict.get(textDoc, "uri")->Belt.Option.flatMap(Js.Json.decodeString)
      let text = Js.Dict.get(textDoc, "text")->Belt.Option.flatMap(Js.Json.decodeString)
      let version = Js.Dict.get(textDoc, "version")->Belt.Option.flatMap(Js.Json.decodeNumber)

      switch (uri, text, version) {
      | (Some(u), Some(t), Some(v)) =>
        Js.Dict.set(
          documents,
          u,
          {
            text: t,
            version: Belt.Float.toInt(v),
            ast: None,
          },
        )
        let _ = validateDocument(u)
      | _ => ()
      }
    }
  }
}

let handleDidChange = (params: Js.Json.t): unit => {
  switch Js.Json.decodeObject(params) {
  | None => ()
  | Some(paramsObj) =>
    let textDocOpt =
      Js.Dict.get(paramsObj, "textDocument")->Belt.Option.flatMap(Js.Json.decodeObject)
    let changesOpt =
      Js.Dict.get(paramsObj, "contentChanges")->Belt.Option.flatMap(Js.Json.decodeArray)

    switch (textDocOpt, changesOpt) {
    | (Some(textDoc), Some(changes)) =>
      let uriOpt = Js.Dict.get(textDoc, "uri")->Belt.Option.flatMap(Js.Json.decodeString)
      let versionOpt = Js.Dict.get(textDoc, "version")->Belt.Option.flatMap(Js.Json.decodeNumber)

      switch (uriOpt, versionOpt, changes->Belt.Array.get(0)) {
      | (Some(uri), Some(version), Some(change)) =>
        switch Js.Json.decodeObject(change) {
        | Some(changeObj) =>
          switch Js.Dict.get(changeObj, "text")->Belt.Option.flatMap(Js.Json.decodeString) {
          | Some(text) =>
            Js.Dict.set(
              documents,
              uri,
              {
                text,
                version: Belt.Float.toInt(version),
                ast: None,
              },
            )
            let _ = validateDocument(uri)
          | None => ()
          }
        | None => ()
        }
      | _ => ()
      }
    | _ => ()
    }
  }
}

let handleDidClose = (params: Js.Json.t): unit => {
  switch Js.Json.decodeObject(params) {
  | None => ()
  | Some(paramsObj) =>
    switch Js.Dict.get(paramsObj, "textDocument")->Belt.Option.flatMap(Js.Json.decodeObject) {
    | None => ()
    | Some(textDoc) =>
      switch Js.Dict.get(textDoc, "uri")->Belt.Option.flatMap(Js.Json.decodeString) {
      | Some(uri) => Js.Dict.unsafeDeleteKey(. documents, uri)
      | None => ()
      }
    }
  }
}

let handleMessage = (message: lspMessage): option<lspMessage> => {
  switch message.method {
  | Some("initialize") =>
    switch message.id {
    | Some(id) => Some(handleInitialize(id))
    | None => None
    }
  | Some("initialized") => None
  | Some("shutdown") =>
    switch message.id {
    | Some(id) =>
      Some({
        jsonrpc: "2.0",
        id: Some(id),
        method: None,
        params: None,
        result: Some(Js.Json.null),
        error: None,
      })
    | None => None
    }
  | Some("textDocument/didOpen") =>
    switch message.params {
    | Some(params) => handleDidOpen(params)
    | None => ()
    }
    None
  | Some("textDocument/didChange") =>
    switch message.params {
    | Some(params) => handleDidChange(params)
    | None => ()
    }
    None
  | Some("textDocument/didClose") =>
    switch message.params {
    | Some(params) => handleDidClose(params)
    | None => ()
    }
    None
  | Some("textDocument/completion") =>
    switch message.id {
    | Some(id) =>
      let completions = switch message.params {
      | Some(params) => handleCompletion(params)
      | None => []
      }
      Some({
        jsonrpc: "2.0",
        id: Some(id),
        method: None,
        params: None,
        result: Some(
          Js.Json.array(
            completions->Belt.Array.map(c =>
              Js.Json.object_(
                Js.Dict.fromArray(
                  Belt.Array.concatMany([
                    [("label", Js.Json.string(c.label)), ("kind", Js.Json.number(Belt.Int.toFloat(c.kind)))],
                    switch c.detail {
                    | Some(d) => [("detail", Js.Json.string(d))]
                    | None => []
                    },
                    switch c.insertText {
                    | Some(t) => [("insertText", Js.Json.string(t))]
                    | None => []
                    },
                  ]),
                ),
              )
            ),
          ),
        ),
        error: None,
      })
    | None => None
    }
  | Some("textDocument/hover") =>
    switch (message.id, message.params) {
    | (Some(id), Some(params)) =>
      switch handleHover(params) {
      | Some(hover) =>
        Some({
          jsonrpc: "2.0",
          id: Some(id),
          method: None,
          params: None,
          result: Some(
            Js.Json.object_(Js.Dict.fromArray([("contents", Js.Json.string(hover["contents"]))])),
          ),
          error: None,
        })
      | None =>
        Some({
          jsonrpc: "2.0",
          id: Some(id),
          method: None,
          params: None,
          result: Some(Js.Json.null),
          error: None,
        })
      }
    | _ => None
    }
  | Some("textDocument/definition") =>
    switch message.id {
    | Some(id) =>
      Some({
        jsonrpc: "2.0",
        id: Some(id),
        method: None,
        params: None,
        result: Some(Js.Json.null),
        error: None,
      })
    | None => None
    }
  | _ => None
  }
}
