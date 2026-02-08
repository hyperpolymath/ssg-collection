// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * NoteG Language Parser
 * Builds AST from token stream
 */

open Lexer

// AST Node Types
type rec programNode = {body: array<statementNode>}

and statementNode =
  | VariableDeclaration({kind: string, name: string, init: option<expressionNode>})
  | FunctionDeclaration({name: string, params: array<string>, body: array<statementNode>})
  | ExpressionStatement({expression: expressionNode})
  | ReturnStatement({argument: option<expressionNode>})
  | IfStatement({
      test: expressionNode,
      consequent: array<statementNode>,
      alternate: option<array<statementNode>>,
    })
  | ForStatement({variable: string, iterable: expressionNode, body: array<statementNode>})
  | ImportStatement({specifiers: array<string>, source: string})
  | ExportStatement({declaration: statementNode})
  | TemplateStatement({parts: array<templatePart>})

and templatePart = TextPart(string) | ExprPart(expressionNode)

and expressionNode =
  | Identifier({name: string})
  | Literal({value: Js.Json.t, raw: string})
  | BinaryExpression({operator: string, left: expressionNode, right: expressionNode})
  | UnaryExpression({operator: string, argument: expressionNode})
  | CallExpression({callee: expressionNode, arguments: array<expressionNode>})
  | MemberExpression({object: expressionNode, property: expressionNode, computed: bool})
  | ArrayExpression({elements: array<expressionNode>})
  | ObjectExpression({properties: array<{key: string, value: expressionNode}>})
  | PipeExpression({left: expressionNode, right: expressionNode})
  | TemplateExpression({expression: expressionNode})

type parseError = {
  message: string,
  token: token,
}

type parser = {
  tokens: array<token>,
  mutable pos: int,
  mutable errors: array<parseError>,
}

let createParser = (tokens: array<token>): parser => {
  // Filter out comments and newlines for simpler parsing
  let filteredTokens = tokens->Belt.Array.keep(t => t.type_ != Comment && t.type_ != Newline)
  {
    tokens: filteredTokens,
    pos: 0,
    errors: [],
  }
}

let isAtEnd = (parser: parser): bool => {
  switch parser.tokens->Belt.Array.get(parser.pos) {
  | Some(t) => t.type_ == EOF
  | None => true
  }
}

let peek = (parser: parser): token => {
  parser.tokens->Belt.Array.getExn(parser.pos)
}

let previous = (parser: parser): token => {
  parser.tokens->Belt.Array.getExn(parser.pos - 1)
}

let advance = (parser: parser): token => {
  if !isAtEnd(parser) {
    parser.pos = parser.pos + 1
  }
  previous(parser)
}

let check = (parser: parser, type_: tokenType): bool => {
  if isAtEnd(parser) {
    false
  } else {
    peek(parser).type_ == type_
  }
}

let match_ = (parser: parser, types: array<tokenType>): bool => {
  types->Belt.Array.some(t => {
    if check(parser, t) {
      let _ = advance(parser)
      true
    } else {
      false
    }
  })
}

let addError = (parser: parser, token: token, message: string): parseError => {
  let error = {message, token}
  Js.Array2.push(parser.errors, error)->ignore
  error
}

let expect = (parser: parser, type_: tokenType, message: string): token => {
  if check(parser, type_) {
    advance(parser)
  } else {
    raise(Js.Exn.raiseError(message))
  }
}

let synchronize = (parser: parser): unit => {
  let _ = advance(parser)
  while !isAtEnd(parser) {
    if previous(parser).type_ == Semicolon {
      ()
    } else if check(parser, Keyword) {
      let kw = peek(parser).value
      if (
        kw == "let" ||
        kw == "const" ||
        kw == "fn" ||
        kw == "if" ||
        kw == "for" ||
        kw == "return" ||
        kw == "import" ||
        kw == "export"
      ) {
        ()
      } else {
        let _ = advance(parser)
      }
    } else {
      let _ = advance(parser)
    }
  }
}

// Forward declarations for recursive parsing
let rec parseExpression = (parser: parser): expressionNode => parsePipe(parser)

and parsePipe = (parser: parser): expressionNode => {
  let left = ref(parseOr(parser))

  while match_(parser, [Pipe]) {
    let right = parseOr(parser)
    left := PipeExpression({left: left.contents, right})
  }

  left.contents
}

and parseOr = (parser: parser): expressionNode => {
  let left = ref(parseAnd(parser))

  while match_(parser, [Or]) {
    let right = parseAnd(parser)
    left := BinaryExpression({operator: "||", left: left.contents, right})
  }

  left.contents
}

and parseAnd = (parser: parser): expressionNode => {
  let left = ref(parseEquality(parser))

  while match_(parser, [And]) {
    let right = parseEquality(parser)
    left := BinaryExpression({operator: "&&", left: left.contents, right})
  }

  left.contents
}

and parseEquality = (parser: parser): expressionNode => {
  let left = ref(parseTerm(parser))

  while match_(parser, [Equals, NotEquals]) {
    let operator = previous(parser).value
    let right = parseTerm(parser)
    left := BinaryExpression({operator, left: left.contents, right})
  }

  left.contents
}

and parseTerm = (parser: parser): expressionNode => {
  let left = ref(parseFactor(parser))

  while match_(parser, [Plus, Minus]) {
    let operator = previous(parser).value
    let right = parseFactor(parser)
    left := BinaryExpression({operator, left: left.contents, right})
  }

  left.contents
}

and parseFactor = (parser: parser): expressionNode => {
  let left = ref(parseUnary(parser))

  while match_(parser, [Multiply, Divide, Modulo]) {
    let operator = previous(parser).value
    let right = parseUnary(parser)
    left := BinaryExpression({operator, left: left.contents, right})
  }

  left.contents
}

and parseUnary = (parser: parser): expressionNode => {
  if match_(parser, [Not, Minus]) {
    let operator = previous(parser).value
    let argument = parseUnary(parser)
    UnaryExpression({operator, argument})
  } else {
    parseCall(parser)
  }
}

and parseCall = (parser: parser): expressionNode => {
  let expr = ref(parsePrimary(parser))

  let continue = ref(true)
  while continue.contents {
    if match_(parser, [LParen]) {
      let args = []
      if !check(parser, RParen) {
        let _ = Js.Array2.push(args, parseExpression(parser))
        while match_(parser, [Comma]) {
          let _ = Js.Array2.push(args, parseExpression(parser))
        }
      }
      let _ = expect(parser, RParen, "Expected ')' after arguments")
      expr := CallExpression({callee: expr.contents, arguments: args})
    } else if match_(parser, [Dot]) {
      let propToken = expect(parser, Identifier, "Expected property name")
      expr :=
        MemberExpression({
          object: expr.contents,
          property: Identifier({name: propToken.value}),
          computed: false,
        })
    } else if match_(parser, [LBracket]) {
      let property = parseExpression(parser)
      let _ = expect(parser, RBracket, "Expected ']'")
      expr := MemberExpression({object: expr.contents, property, computed: true})
    } else {
      continue := false
    }
  }

  expr.contents
}

and parsePrimary = (parser: parser): expressionNode => {
  if match_(parser, [Number]) {
    let value = previous(parser).value
    Literal({
      value: Js.Json.number(Belt.Float.fromString(value)->Belt.Option.getWithDefault(0.0)),
      raw: value,
    })
  } else if match_(parser, [String]) {
    let value = previous(parser).value
    Literal({value: Js.Json.string(value), raw: `"${value}"`})
  } else if match_(parser, [Boolean]) {
    let value = previous(parser).value
    Literal({value: Js.Json.boolean(value == "true"), raw: value})
  } else if match_(parser, [Null]) {
    Literal({value: Js.Json.null, raw: "null"})
  } else if match_(parser, [Identifier]) {
    Identifier({name: previous(parser).value})
  } else if match_(parser, [LBracket]) {
    let elements = []
    if !check(parser, RBracket) {
      let _ = Js.Array2.push(elements, parseExpression(parser))
      while match_(parser, [Comma]) {
        let _ = Js.Array2.push(elements, parseExpression(parser))
      }
    }
    let _ = expect(parser, RBracket, "Expected ']'")
    ArrayExpression({elements: elements})
  } else if match_(parser, [LBrace]) {
    let properties = []
    if !check(parser, RBrace) {
      let key = expect(parser, Identifier, "Expected property key").value
      let _ = expect(parser, Colon, "Expected ':' after property key")
      let value = parseExpression(parser)
      let _ = Js.Array2.push(properties, {key, value})
      while match_(parser, [Comma]) {
        let key = expect(parser, Identifier, "Expected property key").value
        let _ = expect(parser, Colon, "Expected ':' after property key")
        let value = parseExpression(parser)
        let _ = Js.Array2.push(properties, {key, value})
      }
    }
    let _ = expect(parser, RBrace, "Expected '}'")
    ObjectExpression({properties: properties})
  } else if match_(parser, [LParen]) {
    let expr = parseExpression(parser)
    let _ = expect(parser, RParen, "Expected ')'")
    expr
  } else {
    let _ = addError(parser, peek(parser), "Expected expression")
    raise(Js.Exn.raiseError("Expected expression"))
  }
}

let parseVariableDeclaration = (parser: parser): statementNode => {
  let kind = advance(parser).value
  let name = expect(parser, Identifier, "Expected variable name").value

  let init = if match_(parser, [Assign]) {
    Some(parseExpression(parser))
  } else {
    None
  }

  let _ = match_(parser, [Semicolon])

  VariableDeclaration({kind, name, init})
}

let rec parseFunctionDeclaration = (parser: parser): statementNode => {
  let _ = advance(parser) // fn
  let name = expect(parser, Identifier, "Expected function name").value

  let _ = expect(parser, LParen, "Expected '(' after function name")
  let params = []

  if !check(parser, RParen) {
    let _ = Js.Array2.push(params, expect(parser, Identifier, "Expected parameter name").value)
    while match_(parser, [Comma]) {
      let _ = Js.Array2.push(params, expect(parser, Identifier, "Expected parameter name").value)
    }
  }

  let _ = expect(parser, RParen, "Expected ')' after parameters")
  let _ = expect(parser, LBrace, "Expected '{' before function body")

  let body = []
  while !check(parser, RBrace) && !isAtEnd(parser) {
    switch parseStatement(parser) {
    | Some(stmt) => Js.Array2.push(body, stmt)->ignore
    | None => ()
    }
  }

  let _ = expect(parser, RBrace, "Expected '}' after function body")

  FunctionDeclaration({name, params: params, body: body})
}

and parseReturnStatement = (parser: parser): statementNode => {
  let _ = advance(parser) // return

  let argument = if !check(parser, Semicolon) && !check(parser, RBrace) {
    Some(parseExpression(parser))
  } else {
    None
  }

  let _ = match_(parser, [Semicolon])

  ReturnStatement({argument: argument})
}

and parseIfStatement = (parser: parser): statementNode => {
  let _ = advance(parser) // if
  let _ = expect(parser, LParen, "Expected '(' after 'if'")
  let test = parseExpression(parser)
  let _ = expect(parser, RParen, "Expected ')' after condition")
  let _ = expect(parser, LBrace, "Expected '{' before if body")

  let consequent = []
  while !check(parser, RBrace) && !isAtEnd(parser) {
    switch parseStatement(parser) {
    | Some(stmt) => Js.Array2.push(consequent, stmt)->ignore
    | None => ()
    }
  }
  let _ = expect(parser, RBrace, "Expected '}' after if body")

  let alternate = if check(parser, Keyword) && peek(parser).value == "else" {
    let _ = advance(parser)
    let _ = expect(parser, LBrace, "Expected '{' before else body")
    let alt = []
    while !check(parser, RBrace) && !isAtEnd(parser) {
      switch parseStatement(parser) {
      | Some(stmt) => Js.Array2.push(alt, stmt)->ignore
      | None => ()
      }
    }
    let _ = expect(parser, RBrace, "Expected '}' after else body")
    Some(alt)
  } else {
    None
  }

  IfStatement({test, consequent: consequent, alternate})
}

and parseForStatement = (parser: parser): statementNode => {
  let _ = advance(parser) // for
  let variable = expect(parser, Identifier, "Expected loop variable").value
  let _ = expect(parser, Keyword, "Expected 'in'")
  let iterable = parseExpression(parser)
  let _ = expect(parser, LBrace, "Expected '{' before for body")

  let body = []
  while !check(parser, RBrace) && !isAtEnd(parser) {
    switch parseStatement(parser) {
    | Some(stmt) => Js.Array2.push(body, stmt)->ignore
    | None => ()
    }
  }
  let _ = expect(parser, RBrace, "Expected '}' after for body")

  ForStatement({variable, iterable, body: body})
}

and parseImportStatement = (parser: parser): statementNode => {
  let _ = advance(parser) // import
  let _ = expect(parser, LBrace, "Expected '{' after import")

  let specifiers = []
  if !check(parser, RBrace) {
    let _ = Js.Array2.push(specifiers, expect(parser, Identifier, "Expected import name").value)
    while match_(parser, [Comma]) {
      let _ = Js.Array2.push(specifiers, expect(parser, Identifier, "Expected import name").value)
    }
  }

  let _ = expect(parser, RBrace, "Expected '}' after imports")
  let _ = expect(parser, Keyword, "Expected 'from'")
  let source = expect(parser, String, "Expected module path").value
  let _ = match_(parser, [Semicolon])

  ImportStatement({specifiers: specifiers, source})
}

and parseExportStatement = (parser: parser): statementNode => {
  let _ = advance(parser) // export
  switch parseStatement(parser) {
  | Some(declaration) => ExportStatement({declaration: declaration})
  | None => raise(Js.Exn.raiseError("Expected declaration after export"))
  }
}

and parseTemplateStatement = (parser: parser): statementNode => {
  let _ = advance(parser) // template
  let parts = []

  let continue = ref(true)
  while !isAtEnd(parser) && continue.contents {
    if match_(parser, [TemplateStart]) {
      Js.Array2.push(parts, ExprPart(parseExpression(parser)))->ignore
      let _ = expect(parser, TemplateEnd, "Expected '}}'")
    } else if check(parser, TemplateText) {
      Js.Array2.push(parts, TextPart(advance(parser).value))->ignore
    } else {
      continue := false
    }
  }

  TemplateStatement({parts: parts})
}

and parseExpressionStatement = (parser: parser): statementNode => {
  let expression = parseExpression(parser)
  let _ = match_(parser, [Semicolon])
  ExpressionStatement({expression: expression})
}

and parseStatement = (parser: parser): option<statementNode> => {
  let token = peek(parser)

  if token.type_ == Keyword {
    switch token.value {
    | "let" | "const" => Some(parseVariableDeclaration(parser))
    | "fn" => Some(parseFunctionDeclaration(parser))
    | "return" => Some(parseReturnStatement(parser))
    | "if" => Some(parseIfStatement(parser))
    | "for" => Some(parseForStatement(parser))
    | "import" => Some(parseImportStatement(parser))
    | "export" => Some(parseExportStatement(parser))
    | "template" => Some(parseTemplateStatement(parser))
    | _ => Some(parseExpressionStatement(parser))
    }
  } else {
    Some(parseExpressionStatement(parser))
  }
}

let parse = (parser: parser): {"ast": programNode, "errors": array<parseError>} => {
  let body = []

  while !isAtEnd(parser) {
    try {
      switch parseStatement(parser) {
      | Some(stmt) => Js.Array2.push(body, stmt)->ignore
      | None => ()
      }
    } catch {
    | _ => synchronize(parser)
    }
  }

  {"ast": {body: body}, "errors": parser.errors}
}

let parseSource = (tokens: array<token>): {"ast": programNode, "errors": array<parseError>} => {
  let parser = createParser(tokens)
  parse(parser)
}
