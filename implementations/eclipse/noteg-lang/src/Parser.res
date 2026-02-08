// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Parser.res — NoteG language parser

open Lexer

// ============================================================
// AST NODES
// ============================================================

type rec astNode =
  | Program(array<astNode>)
  | LetDecl({name: string, value: astNode})
  | FunctionDecl({name: string, params: array<string>, body: astNode})
  | IfStmt({condition: astNode, thenBranch: astNode, elseBranch: option<astNode>})
  | ForStmt({variable: string, iterable: astNode, body: astNode})
  | WhileStmt({condition: astNode, body: astNode})
  | ReturnStmt({value: option<astNode>})
  | BinaryExpr({operator: string, left: astNode, right: astNode})
  | UnaryExpr({operator: string, operand: astNode})
  | CallExpr({callee: astNode, args: array<astNode>})
  | IdentifierExpr({name: string})
  | LiteralExpr({value: literalValue})
  | ArrayExpr({elements: array<astNode>})
  | ObjectExpr({properties: array<(string, astNode)>})
  | TemplateExpr({variable: string})
  | Block({statements: array<astNode>})

and literalValue =
  | StringLit(string)
  | NumberLit(float)
  | BoolLit(bool)
  | NullLit

// ============================================================
// PARSER STATE
// ============================================================

type parserState = {
  tokens: array<token>,
  mutable pos: int,
}

exception ParseError(string)

// ============================================================
// HELPER FUNCTIONS
// ============================================================

let create = (tokens: array<token>): parserState => {
  tokens,
  pos: 0,
}

let peek = (state: parserState): token => {
  state.tokens[state.pos]
}

let previous = (state: parserState): token => {
  state.tokens[state.pos - 1]
}

let isAtEnd = (state: parserState): bool => {
  peek(state).tokenType == Eof
}

let advance = (state: parserState): token => {
  if !isAtEnd(state) {
    state.pos = state.pos + 1
  }
  previous(state)
}

let check = (state: parserState, tokenType: tokenType): bool => {
  !isAtEnd(state) && peek(state).tokenType == tokenType
}

let matchToken = (state: parserState, types: array<tokenType>): bool => {
  types->Array.some(t => {
    if check(state, t) {
      let _ = advance(state)
      true
    } else {
      false
    }
  })
}

let consume = (state: parserState, tokenType: tokenType, message: string): token => {
  if check(state, tokenType) {
    advance(state)
  } else {
    raise(ParseError(message ++ " at line " ++ Int.toString(peek(state).line)))
  }
}

// ============================================================
// EXPRESSION PARSERS
// ============================================================

let rec expression = (state: parserState): astNode => {
  orExpr(state)
}

and orExpr = (state: parserState): astNode => {
  let left = ref(andExpr(state))
  while matchToken(state, [Or]) {
    let right = andExpr(state)
    left := BinaryExpr({operator: "or", left: left.contents, right})
  }
  left.contents
}

and andExpr = (state: parserState): astNode => {
  let left = ref(equality(state))
  while matchToken(state, [And]) {
    let right = equality(state)
    left := BinaryExpr({operator: "and", left: left.contents, right})
  }
  left.contents
}

and equality = (state: parserState): astNode => {
  let left = ref(comparison(state))
  while matchToken(state, [Eq, Neq]) {
    let operator = previous(state).value
    let right = comparison(state)
    left := BinaryExpr({operator, left: left.contents, right})
  }
  left.contents
}

and comparison = (state: parserState): astNode => {
  let left = ref(term(state))
  while matchToken(state, [Lt, Gt, Lte, Gte]) {
    let operator = previous(state).value
    let right = term(state)
    left := BinaryExpr({operator, left: left.contents, right})
  }
  left.contents
}

and term = (state: parserState): astNode => {
  let left = ref(factor(state))
  while matchToken(state, [Plus, Minus]) {
    let operator = previous(state).value
    let right = factor(state)
    left := BinaryExpr({operator, left: left.contents, right})
  }
  left.contents
}

and factor = (state: parserState): astNode => {
  let left = ref(unary(state))
  while matchToken(state, [Star, Slash, Percent]) {
    let operator = previous(state).value
    let right = unary(state)
    left := BinaryExpr({operator, left: left.contents, right})
  }
  left.contents
}

and unary = (state: parserState): astNode => {
  if matchToken(state, [Not, Minus]) {
    let operator = previous(state).value
    let operand = unary(state)
    UnaryExpr({operator, operand})
  } else {
    call(state)
  }
}

and call = (state: parserState): astNode => {
  let expr = ref(primary(state))
  while true {
    if matchToken(state, [LParen]) {
      let args = ref([])
      if !check(state, RParen) {
        args := Array.concat(args.contents, [expression(state)])
        while matchToken(state, [Comma]) {
          args := Array.concat(args.contents, [expression(state)])
        }
      }
      let _ = consume(state, RParen, "Expected ')' after arguments")
      expr := CallExpr({callee: expr.contents, args: args.contents})
    } else {
      break
    }
  }
  expr.contents
}

and primary = (state: parserState): astNode => {
  if matchToken(state, [True]) {
    LiteralExpr({value: BoolLit(true)})
  } else if matchToken(state, [False]) {
    LiteralExpr({value: BoolLit(false)})
  } else if matchToken(state, [Number]) {
    LiteralExpr({value: NumberLit(Float.fromString(previous(state).value)->Option.getOr(0.0))})
  } else if matchToken(state, [String]) {
    LiteralExpr({value: StringLit(previous(state).value)})
  } else if matchToken(state, [Identifier]) {
    IdentifierExpr({name: previous(state).value})
  } else if matchToken(state, [TemplateStart]) {
    let variable = consume(state, Identifier, "Expected variable name").value
    let _ = consume(state, TemplateEnd, "Expected '}}'")
    TemplateExpr({variable: variable})
  } else if matchToken(state, [LBracket]) {
    let elements = ref([])
    if !check(state, RBracket) {
      elements := Array.concat(elements.contents, [expression(state)])
      while matchToken(state, [Comma]) {
        elements := Array.concat(elements.contents, [expression(state)])
      }
    }
    let _ = consume(state, RBracket, "Expected ']'")
    ArrayExpr({elements: elements.contents})
  } else if matchToken(state, [LParen]) {
    let expr = expression(state)
    let _ = consume(state, RParen, "Expected ')'")
    expr
  } else {
    raise(ParseError("Unexpected token: " ++ tokenTypeToString(peek(state).tokenType)))
  }
}

// ============================================================
// STATEMENT PARSERS
// ============================================================

let block = (state: parserState): astNode => {
  let _ = consume(state, LBrace, "Expected '{'")
  let statements = ref([])
  while !check(state, RBrace) && !isAtEnd(state) {
    statements := Array.concat(statements.contents, [declaration(state)])
  }
  let _ = consume(state, RBrace, "Expected '}'")
  Block({statements: statements.contents})
}

and statement = (state: parserState): astNode => {
  if check(state, If) {
    ifStatement(state)
  } else if check(state, For) {
    forStatement(state)
  } else if check(state, While) {
    whileStatement(state)
  } else if check(state, Return) {
    returnStatement(state)
  } else if check(state, LBrace) {
    block(state)
  } else {
    expression(state)
  }
}

and ifStatement = (state: parserState): astNode => {
  let _ = advance(state) // consume 'if'
  let condition = expression(state)
  let thenBranch = block(state)
  let elseBranch = if matchToken(state, [Else]) {
    if check(state, If) {
      Some(ifStatement(state))
    } else {
      Some(block(state))
    }
  } else {
    None
  }
  IfStmt({condition, thenBranch, elseBranch})
}

and forStatement = (state: parserState): astNode => {
  let _ = advance(state) // consume 'for'
  let variable = consume(state, Identifier, "Expected variable name").value
  let _ = consume(state, Identifier, "Expected 'in'") // 'in' keyword
  let iterable = expression(state)
  let body = block(state)
  ForStmt({variable, iterable, body})
}

and whileStatement = (state: parserState): astNode => {
  let _ = advance(state) // consume 'while'
  let condition = expression(state)
  let body = block(state)
  WhileStmt({condition, body})
}

and returnStatement = (state: parserState): astNode => {
  let _ = advance(state) // consume 'return'
  let value = if !check(state, RBrace) && !isAtEnd(state) {
    Some(expression(state))
  } else {
    None
  }
  ReturnStmt({value: value})
}

and letDeclaration = (state: parserState): astNode => {
  let _ = advance(state) // consume 'let'
  let name = consume(state, Identifier, "Expected variable name").value
  let _ = consume(state, Assign, "Expected '=' after variable name")
  let value = expression(state)
  LetDecl({name, value})
}

and functionDeclaration = (state: parserState): astNode => {
  let _ = advance(state) // consume 'fn'
  let name = consume(state, Identifier, "Expected function name").value
  let _ = consume(state, LParen, "Expected '(' after function name")
  let params = ref([])
  if !check(state, RParen) {
    params := Array.concat(params.contents, [consume(state, Identifier, "Expected parameter name").value])
    while matchToken(state, [Comma]) {
      params := Array.concat(params.contents, [consume(state, Identifier, "Expected parameter name").value])
    }
  }
  let _ = consume(state, RParen, "Expected ')' after parameters")
  let body = block(state)
  FunctionDecl({name, params: params.contents, body})
}

and declaration = (state: parserState): astNode => {
  if check(state, Let) {
    letDeclaration(state)
  } else if check(state, Fn) {
    functionDeclaration(state)
  } else {
    statement(state)
  }
}

// ============================================================
// MAIN PARSER
// ============================================================

let parse = (tokens: array<token>): astNode => {
  let state = create(tokens)
  let body = ref([])
  while !isAtEnd(state) {
    body := Array.concat(body.contents, [declaration(state)])
  }
  Program(body.contents)
}

let parseSource = (source: string): astNode => {
  let tokens = tokenize(source)
  parse(tokens)
}
