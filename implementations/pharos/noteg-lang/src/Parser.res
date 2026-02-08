// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG Language - Parser

open Lexer

// AST Node Types
type rec expr =
  | Literal(literalValue)
  | Identifier(string)
  | Binary(expr, binaryOp, expr)
  | Unary(unaryOp, expr)
  | Call(expr, array<expr>)
  | Member(expr, string)
  | Index(expr, expr)
  | Array_(array<expr>)
  | Object(array<(string, expr)>)
  | Lambda(array<string>, expr)
  | Conditional(expr, expr, expr)
  | Template(array<templatePart>)
  | Pipe(expr, expr)

and literalValue =
  | StringLit(string)
  | NumberLit(float)
  | BoolLit(bool)
  | NullLit

and binaryOp =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | AndOp
  | OrOp

and unaryOp =
  | Neg
  | NotOp

and templatePart =
  | TextPart(string)
  | ExprPart(expr)

type rec stmt =
  | ExprStmt(expr)
  | LetStmt(string, option<typeExpr>, expr)
  | ConstStmt(string, option<typeExpr>, expr)
  | FnStmt(string, array<param>, option<typeExpr>, block)
  | IfStmt(expr, block, option<block>)
  | ForStmt(string, expr, block)
  | WhileStmt(expr, block)
  | ReturnStmt(option<expr>)
  | YieldStmt(expr)
  | ImportStmt(array<string>, string)
  | ExportStmt(stmt)
  | TypeStmt(string, typeExpr)
  | TemplateStmt(string, block)
  | ContentStmt(frontmatterExpr, block)
  | A11yStmt(a11yKind, expr)

and param = {
  name: string,
  type_: option<typeExpr>,
}

and block = array<stmt>

and typeExpr =
  | TypeName(string)
  | TypeArray(typeExpr)
  | TypeObject(array<(string, typeExpr)>)
  | TypeFunction(array<typeExpr>, typeExpr)
  | TypeUnion(array<typeExpr>)
  | TypeOptional(typeExpr)

and frontmatterExpr = {
  title: expr,
  meta: option<expr>,
}

and a11yKind =
  | BslA11y
  | GslA11y
  | AslA11y
  | MakatonA11y

type program = array<stmt>

type parseError = {
  message: string,
  token: token,
}

type parserState = {
  tokens: array<token>,
  mutable current: int,
  mutable errors: array<parseError>,
}

// Parser utilities
let createParser = (tokens: array<token>): parserState => {
  tokens,
  current: 0,
  errors: [],
}

let isAtEnd = (parser: parserState): bool => {
  switch parser.tokens[parser.current] {
  | Some({kind: Eof}) => true
  | None => true
  | _ => false
  }
}

let peek = (parser: parserState): option<token> => {
  parser.tokens[parser.current]
}

let previous = (parser: parserState): option<token> => {
  if parser.current > 0 {
    parser.tokens[parser.current - 1]
  } else {
    None
  }
}

let advance = (parser: parserState): option<token> => {
  if !isAtEnd(parser) {
    parser.current = parser.current + 1
  }
  previous(parser)
}

let check = (parser: parserState, kind: tokenKind): bool => {
  switch peek(parser) {
  | Some(token) => token.kind == kind
  | None => false
  }
}

let matchToken = (parser: parserState, kinds: array<tokenKind>): bool => {
  let found = kinds->Array.some(kind => check(parser, kind))
  if found {
    let _ = advance(parser)
  }
  found
}

let consume = (parser: parserState, kind: tokenKind, message: string): result<token, parseError> => {
  if check(parser, kind) {
    switch advance(parser) {
    | Some(t) => Ok(t)
    | None => Error({message, token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
    }
  } else {
    switch peek(parser) {
    | Some(t) => Error({message, token: t})
    | None => Error({message, token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
    }
  }
}

// Expression parsing (Pratt parser style)
let rec parseExpression = (parser: parserState): result<expr, parseError> => {
  parsePipe(parser)
}

and parsePipe = (parser: parserState): result<expr, parseError> => {
  switch parseOr(parser) {
  | Ok(left) => {
      if matchToken(parser, [Pipe]) {
        switch parsePipe(parser) {
        | Ok(right) => Ok(Pipe(left, right))
        | Error(e) => Error(e)
        }
      } else {
        Ok(left)
      }
    }
  | Error(e) => Error(e)
  }
}

and parseOr = (parser: parserState): result<expr, parseError> => {
  switch parseAnd(parser) {
  | Ok(left) => {
      let result = ref(left)
      while matchToken(parser, [Or]) {
        switch parseAnd(parser) {
        | Ok(right) => result := Binary(result.contents, OrOp, right)
        | Error(_) => ()
        }
      }
      Ok(result.contents)
    }
  | Error(e) => Error(e)
  }
}

and parseAnd = (parser: parserState): result<expr, parseError> => {
  switch parseEquality(parser) {
  | Ok(left) => {
      let result = ref(left)
      while matchToken(parser, [And]) {
        switch parseEquality(parser) {
        | Ok(right) => result := Binary(result.contents, AndOp, right)
        | Error(_) => ()
        }
      }
      Ok(result.contents)
    }
  | Error(e) => Error(e)
  }
}

and parseEquality = (parser: parserState): result<expr, parseError> => {
  switch parseComparison(parser) {
  | Ok(left) => {
      let result = ref(left)
      while matchToken(parser, [Equal, NotEqual]) {
        let op = switch previous(parser) {
        | Some({kind: Equal}) => Eq
        | _ => Neq
        }
        switch parseComparison(parser) {
        | Ok(right) => result := Binary(result.contents, op, right)
        | Error(_) => ()
        }
      }
      Ok(result.contents)
    }
  | Error(e) => Error(e)
  }
}

and parseComparison = (parser: parserState): result<expr, parseError> => {
  switch parseTerm(parser) {
  | Ok(left) => {
      let result = ref(left)
      while matchToken(parser, [Less, LessEqual, Greater, GreaterEqual]) {
        let op = switch previous(parser) {
        | Some({kind: Less}) => Lt
        | Some({kind: LessEqual}) => Lte
        | Some({kind: Greater}) => Gt
        | _ => Gte
        }
        switch parseTerm(parser) {
        | Ok(right) => result := Binary(result.contents, op, right)
        | Error(_) => ()
        }
      }
      Ok(result.contents)
    }
  | Error(e) => Error(e)
  }
}

and parseTerm = (parser: parserState): result<expr, parseError> => {
  switch parseFactor(parser) {
  | Ok(left) => {
      let result = ref(left)
      while matchToken(parser, [Plus, Minus]) {
        let op = switch previous(parser) {
        | Some({kind: Plus}) => Add
        | _ => Sub
        }
        switch parseFactor(parser) {
        | Ok(right) => result := Binary(result.contents, op, right)
        | Error(_) => ()
        }
      }
      Ok(result.contents)
    }
  | Error(e) => Error(e)
  }
}

and parseFactor = (parser: parserState): result<expr, parseError> => {
  switch parseUnary(parser) {
  | Ok(left) => {
      let result = ref(left)
      while matchToken(parser, [Star, Slash, Percent]) {
        let op = switch previous(parser) {
        | Some({kind: Star}) => Mul
        | Some({kind: Slash}) => Div
        | _ => Mod
        }
        switch parseUnary(parser) {
        | Ok(right) => result := Binary(result.contents, op, right)
        | Error(_) => ()
        }
      }
      Ok(result.contents)
    }
  | Error(e) => Error(e)
  }
}

and parseUnary = (parser: parserState): result<expr, parseError> => {
  if matchToken(parser, [Minus, Not]) {
    let op = switch previous(parser) {
    | Some({kind: Minus}) => Neg
    | _ => NotOp
    }
    switch parseUnary(parser) {
    | Ok(right) => Ok(Unary(op, right))
    | Error(e) => Error(e)
    }
  } else {
    parseCall(parser)
  }
}

and parseCall = (parser: parserState): result<expr, parseError> => {
  switch parsePrimary(parser) {
  | Ok(callee) => {
      let result = ref(callee)
      let continue = ref(true)
      while continue.contents {
        if matchToken(parser, [LeftParen]) {
          // Function call
          let args = ref([])
          if !check(parser, RightParen) {
            switch parseExpression(parser) {
            | Ok(arg) => args := args.contents->Array.concat([arg])
            | Error(_) => ()
            }
            while matchToken(parser, [Comma]) {
              switch parseExpression(parser) {
              | Ok(arg) => args := args.contents->Array.concat([arg])
              | Error(_) => ()
              }
            }
          }
          switch consume(parser, RightParen, "Expected ')' after arguments") {
          | Ok(_) => result := Call(result.contents, args.contents)
          | Error(_) => continue := false
          }
        } else if matchToken(parser, [Dot]) {
          // Member access
          switch peek(parser) {
          | Some({kind: Identifier(name)}) => {
              let _ = advance(parser)
              result := Member(result.contents, name)
            }
          | _ => continue := false
          }
        } else if matchToken(parser, [LeftBracket]) {
          // Index access
          switch parseExpression(parser) {
          | Ok(index) => {
              switch consume(parser, RightBracket, "Expected ']' after index") {
              | Ok(_) => result := Index(result.contents, index)
              | Error(_) => continue := false
              }
            }
          | Error(_) => continue := false
          }
        } else {
          continue := false
        }
      }
      Ok(result.contents)
    }
  | Error(e) => Error(e)
  }
}

and parsePrimary = (parser: parserState): result<expr, parseError> => {
  switch peek(parser) {
  | Some({kind: String(s)}) => {
      let _ = advance(parser)
      Ok(Literal(StringLit(s)))
    }
  | Some({kind: Number(n)}) => {
      let _ = advance(parser)
      Ok(Literal(NumberLit(n)))
    }
  | Some({kind: Boolean(b)}) => {
      let _ = advance(parser)
      Ok(Literal(BoolLit(b)))
    }
  | Some({kind: Null}) => {
      let _ = advance(parser)
      Ok(Literal(NullLit))
    }
  | Some({kind: Identifier(name)}) => {
      let _ = advance(parser)
      Ok(Identifier(name))
    }
  | Some({kind: LeftParen}) => {
      let _ = advance(parser)
      switch parseExpression(parser) {
      | Ok(expr) => {
          switch consume(parser, RightParen, "Expected ')' after expression") {
          | Ok(_) => Ok(expr)
          | Error(e) => Error(e)
          }
        }
      | Error(e) => Error(e)
      }
    }
  | Some({kind: LeftBracket}) => {
      // Array literal
      let _ = advance(parser)
      let elements = ref([])
      if !check(parser, RightBracket) {
        switch parseExpression(parser) {
        | Ok(e) => elements := elements.contents->Array.concat([e])
        | Error(_) => ()
        }
        while matchToken(parser, [Comma]) {
          switch parseExpression(parser) {
          | Ok(e) => elements := elements.contents->Array.concat([e])
          | Error(_) => ()
          }
        }
      }
      switch consume(parser, RightBracket, "Expected ']' after array") {
      | Ok(_) => Ok(Array_(elements.contents))
      | Error(e) => Error(e)
      }
    }
  | Some(token) => Error({message: `Unexpected token: ${token.lexeme}`, token})
  | None => Error({message: "Unexpected end of input", token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
  }
}

// Statement parsing
let rec parseStatement = (parser: parserState): result<stmt, parseError> => {
  switch peek(parser) {
  | Some({kind: Keyword("let")}) => parseLetStmt(parser)
  | Some({kind: Keyword("const")}) => parseConstStmt(parser)
  | Some({kind: Keyword("fn")}) => parseFnStmt(parser)
  | Some({kind: Keyword("if")}) => parseIfStmt(parser)
  | Some({kind: Keyword("for")}) => parseForStmt(parser)
  | Some({kind: Keyword("return")}) => parseReturnStmt(parser)
  | _ => parseExprStmt(parser)
  }
}

and parseLetStmt = (parser: parserState): result<stmt, parseError> => {
  let _ = advance(parser) // consume 'let'
  switch peek(parser) {
  | Some({kind: Identifier(name)}) => {
      let _ = advance(parser)
      switch consume(parser, Assign, "Expected '=' after variable name") {
      | Ok(_) => {
          switch parseExpression(parser) {
          | Ok(value) => Ok(LetStmt(name, None, value))
          | Error(e) => Error(e)
          }
        }
      | Error(e) => Error(e)
      }
    }
  | Some(token) => Error({message: "Expected variable name", token})
  | None => Error({message: "Expected variable name", token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
  }
}

and parseConstStmt = (parser: parserState): result<stmt, parseError> => {
  let _ = advance(parser) // consume 'const'
  switch peek(parser) {
  | Some({kind: Identifier(name)}) => {
      let _ = advance(parser)
      switch consume(parser, Assign, "Expected '=' after constant name") {
      | Ok(_) => {
          switch parseExpression(parser) {
          | Ok(value) => Ok(ConstStmt(name, None, value))
          | Error(e) => Error(e)
          }
        }
      | Error(e) => Error(e)
      }
    }
  | Some(token) => Error({message: "Expected constant name", token})
  | None => Error({message: "Expected constant name", token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
  }
}

and parseFnStmt = (parser: parserState): result<stmt, parseError> => {
  let _ = advance(parser) // consume 'fn'
  switch peek(parser) {
  | Some({kind: Identifier(name)}) => {
      let _ = advance(parser)
      switch consume(parser, LeftParen, "Expected '(' after function name") {
      | Ok(_) => {
          let params = ref([])
          // Parse parameters
          switch consume(parser, RightParen, "Expected ')' after parameters") {
          | Ok(_) => {
              switch consume(parser, LeftBrace, "Expected '{' before function body") {
              | Ok(_) => {
                  let body = ref([])
                  while !check(parser, RightBrace) && !isAtEnd(parser) {
                    switch parseStatement(parser) {
                    | Ok(stmt) => body := body.contents->Array.concat([stmt])
                    | Error(_) => ()
                    }
                  }
                  switch consume(parser, RightBrace, "Expected '}' after function body") {
                  | Ok(_) => Ok(FnStmt(name, params.contents, None, body.contents))
                  | Error(e) => Error(e)
                  }
                }
              | Error(e) => Error(e)
              }
            }
          | Error(e) => Error(e)
          }
        }
      | Error(e) => Error(e)
      }
    }
  | Some(token) => Error({message: "Expected function name", token})
  | None => Error({message: "Expected function name", token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
  }
}

and parseIfStmt = (parser: parserState): result<stmt, parseError> => {
  let _ = advance(parser) // consume 'if'
  switch parseExpression(parser) {
  | Ok(condition) => {
      switch consume(parser, LeftBrace, "Expected '{' after if condition") {
      | Ok(_) => {
          let thenBody = ref([])
          while !check(parser, RightBrace) && !isAtEnd(parser) {
            switch parseStatement(parser) {
            | Ok(stmt) => thenBody := thenBody.contents->Array.concat([stmt])
            | Error(_) => ()
            }
          }
          switch consume(parser, RightBrace, "Expected '}' after if body") {
          | Ok(_) => {
              if matchToken(parser, [Keyword("else")]) {
                switch consume(parser, LeftBrace, "Expected '{' after else") {
                | Ok(_) => {
                    let elseBody = ref([])
                    while !check(parser, RightBrace) && !isAtEnd(parser) {
                      switch parseStatement(parser) {
                      | Ok(stmt) => elseBody := elseBody.contents->Array.concat([stmt])
                      | Error(_) => ()
                      }
                    }
                    switch consume(parser, RightBrace, "Expected '}' after else body") {
                    | Ok(_) => Ok(IfStmt(condition, thenBody.contents, Some(elseBody.contents)))
                    | Error(e) => Error(e)
                    }
                  }
                | Error(e) => Error(e)
                }
              } else {
                Ok(IfStmt(condition, thenBody.contents, None))
              }
            }
          | Error(e) => Error(e)
          }
        }
      | Error(e) => Error(e)
      }
    }
  | Error(e) => Error(e)
  }
}

and parseForStmt = (parser: parserState): result<stmt, parseError> => {
  let _ = advance(parser) // consume 'for'
  switch peek(parser) {
  | Some({kind: Identifier(name)}) => {
      let _ = advance(parser)
      switch consume(parser, Keyword("in"), "Expected 'in' after variable") {
      | Ok(_) => {
          switch parseExpression(parser) {
          | Ok(iterable) => {
              switch consume(parser, LeftBrace, "Expected '{' after for clause") {
              | Ok(_) => {
                  let body = ref([])
                  while !check(parser, RightBrace) && !isAtEnd(parser) {
                    switch parseStatement(parser) {
                    | Ok(stmt) => body := body.contents->Array.concat([stmt])
                    | Error(_) => ()
                    }
                  }
                  switch consume(parser, RightBrace, "Expected '}' after for body") {
                  | Ok(_) => Ok(ForStmt(name, iterable, body.contents))
                  | Error(e) => Error(e)
                  }
                }
              | Error(e) => Error(e)
              }
            }
          | Error(e) => Error(e)
          }
        }
      | Error(e) => Error(e)
      }
    }
  | Some(token) => Error({message: "Expected variable name", token})
  | None => Error({message: "Expected variable name", token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
  }
}

and parseReturnStmt = (parser: parserState): result<stmt, parseError> => {
  let _ = advance(parser) // consume 'return'
  if check(parser, Newline) || check(parser, RightBrace) || isAtEnd(parser) {
    Ok(ReturnStmt(None))
  } else {
    switch parseExpression(parser) {
    | Ok(value) => Ok(ReturnStmt(Some(value)))
    | Error(e) => Error(e)
    }
  }
}

and parseExprStmt = (parser: parserState): result<stmt, parseError> => {
  // Skip newlines
  while matchToken(parser, [Newline]) {
    ()
  }
  if isAtEnd(parser) {
    Error({message: "Unexpected end of input", token: {kind: Eof, start: {line: 0, column: 0, offset: 0}, end_: {line: 0, column: 0, offset: 0}, lexeme: ""}})
  } else {
    switch parseExpression(parser) {
    | Ok(expr) => Ok(ExprStmt(expr))
    | Error(e) => Error(e)
    }
  }
}

// Main parse function
let parse = (tokens: array<token>): result<program, array<parseError>> => {
  let parser = createParser(tokens)
  let statements = ref([])

  while !isAtEnd(parser) {
    // Skip newlines
    while matchToken(parser, [Newline]) {
      ()
    }
    if !isAtEnd(parser) {
      switch parseStatement(parser) {
      | Ok(stmt) => statements := statements.contents->Array.concat([stmt])
      | Error(e) => {
          parser.errors = parser.errors->Array.concat([e])
          let _ = advance(parser) // Skip problematic token
        }
      }
    }
  }

  if parser.errors->Array.length > 0 {
    Error(parser.errors)
  } else {
    Ok(statements.contents)
  }
}
