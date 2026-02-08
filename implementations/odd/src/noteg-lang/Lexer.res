// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * NoteG Language Lexer
 * Tokenizes NoteG source files for parsing
 */

type tokenType =
  // Literals
  | String
  | Number
  | Boolean
  | Null
  // Identifiers
  | Identifier
  | Keyword
  // Operators
  | Assign
  | Equals
  | NotEquals
  | Plus
  | Minus
  | Multiply
  | Divide
  | Modulo
  | And
  | Or
  | Not
  | Pipe
  | Arrow
  // Delimiters
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Comma
  | Dot
  | Colon
  | Semicolon
  // Template
  | TemplateStart
  | TemplateEnd
  | TemplateText
  // Special
  | Comment
  | Newline
  | EOF
  | Error

type token = {
  type_: tokenType,
  value: string,
  line: int,
  column: int,
  length: int,
}

type lexerError = {
  message: string,
  line: int,
  column: int,
}

let keywords = Belt.Set.String.fromArray([
  "let",
  "const",
  "fn",
  "if",
  "else",
  "for",
  "while",
  "return",
  "import",
  "export",
  "from",
  "as",
  "template",
  "content",
  "site",
  "true",
  "false",
  "null",
  "and",
  "or",
  "not",
])

type lexer = {
  source: string,
  mutable pos: int,
  mutable line: int,
  mutable column: int,
  mutable tokens: array<token>,
  mutable errors: array<lexerError>,
  mutable inTemplate: bool,
}

let createLexer = (source: string): lexer => {
  source,
  pos: 0,
  line: 1,
  column: 1,
  tokens: [],
  errors: [],
  inTemplate: false,
}

let isAtEnd = (lexer: lexer): bool => lexer.pos >= Js.String2.length(lexer.source)

let peek = (lexer: lexer): string => {
  if isAtEnd(lexer) {
    "\x00"
  } else {
    Js.String2.charAt(lexer.source, lexer.pos)
  }
}

let peekNext = (lexer: lexer): string => {
  if lexer.pos + 1 >= Js.String2.length(lexer.source) {
    "\x00"
  } else {
    Js.String2.charAt(lexer.source, lexer.pos + 1)
  }
}

let advance = (lexer: lexer): string => {
  let char = Js.String2.charAt(lexer.source, lexer.pos)
  lexer.pos = lexer.pos + 1
  lexer.column = lexer.column + 1
  char
}

let addToken = (lexer: lexer, type_: tokenType, value: string): unit => {
  Js.Array2.push(
    lexer.tokens,
    {
      type_,
      value,
      line: lexer.line,
      column: lexer.column - Js.String2.length(value),
      length: Js.String2.length(value),
    },
  )->ignore
}

let addError = (lexer: lexer, message: string): unit => {
  Js.Array2.push(
    lexer.errors,
    {
      message,
      line: lexer.line,
      column: lexer.column,
    },
  )->ignore
}

let isDigit = (char: string): bool => char >= "0" && char <= "9"

let isAlpha = (char: string): bool =>
  (char >= "a" && char <= "z") || (char >= "A" && char <= "Z") || char == "_"

let isAlphaNumeric = (char: string): bool => isAlpha(char) || isDigit(char)

let scanString = (lexer: lexer, quote: string): unit => {
  let value = ref("")

  while !isAtEnd(lexer) && peek(lexer) != quote {
    if peek(lexer) == "\n" {
      lexer.line = lexer.line + 1
      lexer.column = 1
    }
    if peek(lexer) == "\\" {
      let _ = advance(lexer)
      let escaped = advance(lexer)
      value :=
        value.contents ++
        switch escaped {
        | "n" => "\n"
        | "t" => "\t"
        | "r" => "\r"
        | "\\" => "\\"
        | "\"" => "\""
        | "'" => "'"
        | c => c
        }
    } else {
      value := value.contents ++ advance(lexer)
    }
  }

  if isAtEnd(lexer) {
    addError(lexer, "Unterminated string")
  } else {
    let _ = advance(lexer) // closing quote
    addToken(lexer, String, value.contents)
  }
}

let scanNumber = (lexer: lexer, first: string): unit => {
  let value = ref(first)

  while isDigit(peek(lexer)) {
    value := value.contents ++ advance(lexer)
  }

  if peek(lexer) == "." && isDigit(peekNext(lexer)) {
    value := value.contents ++ advance(lexer) // decimal point
    while isDigit(peek(lexer)) {
      value := value.contents ++ advance(lexer)
    }
  }

  addToken(lexer, Number, value.contents)
}

let scanIdentifier = (lexer: lexer, first: string): unit => {
  let value = ref(first)

  while isAlphaNumeric(peek(lexer)) {
    value := value.contents ++ advance(lexer)
  }

  if value.contents == "true" || value.contents == "false" {
    addToken(lexer, Boolean, value.contents)
  } else if value.contents == "null" {
    addToken(lexer, Null, value.contents)
  } else if Belt.Set.String.has(keywords, value.contents) {
    addToken(lexer, Keyword, value.contents)
  } else {
    addToken(lexer, Identifier, value.contents)
  }
}

let scanLineComment = (lexer: lexer): unit => {
  let value = ref("//")
  let _ = advance(lexer) // second /

  while !isAtEnd(lexer) && peek(lexer) != "\n" {
    value := value.contents ++ advance(lexer)
  }

  addToken(lexer, Comment, value.contents)
}

let scanBlockComment = (lexer: lexer): unit => {
  let value = ref("/*")
  let _ = advance(lexer) // *

  while !isAtEnd(lexer) {
    if peek(lexer) == "*" && peekNext(lexer) == "/" {
      value := value.contents ++ advance(lexer) ++ advance(lexer)
      break
    }
    if peek(lexer) == "\n" {
      lexer.line = lexer.line + 1
      lexer.column = 1
    }
    value := value.contents ++ advance(lexer)
  }

  addToken(lexer, Comment, value.contents)
}

let scanToken = (lexer: lexer): unit => {
  let char = advance(lexer)

  switch char {
  | "(" => addToken(lexer, LParen, char)
  | ")" => addToken(lexer, RParen, char)
  | "{" =>
    if peek(lexer) == "{" {
      let _ = advance(lexer)
      addToken(lexer, TemplateStart, "{{")
      lexer.inTemplate = true
    } else {
      addToken(lexer, LBrace, char)
    }
  | "}" =>
    if peek(lexer) == "}" && lexer.inTemplate {
      let _ = advance(lexer)
      addToken(lexer, TemplateEnd, "}}")
      lexer.inTemplate = false
    } else {
      addToken(lexer, RBrace, char)
    }
  | "[" => addToken(lexer, LBracket, char)
  | "]" => addToken(lexer, RBracket, char)
  | "," => addToken(lexer, Comma, char)
  | "." => addToken(lexer, Dot, char)
  | ":" => addToken(lexer, Colon, char)
  | ";" => addToken(lexer, Semicolon, char)
  | "+" => addToken(lexer, Plus, char)
  | "*" => addToken(lexer, Multiply, char)
  | "%" => addToken(lexer, Modulo, char)
  | "|" =>
    if peek(lexer) == ">" {
      let _ = advance(lexer)
      addToken(lexer, Pipe, "|>")
    } else if peek(lexer) == "|" {
      let _ = advance(lexer)
      addToken(lexer, Or, "||")
    } else {
      addToken(lexer, Pipe, char)
    }
  | "-" =>
    if peek(lexer) == ">" {
      let _ = advance(lexer)
      addToken(lexer, Arrow, "->")
    } else {
      addToken(lexer, Minus, char)
    }
  | "=" =>
    if peek(lexer) == "=" {
      let _ = advance(lexer)
      addToken(lexer, Equals, "==")
    } else {
      addToken(lexer, Assign, char)
    }
  | "!" =>
    if peek(lexer) == "=" {
      let _ = advance(lexer)
      addToken(lexer, NotEquals, "!=")
    } else {
      addToken(lexer, Not, char)
    }
  | "&" =>
    if peek(lexer) == "&" {
      let _ = advance(lexer)
      addToken(lexer, And, "&&")
    }
  | "/" =>
    if peek(lexer) == "/" {
      scanLineComment(lexer)
    } else if peek(lexer) == "*" {
      scanBlockComment(lexer)
    } else {
      addToken(lexer, Divide, char)
    }
  | " " | "\t" | "\r" => ()
  | "\n" =>
    addToken(lexer, Newline, char)
    lexer.line = lexer.line + 1
    lexer.column = 1
  | "\"" | "'" => scanString(lexer, char)
  | c =>
    if isDigit(c) {
      scanNumber(lexer, c)
    } else if isAlpha(c) {
      scanIdentifier(lexer, c)
    } else {
      addError(lexer, `Unexpected character: ${c}`)
    }
  }
}

let tokenize = (lexer: lexer): {"tokens": array<token>, "errors": array<lexerError>} => {
  while !isAtEnd(lexer) {
    scanToken(lexer)
  }

  addToken(lexer, EOF, "")
  {"tokens": lexer.tokens, "errors": lexer.errors}
}

let tokenizeSource = (source: string): {"tokens": array<token>, "errors": array<lexerError>} => {
  let lexer = createLexer(source)
  tokenize(lexer)
}
