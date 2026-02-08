// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Lexer.res — NoteG language lexer

// ============================================================
// TOKEN TYPES
// ============================================================

type tokenType =
  // Literals
  | String
  | Number
  | Boolean
  | Identifier
  // Keywords
  | Let
  | If
  | Else
  | For
  | While
  | Fn
  | Return
  | True
  | False
  // Operators
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  | Not
  | Assign
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
  | Arrow
  // Template syntax
  | TemplateStart
  | TemplateEnd
  // Special
  | Newline
  | Eof
  | Error

type token = {
  tokenType: tokenType,
  value: string,
  line: int,
  column: int,
}

// ============================================================
// LEXER STATE
// ============================================================

type lexerState = {
  source: string,
  mutable pos: int,
  mutable line: int,
  mutable column: int,
}

let keywords: Belt.Map.String.t<tokenType> = Belt.Map.String.fromArray([
  ("let", Let),
  ("if", If),
  ("else", Else),
  ("for", For),
  ("while", While),
  ("fn", Fn),
  ("return", Return),
  ("true", True),
  ("false", False),
  ("and", And),
  ("or", Or),
  ("not", Not),
])

// ============================================================
// HELPER FUNCTIONS
// ============================================================

let create = (source: string): lexerState => {
  source,
  pos: 0,
  line: 1,
  column: 1,
}

let isAtEnd = (state: lexerState): bool => {
  state.pos >= Js.String2.length(state.source)
}

let peek = (state: lexerState): string => {
  if isAtEnd(state) {
    "\000"
  } else {
    Js.String2.charAt(state.source, state.pos)
  }
}

let peekNext = (state: lexerState): string => {
  if state.pos + 1 >= Js.String2.length(state.source) {
    "\000"
  } else {
    Js.String2.charAt(state.source, state.pos + 1)
  }
}

let advance = (state: lexerState): string => {
  let char = Js.String2.charAt(state.source, state.pos)
  state.pos = state.pos + 1
  state.column = state.column + 1
  char
}

let isDigit = (char: string): bool => {
  char >= "0" && char <= "9"
}

let isAlpha = (char: string): bool => {
  (char >= "a" && char <= "z") || (char >= "A" && char <= "Z") || char == "_"
}

let isAlphaNumeric = (char: string): bool => {
  isAlpha(char) || isDigit(char)
}

let makeToken = (state: lexerState, tokenType: tokenType, value: string): token => {
  {
    tokenType,
    value,
    line: state.line,
    column: state.column - Js.String2.length(value),
  }
}

// ============================================================
// SKIP FUNCTIONS
// ============================================================

let skipWhitespace = (state: lexerState): unit => {
  while !isAtEnd(state) {
    let char = peek(state)
    if char == " " || char == "\t" || char == "\r" {
      let _ = advance(state)
    } else {
      break
    }
  }
}

let skipLineComment = (state: lexerState): unit => {
  while !isAtEnd(state) && peek(state) != "\n" {
    let _ = advance(state)
  }
}

// ============================================================
// TOKEN PARSERS
// ============================================================

let lexString = (state: lexerState, quote: string): token => {
  let value = ref("")
  while !isAtEnd(state) && peek(state) != quote {
    if peek(state) == "\n" {
      state.line = state.line + 1
      state.column = 1
    }
    if peek(state) == "\\" && peekNext(state) == quote {
      let _ = advance(state)
    }
    value := value.contents ++ advance(state)
  }

  if isAtEnd(state) {
    makeToken(state, Error, "Unterminated string")
  } else {
    let _ = advance(state) // closing quote
    makeToken(state, String, value.contents)
  }
}

let lexNumber = (state: lexerState, first: string): token => {
  let value = ref(first)
  while isDigit(peek(state)) {
    value := value.contents ++ advance(state)
  }

  if peek(state) == "." && isDigit(peekNext(state)) {
    value := value.contents ++ advance(state) // .
    while isDigit(peek(state)) {
      value := value.contents ++ advance(state)
    }
  }

  makeToken(state, Number, value.contents)
}

let lexIdentifier = (state: lexerState, first: string): token => {
  let value = ref(first)
  while isAlphaNumeric(peek(state)) {
    value := value.contents ++ advance(state)
  }

  let tokenType = switch Belt.Map.String.get(keywords, value.contents) {
  | Some(kw) => kw
  | None => Identifier
  }

  makeToken(state, tokenType, value.contents)
}

// ============================================================
// MAIN TOKENIZER
// ============================================================

let rec nextToken = (state: lexerState): token => {
  skipWhitespace(state)

  if isAtEnd(state) {
    makeToken(state, Eof, "")
  } else {
    let char = advance(state)

    // Template syntax
    if char == "{" && peek(state) == "{" {
      let _ = advance(state)
      makeToken(state, TemplateStart, "{{")
    } else if char == "}" && peek(state) == "}" {
      let _ = advance(state)
      makeToken(state, TemplateEnd, "}}")
    } else {
      // Single character tokens
      switch char {
      | "(" => makeToken(state, LParen, char)
      | ")" => makeToken(state, RParen, char)
      | "{" => makeToken(state, LBrace, char)
      | "}" => makeToken(state, RBrace, char)
      | "[" => makeToken(state, LBracket, char)
      | "]" => makeToken(state, RBracket, char)
      | "," => makeToken(state, Comma, char)
      | "." => makeToken(state, Dot, char)
      | ":" => makeToken(state, Colon, char)
      | ";" => makeToken(state, Semicolon, char)
      | "+" => makeToken(state, Plus, char)
      | "-" =>
        if peek(state) == ">" {
          let _ = advance(state)
          makeToken(state, Arrow, "->")
        } else {
          makeToken(state, Minus, char)
        }
      | "*" => makeToken(state, Star, char)
      | "/" =>
        if peek(state) == "/" {
          skipLineComment(state)
          nextToken(state)
        } else {
          makeToken(state, Slash, char)
        }
      | "%" => makeToken(state, Percent, char)
      | "=" =>
        if peek(state) == "=" {
          let _ = advance(state)
          makeToken(state, Eq, "==")
        } else {
          makeToken(state, Assign, char)
        }
      | "!" =>
        if peek(state) == "=" {
          let _ = advance(state)
          makeToken(state, Neq, "!=")
        } else {
          makeToken(state, Not, char)
        }
      | "<" =>
        if peek(state) == "=" {
          let _ = advance(state)
          makeToken(state, Lte, "<=")
        } else {
          makeToken(state, Lt, char)
        }
      | ">" =>
        if peek(state) == "=" {
          let _ = advance(state)
          makeToken(state, Gte, ">=")
        } else {
          makeToken(state, Gt, char)
        }
      | "\n" =>
        state.line = state.line + 1
        state.column = 1
        makeToken(state, Newline, char)
      | "\"" | "'" => lexString(state, char)
      | _ =>
        if isDigit(char) {
          lexNumber(state, char)
        } else if isAlpha(char) {
          lexIdentifier(state, char)
        } else {
          makeToken(state, Error, char)
        }
      }
    }
  }
}

let tokenize = (source: string): array<token> => {
  let state = create(source)
  let tokens = ref([])

  while !isAtEnd(state) {
    let token = nextToken(state)
    if token.tokenType != Newline {
      tokens := Array.concat(tokens.contents, [token])
    }
  }

  tokens := Array.concat(tokens.contents, [makeToken(state, Eof, "")])
  tokens.contents
}

// ============================================================
// EXPORTS
// ============================================================

let tokenTypeToString = (t: tokenType): string => {
  switch t {
  | String => "STRING"
  | Number => "NUMBER"
  | Boolean => "BOOLEAN"
  | Identifier => "IDENTIFIER"
  | Let => "LET"
  | If => "IF"
  | Else => "ELSE"
  | For => "FOR"
  | While => "WHILE"
  | Fn => "FN"
  | Return => "RETURN"
  | True => "TRUE"
  | False => "FALSE"
  | Plus => "PLUS"
  | Minus => "MINUS"
  | Star => "STAR"
  | Slash => "SLASH"
  | Percent => "PERCENT"
  | Eq => "EQ"
  | Neq => "NEQ"
  | Lt => "LT"
  | Gt => "GT"
  | Lte => "LTE"
  | Gte => "GTE"
  | And => "AND"
  | Or => "OR"
  | Not => "NOT"
  | Assign => "ASSIGN"
  | LParen => "LPAREN"
  | RParen => "RPAREN"
  | LBrace => "LBRACE"
  | RBrace => "RBRACE"
  | LBracket => "LBRACKET"
  | RBracket => "RBRACKET"
  | Comma => "COMMA"
  | Dot => "DOT"
  | Colon => "COLON"
  | Semicolon => "SEMICOLON"
  | Arrow => "ARROW"
  | TemplateStart => "TEMPLATE_START"
  | TemplateEnd => "TEMPLATE_END"
  | Newline => "NEWLINE"
  | Eof => "EOF"
  | Error => "ERROR"
  }
}
