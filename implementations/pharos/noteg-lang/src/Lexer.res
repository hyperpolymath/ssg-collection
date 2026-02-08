// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG Language - Lexer

type position = {
  line: int,
  column: int,
  offset: int,
}

type tokenKind =
  // Literals
  | String(string)
  | Number(float)
  | Boolean(bool)
  | Null
  // Identifiers and keywords
  | Identifier(string)
  | Keyword(string)
  // Operators
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | And
  | Or
  | Not
  | Assign
  | Arrow
  | Pipe
  // Delimiters
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Comma
  | Colon
  | Semicolon
  | Dot
  // Template
  | TemplateStart
  | TemplateEnd
  | TemplateText(string)
  // Special
  | Newline
  | Indent(int)
  | Dedent
  | Eof
  | Error(string)

type token = {
  kind: tokenKind,
  start: position,
  end_: position,
  lexeme: string,
}

type lexerState = {
  source: string,
  mutable pos: int,
  mutable line: int,
  mutable column: int,
  mutable indentStack: array<int>,
  mutable tokens: array<token>,
}

// Keywords
let keywords = [
  "let",
  "const",
  "fn",
  "if",
  "else",
  "match",
  "for",
  "in",
  "while",
  "return",
  "yield",
  "import",
  "export",
  "from",
  "as",
  "type",
  "template",
  "content",
  "meta",
  "a11y",
  "bsl",
  "gsl",
  "asl",
  "makaton",
]

let isKeyword = (s: string): bool => {
  keywords->Array.includes(s)
}

let createLexer = (source: string): lexerState => {
  source,
  pos: 0,
  line: 1,
  column: 1,
  indentStack: [0],
  tokens: [],
}

let isAtEnd = (lexer: lexerState): bool => {
  lexer.pos >= Js.String2.length(lexer.source)
}

let peek = (lexer: lexerState): option<string> => {
  if isAtEnd(lexer) {
    None
  } else {
    Some(Js.String2.charAt(lexer.source, lexer.pos))
  }
}

let peekNext = (lexer: lexerState): option<string> => {
  if lexer.pos + 1 >= Js.String2.length(lexer.source) {
    None
  } else {
    Some(Js.String2.charAt(lexer.source, lexer.pos + 1))
  }
}

let advance = (lexer: lexerState): string => {
  let c = Js.String2.charAt(lexer.source, lexer.pos)
  lexer.pos = lexer.pos + 1
  if c == "\n" {
    lexer.line = lexer.line + 1
    lexer.column = 1
  } else {
    lexer.column = lexer.column + 1
  }
  c
}

let currentPosition = (lexer: lexerState): position => {
  {line: lexer.line, column: lexer.column, offset: lexer.pos}
}

let makeToken = (lexer: lexerState, kind: tokenKind, start: position, lexeme: string): token => {
  {
    kind,
    start,
    end_: currentPosition(lexer),
    lexeme,
  }
}

let isDigit = (c: string): bool => {
  c >= "0" && c <= "9"
}

let isAlpha = (c: string): bool => {
  (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "_"
}

let isAlphaNumeric = (c: string): bool => {
  isAlpha(c) || isDigit(c)
}

let scanString = (lexer: lexerState, quote: string): token => {
  let start = currentPosition(lexer)
  let _ = advance(lexer) // consume opening quote
  let content = ref("")

  while !isAtEnd(lexer) && peek(lexer) != Some(quote) {
    switch peek(lexer) {
    | Some("\\") => {
        let _ = advance(lexer)
        switch peek(lexer) {
        | Some("n") => {
            content := content.contents ++ "\n"
            let _ = advance(lexer)
          }
        | Some("t") => {
            content := content.contents ++ "\t"
            let _ = advance(lexer)
          }
        | Some("\\") => {
            content := content.contents ++ "\\"
            let _ = advance(lexer)
          }
        | Some(c) if c == quote => {
            content := content.contents ++ quote
            let _ = advance(lexer)
          }
        | _ => ()
        }
      }
    | Some(c) => {
        content := content.contents ++ c
        let _ = advance(lexer)
      }
    | None => ()
    }
  }

  if isAtEnd(lexer) {
    makeToken(lexer, Error("Unterminated string"), start, content.contents)
  } else {
    let _ = advance(lexer) // consume closing quote
    makeToken(lexer, String(content.contents), start, content.contents)
  }
}

let scanNumber = (lexer: lexerState): token => {
  let start = currentPosition(lexer)
  let numStr = ref("")

  while !isAtEnd(lexer) && (isDigit(peek(lexer)->Option.getOr("")) || peek(lexer) == Some(".")) {
    numStr := numStr.contents ++ advance(lexer)
  }

  switch Float.fromString(numStr.contents) {
  | Some(n) => makeToken(lexer, Number(n), start, numStr.contents)
  | None => makeToken(lexer, Error("Invalid number"), start, numStr.contents)
  }
}

let scanIdentifier = (lexer: lexerState): token => {
  let start = currentPosition(lexer)
  let ident = ref("")

  while !isAtEnd(lexer) && isAlphaNumeric(peek(lexer)->Option.getOr("")) {
    ident := ident.contents ++ advance(lexer)
  }

  let kind = switch ident.contents {
  | "true" => Boolean(true)
  | "false" => Boolean(false)
  | "null" => Null
  | s if isKeyword(s) => Keyword(s)
  | s => Identifier(s)
  }

  makeToken(lexer, kind, start, ident.contents)
}

let scanToken = (lexer: lexerState): token => {
  // Skip whitespace (but track for indentation)
  while !isAtEnd(lexer) && (peek(lexer) == Some(" ") || peek(lexer) == Some("\t")) {
    let _ = advance(lexer)
  }

  if isAtEnd(lexer) {
    makeToken(lexer, Eof, currentPosition(lexer), "")
  } else {
    let start = currentPosition(lexer)
    let c = advance(lexer)

    switch c {
    | "(" => makeToken(lexer, LeftParen, start, c)
    | ")" => makeToken(lexer, RightParen, start, c)
    | "{" => {
        // Check for template start {{
        if peek(lexer) == Some("{") {
          let _ = advance(lexer)
          makeToken(lexer, TemplateStart, start, "{{")
        } else {
          makeToken(lexer, LeftBrace, start, c)
        }
      }
    | "}" => {
        if peek(lexer) == Some("}") {
          let _ = advance(lexer)
          makeToken(lexer, TemplateEnd, start, "}}")
        } else {
          makeToken(lexer, RightBrace, start, c)
        }
      }
    | "[" => makeToken(lexer, LeftBracket, start, c)
    | "]" => makeToken(lexer, RightBracket, start, c)
    | "," => makeToken(lexer, Comma, start, c)
    | ":" => makeToken(lexer, Colon, start, c)
    | ";" => makeToken(lexer, Semicolon, start, c)
    | "." => makeToken(lexer, Dot, start, c)
    | "+" => makeToken(lexer, Plus, start, c)
    | "-" => {
        if peek(lexer) == Some(">") {
          let _ = advance(lexer)
          makeToken(lexer, Arrow, start, "->")
        } else {
          makeToken(lexer, Minus, start, c)
        }
      }
    | "*" => makeToken(lexer, Star, start, c)
    | "/" => {
        // Check for comments
        if peek(lexer) == Some("/") {
          // Line comment
          while !isAtEnd(lexer) && peek(lexer) != Some("\n") {
            let _ = advance(lexer)
          }
          scanToken(lexer)
        } else if peek(lexer) == Some("*") {
          // Block comment
          let _ = advance(lexer)
          while !isAtEnd(lexer) && !(peek(lexer) == Some("*") && peekNext(lexer) == Some("/")) {
            let _ = advance(lexer)
          }
          if !isAtEnd(lexer) {
            let _ = advance(lexer)
            let _ = advance(lexer)
          }
          scanToken(lexer)
        } else {
          makeToken(lexer, Slash, start, c)
        }
      }
    | "%" => makeToken(lexer, Percent, start, c)
    | "=" => {
        if peek(lexer) == Some("=") {
          let _ = advance(lexer)
          makeToken(lexer, Equal, start, "==")
        } else {
          makeToken(lexer, Assign, start, c)
        }
      }
    | "!" => {
        if peek(lexer) == Some("=") {
          let _ = advance(lexer)
          makeToken(lexer, NotEqual, start, "!=")
        } else {
          makeToken(lexer, Not, start, c)
        }
      }
    | "<" => {
        if peek(lexer) == Some("=") {
          let _ = advance(lexer)
          makeToken(lexer, LessEqual, start, "<=")
        } else {
          makeToken(lexer, Less, start, c)
        }
      }
    | ">" => {
        if peek(lexer) == Some("=") {
          let _ = advance(lexer)
          makeToken(lexer, GreaterEqual, start, ">=")
        } else {
          makeToken(lexer, Greater, start, c)
        }
      }
    | "&" if peek(lexer) == Some("&") => {
        let _ = advance(lexer)
        makeToken(lexer, And, start, "&&")
      }
    | "|" => {
        if peek(lexer) == Some("|") {
          let _ = advance(lexer)
          makeToken(lexer, Or, start, "||")
        } else {
          makeToken(lexer, Pipe, start, c)
        }
      }
    | "\n" => makeToken(lexer, Newline, start, c)
    | "\"" | "'" => {
        lexer.pos = lexer.pos - 1
        lexer.column = lexer.column - 1
        scanString(lexer, c)
      }
    | c if isDigit(c) => {
        lexer.pos = lexer.pos - 1
        lexer.column = lexer.column - 1
        scanNumber(lexer)
      }
    | c if isAlpha(c) => {
        lexer.pos = lexer.pos - 1
        lexer.column = lexer.column - 1
        scanIdentifier(lexer)
      }
    | c => makeToken(lexer, Error(`Unexpected character: ${c}`), start, c)
    }
  }
}

let tokenize = (source: string): array<token> => {
  let lexer = createLexer(source)
  let tokens = ref([])

  while !isAtEnd(lexer) {
    let token = scanToken(lexer)
    tokens := tokens.contents->Array.concat([token])
    if token.kind == Eof {
      lexer.pos = Js.String2.length(lexer.source) // Force exit
    }
  }

  // Add EOF if not present
  if tokens.contents->Array.length == 0 || tokens.contents[Array.length(tokens.contents) - 1]->Option.map(t => t.kind) != Some(Eof) {
    tokens := tokens.contents->Array.concat([makeToken(lexer, Eof, currentPosition(lexer), "")])
  }

  tokens.contents
}
