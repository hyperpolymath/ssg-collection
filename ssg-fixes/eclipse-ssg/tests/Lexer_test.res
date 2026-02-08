// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Lexer_test.res — Unit tests for NoteG lexer

open Lexer

// ============================================================
// DENO TEST BINDINGS
// ============================================================

@scope("Deno") @val
external test: (string, unit => unit) => unit = "test"

let assertEquals = (a, b, msg) => {
  if a != b {
    Js.Exn.raiseError(msg ++ ": expected " ++ Js.Json.stringify(Obj.magic(b)) ++ " but got " ++ Js.Json.stringify(Obj.magic(a)))
  }
}

let assertArrayIncludes = (arr: array<'a>, items: array<'a>, _msg: string) => {
  items->Array.forEach(item => {
    if !arr->Array.includes(item) {
      Js.Exn.raiseError("Array does not include expected item")
    }
  })
}

// ============================================================
// TESTS
// ============================================================

let _ = test("Lexer - tokenizes keywords", () => {
  let source = "let fn if else for while return true false"
  let tokens = tokenize(source)
  let types = tokens->Array.map(t => t.tokenType)
  assertArrayIncludes(types, [Let, Fn, If, Else, For, While, Return, True, False], "")
})

let _ = test("Lexer - tokenizes numbers", () => {
  let source = "42 3.14 0 100"
  let tokens = tokenize(source)
  let numbers = tokens->Array.filter(t => t.tokenType == Number)
  assertEquals(Array.length(numbers), 4, "Expected 4 numbers")
  assertEquals(numbers[0]->Option.map(t => t.value), Some("42"), "First number")
  assertEquals(numbers[1]->Option.map(t => t.value), Some("3.14"), "Second number")
})

let _ = test("Lexer - tokenizes strings", () => {
  let source = "\"hello\" 'world'"
  let tokens = tokenize(source)
  let strings = tokens->Array.filter(t => t.tokenType == String)
  assertEquals(Array.length(strings), 2, "Expected 2 strings")
  assertEquals(strings[0]->Option.map(t => t.value), Some("hello"), "First string")
  assertEquals(strings[1]->Option.map(t => t.value), Some("world"), "Second string")
})

let _ = test("Lexer - tokenizes operators", () => {
  let source = "+ - * / % == != < > <= >= ="
  let tokens = tokenize(source)
  let types = tokens->Array.filter(t => t.tokenType != Eof)->Array.map(t => t.tokenType)
  assertArrayIncludes(types, [Plus, Minus, Star, Slash, Percent, Eq, Neq, Lt, Gt, Lte, Gte, Assign], "")
})

let _ = test("Lexer - tokenizes template syntax", () => {
  let source = "{{ variable }}"
  let tokens = tokenize(source)
  assertEquals(tokens[0]->Option.map(t => t.tokenType), Some(TemplateStart), "Template start")
  assertEquals(tokens[1]->Option.map(t => t.tokenType), Some(Identifier), "Identifier")
  assertEquals(tokens[1]->Option.map(t => t.value), Some("variable"), "Variable name")
  assertEquals(tokens[2]->Option.map(t => t.tokenType), Some(TemplateEnd), "Template end")
})

let _ = test("Lexer - tokenizes identifiers", () => {
  let source = "foo bar_baz myVar123"
  let tokens = tokenize(source)
  let identifiers = tokens->Array.filter(t => t.tokenType == Identifier)
  assertEquals(Array.length(identifiers), 3, "Expected 3 identifiers")
  assertEquals(identifiers[0]->Option.map(t => t.value), Some("foo"), "First identifier")
  assertEquals(identifiers[1]->Option.map(t => t.value), Some("bar_baz"), "Second identifier")
  assertEquals(identifiers[2]->Option.map(t => t.value), Some("myVar123"), "Third identifier")
})

let _ = test("Lexer - tracks line numbers", () => {
  let source = "let x = 1\nlet y = 2\nlet z = 3"
  let tokens = tokenize(source)
  let lets = tokens->Array.filter(t => t.tokenType == Let)
  assertEquals(lets[0]->Option.map(t => t.line), Some(1), "First line")
  assertEquals(lets[1]->Option.map(t => t.line), Some(2), "Second line")
  assertEquals(lets[2]->Option.map(t => t.line), Some(3), "Third line")
})

let _ = test("Lexer - handles comments", () => {
  let source = "let x = 1 // this is a comment\nlet y = 2"
  let tokens = tokenize(source)
  let identifiers = tokens->Array.filter(t => t.tokenType == Identifier)
  assertEquals(Array.length(identifiers), 2, "Expected 2 identifiers")
  assertEquals(identifiers[0]->Option.map(t => t.value), Some("x"), "First identifier")
  assertEquals(identifiers[1]->Option.map(t => t.value), Some("y"), "Second identifier")
})

let _ = test("Lexer - tokenizes arrow", () => {
  let source = "->"
  let tokens = tokenize(source)
  assertEquals(tokens[0]->Option.map(t => t.tokenType), Some(Arrow), "Arrow token")
  assertEquals(tokens[0]->Option.map(t => t.value), Some("->"), "Arrow value")
})

let _ = test("Lexer - handles empty input", () => {
  let source = ""
  let tokens = tokenize(source)
  assertEquals(Array.length(tokens), 1, "Expected 1 token")
  assertEquals(tokens[0]->Option.map(t => t.tokenType), Some(Eof), "EOF token")
})
