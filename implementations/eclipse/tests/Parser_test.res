// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Parser_test.res — Unit tests for NoteG parser

open Parser

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

let assertExists = (opt: option<'a>, msg: string) => {
  switch opt {
  | None => Js.Exn.raiseError(msg ++ ": expected value but got None")
  | Some(_) => ()
  }
}

// ============================================================
// HELPER FUNCTIONS
// ============================================================

let getNodeType = (node: astNode): string => {
  switch node {
  | Program(_) => "Program"
  | LetDecl(_) => "Let"
  | FunctionDecl(_) => "Function"
  | IfStmt(_) => "If"
  | ForStmt(_) => "For"
  | WhileStmt(_) => "While"
  | ReturnStmt(_) => "Return"
  | BinaryExpr(_) => "BinaryExpr"
  | UnaryExpr(_) => "UnaryExpr"
  | CallExpr(_) => "Call"
  | IdentifierExpr(_) => "Identifier"
  | LiteralExpr(_) => "Literal"
  | ArrayExpr(_) => "Array"
  | ObjectExpr(_) => "Object"
  | TemplateExpr(_) => "Template"
  | Block(_) => "Block"
  }
}

let getProgramBody = (node: astNode): array<astNode> => {
  switch node {
  | Program(body) => body
  | _ => []
  }
}

// ============================================================
// TESTS
// ============================================================

let _ = test("Parser - parses let declaration", () => {
  let ast = parseSource("let x = 42")
  let body = getProgramBody(ast)
  assertEquals(getNodeType(ast), "Program", "Node type")
  assertEquals(Array.length(body), 1, "Body length")

  switch body[0] {
  | Some(LetDecl({name, value})) =>
    assertEquals(name, "x", "Variable name")
    switch value {
    | LiteralExpr({value: NumberLit(n)}) =>
      assertEquals(n, 42.0, "Variable value")
    | _ => Js.Exn.raiseError("Expected number literal")
    }
  | _ => Js.Exn.raiseError("Expected let declaration")
  }
})

let _ = test("Parser - parses function declaration", () => {
  let ast = parseSource("fn add(a, b) { return a + b }")
  let body = getProgramBody(ast)
  assertEquals(Array.length(body), 1, "Body length")

  switch body[0] {
  | Some(FunctionDecl({name, params})) =>
    assertEquals(name, "add", "Function name")
    assertEquals(params, ["a", "b"], "Parameters")
  | _ => Js.Exn.raiseError("Expected function declaration")
  }
})

let _ = test("Parser - parses if statement", () => {
  let ast = parseSource("if x > 0 { let y = 1 }")
  let body = getProgramBody(ast)
  assertEquals(Array.length(body), 1, "Body length")

  switch body[0] {
  | Some(IfStmt({condition, thenBranch, elseBranch})) =>
    assertExists(Some(condition), "Condition")
    assertExists(Some(thenBranch), "Then branch")
    assertEquals(Option.isNone(elseBranch), true, "No else branch")
  | _ => Js.Exn.raiseError("Expected if statement")
  }
})

let _ = test("Parser - parses if-else statement", () => {
  let ast = parseSource("if x > 0 { let y = 1 } else { let y = 0 }")
  let body = getProgramBody(ast)

  switch body[0] {
  | Some(IfStmt({elseBranch})) =>
    assertExists(elseBranch, "Else branch")
  | _ => Js.Exn.raiseError("Expected if statement")
  }
})

let _ = test("Parser - parses binary expressions", () => {
  let ast = parseSource("let x = 1 + 2 * 3")
  let body = getProgramBody(ast)

  switch body[0] {
  | Some(LetDecl({value})) =>
    switch value {
    | BinaryExpr({operator}) =>
      // 1 + (2 * 3) due to precedence
      assertEquals(operator, "+", "Operator precedence")
    | _ => Js.Exn.raiseError("Expected binary expression")
    }
  | _ => Js.Exn.raiseError("Expected let declaration")
  }
})

let _ = test("Parser - parses comparison expressions", () => {
  let ast = parseSource("let x = a == b")
  let body = getProgramBody(ast)

  switch body[0] {
  | Some(LetDecl({value})) =>
    switch value {
    | BinaryExpr({operator}) =>
      assertEquals(operator, "==", "Comparison operator")
    | _ => Js.Exn.raiseError("Expected binary expression")
    }
  | _ => Js.Exn.raiseError("Expected let declaration")
  }
})

let _ = test("Parser - parses logical expressions", () => {
  let ast = parseSource("let x = a and b or c")
  let body = getProgramBody(ast)

  switch body[0] {
  | Some(LetDecl({value})) =>
    switch value {
    | BinaryExpr({operator}) =>
      // (a and b) or c
      assertEquals(operator, "or", "Logical operator")
    | _ => Js.Exn.raiseError("Expected binary expression")
    }
  | _ => Js.Exn.raiseError("Expected let declaration")
  }
})

let _ = test("Parser - parses function calls", () => {
  let ast = parseSource("print(42)")
  let body = getProgramBody(ast)
  assertEquals(Array.length(body), 1, "Body length")
  assertEquals(getNodeType(body[0]->Option.getOr(Program([]))), "Call", "Call expression")
})

let _ = test("Parser - parses arrays", () => {
  let ast = parseSource("let arr = [1, 2, 3]")
  let body = getProgramBody(ast)

  switch body[0] {
  | Some(LetDecl({value})) =>
    assertEquals(getNodeType(value), "Array", "Array expression")
  | _ => Js.Exn.raiseError("Expected let declaration")
  }
})

let _ = test("Parser - parses nested expressions", () => {
  let ast = parseSource("let x = (1 + 2) * 3")
  let body = getProgramBody(ast)

  switch body[0] {
  | Some(LetDecl({value})) =>
    switch value {
    | BinaryExpr({operator}) =>
      assertEquals(operator, "*", "Multiplication operator")
    | _ => Js.Exn.raiseError("Expected binary expression")
    }
  | _ => Js.Exn.raiseError("Expected let declaration")
  }
})

let _ = test("Parser - parses multiple statements", () => {
  let ast = parseSource("let x = 1\nlet y = 2\nlet z = x + y")
  let body = getProgramBody(ast)
  assertEquals(Array.length(body), 3, "Expected 3 statements")
})

let _ = test("Parser - parses for loop", () => {
  let ast = parseSource("for item in items { print(item) }")
  let body = getProgramBody(ast)
  assertEquals(Array.length(body), 1, "Body length")
  assertEquals(getNodeType(body[0]->Option.getOr(Program([]))), "For", "For loop")
})

let _ = test("Parser - parses while loop", () => {
  let ast = parseSource("while x > 0 { let x = x - 1 }")
  let body = getProgramBody(ast)
  assertEquals(Array.length(body), 1, "Body length")
  assertEquals(getNodeType(body[0]->Option.getOr(Program([]))), "While", "While loop")
})

let _ = test("Parser - parses template variable", () => {
  let ast = parseSource("let x = {{ title }}")
  let body = getProgramBody(ast)

  switch body[0] {
  | Some(LetDecl({value})) =>
    assertEquals(getNodeType(value), "Template", "Template expression")
  | _ => Js.Exn.raiseError("Expected let declaration")
  }
})
