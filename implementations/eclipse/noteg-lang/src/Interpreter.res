// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Interpreter.res — NoteG language interpreter

open Parser

// ============================================================
// RUNTIME VALUES
// ============================================================

type rec value =
  | StringVal(string)
  | NumberVal(float)
  | BoolVal(bool)
  | NullVal
  | ArrayVal(array<value>)
  | ObjectVal(Js.Dict.t<value>)
  | FunctionVal({
      name: string,
      params: array<string>,
      body: astNode,
      closure: environment,
    })

and environment = {
  values: Js.Dict.t<value>,
  parent: option<environment>,
}

// ============================================================
// RETURN EXCEPTION
// ============================================================

exception ReturnException(value)
exception RuntimeError(string)

// ============================================================
// ENVIRONMENT
// ============================================================

let createEnv = (parent: option<environment>): environment => {
  values: Js.Dict.empty(),
  parent,
}

let define = (env: environment, name: string, value: value): unit => {
  Js.Dict.set(env.values, name, value)
}

let rec get = (env: environment, name: string): value => {
  switch Js.Dict.get(env.values, name) {
  | Some(v) => v
  | None =>
    switch env.parent {
    | Some(p) => get(p, name)
    | None => raise(RuntimeError("Undefined variable: " ++ name))
    }
  }
}

let rec assign = (env: environment, name: string, value: value): unit => {
  switch Js.Dict.get(env.values, name) {
  | Some(_) => Js.Dict.set(env.values, name, value)
  | None =>
    switch env.parent {
    | Some(p) => assign(p, name, value)
    | None => raise(RuntimeError("Undefined variable: " ++ name))
    }
  }
}

// ============================================================
// HELPER FUNCTIONS
// ============================================================

let isTruthy = (value: value): bool => {
  switch value {
  | NullVal => false
  | BoolVal(b) => b
  | NumberVal(n) => n != 0.0
  | StringVal(s) => Js.String2.length(s) > 0
  | ArrayVal(arr) => Array.length(arr) > 0
  | _ => true
  }
}

let valueToString = (value: value): string => {
  switch value {
  | StringVal(s) => s
  | NumberVal(n) => Float.toString(n)
  | BoolVal(b) => b ? "true" : "false"
  | NullVal => "null"
  | ArrayVal(arr) =>
    "[" ++ arr->Array.map(valueToString)->Array.joinWith(", ") ++ "]"
  | ObjectVal(_) => "[object Object]"
  | FunctionVal({name}) => "[function " ++ name ++ "]"
  }
}

// ============================================================
// INTERPRETER
// ============================================================

let rec evaluate = (node: astNode, env: environment): value => {
  switch node {
  | Program(body) =>
    let result = ref(NullVal)
    body->Array.forEach(stmt => {
      result := evaluate(stmt, env)
    })
    result.contents

  | LetDecl({name, value: valueNode}) =>
    let value = evaluate(valueNode, env)
    define(env, name, value)
    value

  | FunctionDecl({name, params, body}) =>
    let fn = FunctionVal({name, params, body, closure: env})
    define(env, name, fn)
    fn

  | IfStmt({condition, thenBranch, elseBranch}) =>
    let condValue = evaluate(condition, env)
    if isTruthy(condValue) {
      evaluate(thenBranch, env)
    } else {
      switch elseBranch {
      | Some(elseNode) => evaluate(elseNode, env)
      | None => NullVal
      }
    }

  | ForStmt({variable, iterable, body}) =>
    let iterValue = evaluate(iterable, env)
    switch iterValue {
    | ArrayVal(arr) =>
      let result = ref(NullVal)
      arr->Array.forEach(item => {
        let loopEnv = createEnv(Some(env))
        define(loopEnv, variable, item)
        result := evaluate(body, loopEnv)
      })
      result.contents
    | _ => raise(RuntimeError("For loop requires an array"))
    }

  | WhileStmt({condition, body}) =>
    let result = ref(NullVal)
    while isTruthy(evaluate(condition, env)) {
      result := evaluate(body, env)
    }
    result.contents

  | ReturnStmt({value: valueOpt}) =>
    let returnValue = switch valueOpt {
    | Some(v) => evaluate(v, env)
    | None => NullVal
    }
    raise(ReturnException(returnValue))

  | Block({statements}) =>
    let blockEnv = createEnv(Some(env))
    let result = ref(NullVal)
    statements->Array.forEach(stmt => {
      result := evaluate(stmt, blockEnv)
    })
    result.contents

  | BinaryExpr({operator, left, right}) =>
    let leftVal = evaluate(left, env)
    let rightVal = evaluate(right, env)
    evaluateBinary(operator, leftVal, rightVal)

  | UnaryExpr({operator, operand}) =>
    let operandVal = evaluate(operand, env)
    evaluateUnary(operator, operandVal)

  | CallExpr({callee, args}) =>
    let calleeVal = evaluate(callee, env)
    let argVals = args->Array.map(a => evaluate(a, env))
    evaluateCall(calleeVal, argVals)

  | IdentifierExpr({name}) => get(env, name)

  | LiteralExpr({value}) =>
    switch value {
    | StringLit(s) => StringVal(s)
    | NumberLit(n) => NumberVal(n)
    | BoolLit(b) => BoolVal(b)
    | NullLit => NullVal
    }

  | ArrayExpr({elements}) =>
    let elementVals = elements->Array.map(e => evaluate(e, env))
    ArrayVal(elementVals)

  | ObjectExpr({properties}) =>
    let dict = Js.Dict.empty()
    properties->Array.forEach(((key, valueNode)) => {
      let value = evaluate(valueNode, env)
      Js.Dict.set(dict, key, value)
    })
    ObjectVal(dict)

  | TemplateExpr({variable}) => get(env, variable)
  }
}

and evaluateBinary = (operator: string, left: value, right: value): value => {
  switch operator {
  | "+" =>
    switch (left, right) {
    | (StringVal(l), _) => StringVal(l ++ valueToString(right))
    | (_, StringVal(r)) => StringVal(valueToString(left) ++ r)
    | (NumberVal(l), NumberVal(r)) => NumberVal(l +. r)
    | _ => raise(RuntimeError("Invalid operands for +"))
    }
  | "-" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => NumberVal(l -. r)
    | _ => raise(RuntimeError("Invalid operands for -"))
    }
  | "*" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => NumberVal(l *. r)
    | _ => raise(RuntimeError("Invalid operands for *"))
    }
  | "/" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => NumberVal(l /. r)
    | _ => raise(RuntimeError("Invalid operands for /"))
    }
  | "%" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => NumberVal(mod_float(l, r))
    | _ => raise(RuntimeError("Invalid operands for %"))
    }
  | "==" => BoolVal(left == right)
  | "!=" => BoolVal(left != right)
  | "<" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => BoolVal(l < r)
    | _ => raise(RuntimeError("Invalid operands for <"))
    }
  | ">" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => BoolVal(l > r)
    | _ => raise(RuntimeError("Invalid operands for >"))
    }
  | "<=" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => BoolVal(l <= r)
    | _ => raise(RuntimeError("Invalid operands for <="))
    }
  | ">=" =>
    switch (left, right) {
    | (NumberVal(l), NumberVal(r)) => BoolVal(l >= r)
    | _ => raise(RuntimeError("Invalid operands for >="))
    }
  | "and" => BoolVal(isTruthy(left) && isTruthy(right))
  | "or" => BoolVal(isTruthy(left) || isTruthy(right))
  | _ => raise(RuntimeError("Unknown operator: " ++ operator))
  }
}

and evaluateUnary = (operator: string, operand: value): value => {
  switch operator {
  | "-" =>
    switch operand {
    | NumberVal(n) => NumberVal(-.n)
    | _ => raise(RuntimeError("Invalid operand for -"))
    }
  | "not" | "!" => BoolVal(!isTruthy(operand))
  | _ => raise(RuntimeError("Unknown operator: " ++ operator))
  }
}

and evaluateCall = (callee: value, args: array<value>): value => {
  switch callee {
  | FunctionVal({name, params, body, closure}) =>
    // Handle built-in functions
    switch name {
    | "print" =>
      args->Array.forEach(arg => {
        Js.log(valueToString(arg))
      })
      NullVal
    | "len" =>
      switch args[0] {
      | Some(ArrayVal(arr)) => NumberVal(Float.fromInt(Array.length(arr)))
      | Some(StringVal(s)) => NumberVal(Float.fromInt(Js.String2.length(s)))
      | _ => raise(RuntimeError("len() requires an array or string"))
      }
    | _ =>
      // User-defined function
      let fnEnv = createEnv(Some(closure))
      params->Array.forEachWithIndex((i, param) => {
        let argValue = switch args[i] {
        | Some(v) => v
        | None => NullVal
        }
        define(fnEnv, param, argValue)
      })
      try {
        evaluate(body, fnEnv)
      } catch {
      | ReturnException(returnValue) => returnValue
      }
    }
  | _ => raise(RuntimeError("Can only call functions"))
  }
}

// ============================================================
// MAIN INTERPRETER
// ============================================================

let setupBuiltins = (env: environment): unit => {
  // Built-in print function
  define(
    env,
    "print",
    FunctionVal({
      name: "print",
      params: ["value"],
      body: Block({statements: []}),
      closure: env,
    }),
  )
  // Built-in len function
  define(
    env,
    "len",
    FunctionVal({
      name: "len",
      params: ["value"],
      body: Block({statements: []}),
      closure: env,
    }),
  )
}

let interpret = (program: astNode): value => {
  let globalEnv = createEnv(None)
  setupBuiltins(globalEnv)
  evaluate(program, globalEnv)
}

let interpretSource = (source: string): value => {
  let ast = parseSource(source)
  interpret(ast)
}
