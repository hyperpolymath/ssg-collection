// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG Language - Interpreter

open Parser

type rec value =
  | VNull
  | VBool(bool)
  | VNumber(float)
  | VString(string)
  | VArray(array<value>)
  | VObject(Js.Dict.t<value>)
  | VFunction(closure)
  | VNative(string, array<value> => result<value, runtimeError>)

and closure = {
  params: array<string>,
  body: block,
  env: environment,
}

and environment = {
  values: Js.Dict.t<value>,
  parent: option<environment>,
}

and runtimeError = {
  message: string,
  line: option<int>,
}

// Environment operations
let createEnv = (parent: option<environment>): environment => {
  {values: Js.Dict.empty(), parent}
}

let defineVar = (env: environment, name: string, value: value): unit => {
  Js.Dict.set(env.values, name, value)
}

let rec lookupVar = (env: environment, name: string): option<value> => {
  switch Js.Dict.get(env.values, name) {
  | Some(v) => Some(v)
  | None =>
    switch env.parent {
    | Some(p) => lookupVar(p, name)
    | None => None
    }
  }
}

let rec assignVar = (env: environment, name: string, value: value): bool => {
  switch Js.Dict.get(env.values, name) {
  | Some(_) => {
      Js.Dict.set(env.values, name, value)
      true
    }
  | None =>
    switch env.parent {
    | Some(p) => assignVar(p, name, value)
    | None => false
    }
  }
}

// Type conversions
let isTruthy = (v: value): bool => {
  switch v {
  | VNull => false
  | VBool(b) => b
  | VNumber(n) => n != 0.0
  | VString(s) => Js.String2.length(s) > 0
  | VArray(a) => Array.length(a) > 0
  | _ => true
  }
}

let valueToString = (v: value): string => {
  switch v {
  | VNull => "null"
  | VBool(true) => "true"
  | VBool(false) => "false"
  | VNumber(n) => Js.Float.toString(n)
  | VString(s) => s
  | VArray(_) => "[Array]"
  | VObject(_) => "[Object]"
  | VFunction(_) => "[Function]"
  | VNative(name, _) => `[Native: ${name}]`
  }
}

// Native functions
let nativePrint = (args: array<value>): result<value, runtimeError> => {
  let output = args->Array.map(valueToString)->Array.joinWith(" ")
  Js.log(output)
  Ok(VNull)
}

let nativeLen = (args: array<value>): result<value, runtimeError> => {
  switch args[0] {
  | Some(VString(s)) => Ok(VNumber(Float.fromInt(Js.String2.length(s))))
  | Some(VArray(a)) => Ok(VNumber(Float.fromInt(Array.length(a))))
  | Some(VObject(o)) => Ok(VNumber(Float.fromInt(Array.length(Js.Dict.keys(o)))))
  | _ => Error({message: "len() expects string, array, or object", line: None})
  }
}

let nativeType = (args: array<value>): result<value, runtimeError> => {
  switch args[0] {
  | Some(VNull) => Ok(VString("null"))
  | Some(VBool(_)) => Ok(VString("boolean"))
  | Some(VNumber(_)) => Ok(VString("number"))
  | Some(VString(_)) => Ok(VString("string"))
  | Some(VArray(_)) => Ok(VString("array"))
  | Some(VObject(_)) => Ok(VString("object"))
  | Some(VFunction(_)) => Ok(VString("function"))
  | Some(VNative(_, _)) => Ok(VString("native"))
  | None => Error({message: "type() expects one argument", line: None})
  }
}

let createGlobalEnv = (): environment => {
  let env = createEnv(None)
  defineVar(env, "print", VNative("print", nativePrint))
  defineVar(env, "len", VNative("len", nativeLen))
  defineVar(env, "type", VNative("type", nativeType))
  defineVar(env, "null", VNull)
  defineVar(env, "true", VBool(true))
  defineVar(env, "false", VBool(false))
  env
}

// Expression evaluation
let rec evalExpr = (expr: expr, env: environment): result<value, runtimeError> => {
  switch expr {
  | Literal(StringLit(s)) => Ok(VString(s))
  | Literal(NumberLit(n)) => Ok(VNumber(n))
  | Literal(BoolLit(b)) => Ok(VBool(b))
  | Literal(NullLit) => Ok(VNull)

  | Identifier(name) =>
    switch lookupVar(env, name) {
    | Some(v) => Ok(v)
    | None => Error({message: `Undefined variable: ${name}`, line: None})
    }

  | Binary(left, op, right) => evalBinary(left, op, right, env)
  | Unary(op, operand) => evalUnary(op, operand, env)
  | Call(callee, args) => evalCall(callee, args, env)
  | Member(obj, prop) => evalMember(obj, prop, env)
  | Index(obj, index) => evalIndex(obj, index, env)
  | Array_(elements) => evalArray(elements, env)
  | Object(entries) => evalObject(entries, env)
  | Lambda(params, body) => Ok(VFunction({params, body: [ExprStmt(body)], env}))
  | Conditional(cond, then_, else_) => evalConditional(cond, then_, else_, env)
  | Template(parts) => evalTemplate(parts, env)
  | Pipe(left, right) => evalPipe(left, right, env)
  }
}

and evalBinary = (left: expr, op: binaryOp, right: expr, env: environment): result<value, runtimeError> => {
  switch (evalExpr(left, env), evalExpr(right, env)) {
  | (Ok(l), Ok(r)) =>
    switch (op, l, r) {
    | (Add, VNumber(a), VNumber(b)) => Ok(VNumber(a +. b))
    | (Add, VString(a), VString(b)) => Ok(VString(a ++ b))
    | (Add, VString(a), b) => Ok(VString(a ++ valueToString(b)))
    | (Add, a, VString(b)) => Ok(VString(valueToString(a) ++ b))
    | (Sub, VNumber(a), VNumber(b)) => Ok(VNumber(a -. b))
    | (Mul, VNumber(a), VNumber(b)) => Ok(VNumber(a *. b))
    | (Div, VNumber(a), VNumber(b)) =>
      if b == 0.0 {
        Error({message: "Division by zero", line: None})
      } else {
        Ok(VNumber(a /. b))
      }
    | (Mod, VNumber(a), VNumber(b)) => Ok(VNumber(mod_float(a, b)))
    | (Eq, a, b) => Ok(VBool(valuesEqual(a, b)))
    | (Neq, a, b) => Ok(VBool(!valuesEqual(a, b)))
    | (Lt, VNumber(a), VNumber(b)) => Ok(VBool(a < b))
    | (Lte, VNumber(a), VNumber(b)) => Ok(VBool(a <= b))
    | (Gt, VNumber(a), VNumber(b)) => Ok(VBool(a > b))
    | (Gte, VNumber(a), VNumber(b)) => Ok(VBool(a >= b))
    | (AndOp, a, b) => Ok(VBool(isTruthy(a) && isTruthy(b)))
    | (OrOp, a, b) => Ok(VBool(isTruthy(a) || isTruthy(b)))
    | _ => Error({message: "Invalid operands for operator", line: None})
    }
  | (Error(e), _) => Error(e)
  | (_, Error(e)) => Error(e)
  }
}

and valuesEqual = (a: value, b: value): bool => {
  switch (a, b) {
  | (VNull, VNull) => true
  | (VBool(x), VBool(y)) => x == y
  | (VNumber(x), VNumber(y)) => x == y
  | (VString(x), VString(y)) => x == y
  | _ => false
  }
}

and evalUnary = (op: unaryOp, operand: expr, env: environment): result<value, runtimeError> => {
  switch evalExpr(operand, env) {
  | Ok(v) =>
    switch (op, v) {
    | (Neg, VNumber(n)) => Ok(VNumber(-.n))
    | (NotOp, v) => Ok(VBool(!isTruthy(v)))
    | _ => Error({message: "Invalid operand for unary operator", line: None})
    }
  | Error(e) => Error(e)
  }
}

and evalCall = (callee: expr, args: array<expr>, env: environment): result<value, runtimeError> => {
  switch evalExpr(callee, env) {
  | Ok(VFunction({params, body, env: closureEnv})) => {
      let localEnv = createEnv(Some(closureEnv))
      let evalArgs = args->Array.map(a => evalExpr(a, env))

      // Check for errors in args
      let hasError = evalArgs->Array.some(r =>
        switch r {
        | Error(_) => true
        | Ok(_) => false
        }
      )

      if hasError {
        Error({message: "Error evaluating arguments", line: None})
      } else {
        // Bind parameters
        params->Array.forEachWithIndex((i, param) => {
          switch evalArgs[i] {
          | Some(Ok(v)) => defineVar(localEnv, param, v)
          | _ => ()
          }
        })

        execBlock(body, localEnv)
      }
    }
  | Ok(VNative(_, fn)) => {
      let evalArgs =
        args
        ->Array.map(a => evalExpr(a, env))
        ->Array.filterMap(r =>
          switch r {
          | Ok(v) => Some(v)
          | Error(_) => None
          }
        )
      fn(evalArgs)
    }
  | Ok(_) => Error({message: "Can only call functions", line: None})
  | Error(e) => Error(e)
  }
}

and evalMember = (obj: expr, prop: string, env: environment): result<value, runtimeError> => {
  switch evalExpr(obj, env) {
  | Ok(VObject(o)) =>
    switch Js.Dict.get(o, prop) {
    | Some(v) => Ok(v)
    | None => Error({message: `Property ${prop} not found`, line: None})
    }
  | Ok(_) => Error({message: "Can only access properties on objects", line: None})
  | Error(e) => Error(e)
  }
}

and evalIndex = (obj: expr, index: expr, env: environment): result<value, runtimeError> => {
  switch (evalExpr(obj, env), evalExpr(index, env)) {
  | (Ok(VArray(a)), Ok(VNumber(i))) => {
      let idx = Float.toInt(i)
      switch a[idx] {
      | Some(v) => Ok(v)
      | None => Error({message: "Index out of bounds", line: None})
      }
    }
  | (Ok(VObject(o)), Ok(VString(k))) =>
    switch Js.Dict.get(o, k) {
    | Some(v) => Ok(v)
    | None => Error({message: `Key ${k} not found`, line: None})
    }
  | (Ok(_), Ok(_)) => Error({message: "Invalid index operation", line: None})
  | (Error(e), _) => Error(e)
  | (_, Error(e)) => Error(e)
  }
}

and evalArray = (elements: array<expr>, env: environment): result<value, runtimeError> => {
  let values = ref([])
  let error = ref(None)

  elements->Array.forEach(e => {
    if error.contents == None {
      switch evalExpr(e, env) {
      | Ok(v) => values := values.contents->Array.concat([v])
      | Error(e) => error := Some(e)
      }
    }
  })

  switch error.contents {
  | Some(e) => Error(e)
  | None => Ok(VArray(values.contents))
  }
}

and evalObject = (entries: array<(string, expr)>, env: environment): result<value, runtimeError> => {
  let obj = Js.Dict.empty()
  let error = ref(None)

  entries->Array.forEach(((key, valueExpr)) => {
    if error.contents == None {
      switch evalExpr(valueExpr, env) {
      | Ok(v) => Js.Dict.set(obj, key, v)
      | Error(e) => error := Some(e)
      }
    }
  })

  switch error.contents {
  | Some(e) => Error(e)
  | None => Ok(VObject(obj))
  }
}

and evalConditional = (cond: expr, then_: expr, else_: expr, env: environment): result<value, runtimeError> => {
  switch evalExpr(cond, env) {
  | Ok(v) =>
    if isTruthy(v) {
      evalExpr(then_, env)
    } else {
      evalExpr(else_, env)
    }
  | Error(e) => Error(e)
  }
}

and evalTemplate = (parts: array<templatePart>, env: environment): result<value, runtimeError> => {
  let result = ref("")
  let error = ref(None)

  parts->Array.forEach(part => {
    if error.contents == None {
      switch part {
      | TextPart(s) => result := result.contents ++ s
      | ExprPart(e) =>
        switch evalExpr(e, env) {
        | Ok(v) => result := result.contents ++ valueToString(v)
        | Error(e) => error := Some(e)
        }
      }
    }
  })

  switch error.contents {
  | Some(e) => Error(e)
  | None => Ok(VString(result.contents))
  }
}

and evalPipe = (left: expr, right: expr, env: environment): result<value, runtimeError> => {
  switch evalExpr(left, env) {
  | Ok(v) =>
    // For pipe, the right side should be a function call
    // We inject the left value as the first argument
    switch right {
    | Call(fn, args) => {
        let newArgs = [left]->Array.concat(args)
        evalCall(fn, newArgs, env)
      }
    | Identifier(_) => evalCall(right, [left], env)
    | _ => Error({message: "Right side of pipe must be a function", line: None})
    }
  | Error(e) => Error(e)
  }
}

// Statement execution
and execStmt = (stmt: stmt, env: environment): result<value, runtimeError> => {
  switch stmt {
  | ExprStmt(e) => evalExpr(e, env)

  | LetStmt(name, _, value) =>
    switch evalExpr(value, env) {
    | Ok(v) => {
        defineVar(env, name, v)
        Ok(VNull)
      }
    | Error(e) => Error(e)
    }

  | ConstStmt(name, _, value) =>
    switch evalExpr(value, env) {
    | Ok(v) => {
        defineVar(env, name, v)
        Ok(VNull)
      }
    | Error(e) => Error(e)
    }

  | FnStmt(name, params, _, body) => {
      let paramNames = params->Array.map(p => p.name)
      let fn = VFunction({params: paramNames, body, env})
      defineVar(env, name, fn)
      Ok(VNull)
    }

  | IfStmt(cond, thenBlock, elseBlock) =>
    switch evalExpr(cond, env) {
    | Ok(v) =>
      if isTruthy(v) {
        execBlock(thenBlock, createEnv(Some(env)))
      } else {
        switch elseBlock {
        | Some(block) => execBlock(block, createEnv(Some(env)))
        | None => Ok(VNull)
        }
      }
    | Error(e) => Error(e)
    }

  | ForStmt(name, iterable, body) =>
    switch evalExpr(iterable, env) {
    | Ok(VArray(items)) => {
        let result = ref(VNull)
        let error = ref(None)

        items->Array.forEach(item => {
          if error.contents == None {
            let loopEnv = createEnv(Some(env))
            defineVar(loopEnv, name, item)
            switch execBlock(body, loopEnv) {
            | Ok(v) => result := v
            | Error(e) => error := Some(e)
            }
          }
        })

        switch error.contents {
        | Some(e) => Error(e)
        | None => Ok(result.contents)
        }
      }
    | Ok(_) => Error({message: "Can only iterate over arrays", line: None})
    | Error(e) => Error(e)
    }

  | WhileStmt(cond, body) => {
      let result = ref(VNull)
      let error = ref(None)
      let continue = ref(true)

      while continue.contents && error.contents == None {
        switch evalExpr(cond, env) {
        | Ok(v) =>
          if isTruthy(v) {
            switch execBlock(body, createEnv(Some(env))) {
            | Ok(v) => result := v
            | Error(e) => error := Some(e)
            }
          } else {
            continue := false
          }
        | Error(e) => error := Some(e)
        }
      }

      switch error.contents {
      | Some(e) => Error(e)
      | None => Ok(result.contents)
      }
    }

  | ReturnStmt(valueOpt) =>
    switch valueOpt {
    | Some(e) => evalExpr(e, env)
    | None => Ok(VNull)
    }

  | YieldStmt(e) => evalExpr(e, env)

  | ImportStmt(_, _) => Ok(VNull) // TODO: implement imports
  | ExportStmt(inner) => execStmt(inner, env)
  | TypeStmt(_, _) => Ok(VNull) // Types are compile-time only
  | TemplateStmt(_, _) => Ok(VNull) // TODO: implement templates
  | ContentStmt(_, _) => Ok(VNull) // TODO: implement content
  | A11yStmt(_, _) => Ok(VNull) // TODO: implement a11y
  }
}

and execBlock = (block: block, env: environment): result<value, runtimeError> => {
  let result = ref(VNull)
  let error = ref(None)

  block->Array.forEach(stmt => {
    if error.contents == None {
      switch execStmt(stmt, env) {
      | Ok(v) => result := v
      | Error(e) => error := Some(e)
      }
    }
  })

  switch error.contents {
  | Some(e) => Error(e)
  | None => Ok(result.contents)
  }
}

// Main interpret function
let interpret = (program: program): result<value, runtimeError> => {
  let env = createGlobalEnv()
  execBlock(program, env)
}
