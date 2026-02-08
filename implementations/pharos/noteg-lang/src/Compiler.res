// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG Language - Compiler (to JavaScript)

open Parser

type compileError = {
  message: string,
  line: option<int>,
}

type compilerState = {
  mutable output: string,
  mutable indent: int,
  mutable errors: array<compileError>,
}

let createCompiler = (): compilerState => {
  output: "",
  indent: 0,
  errors: [],
}

let emit = (compiler: compilerState, code: string): unit => {
  compiler.output = compiler.output ++ code
}

let emitLine = (compiler: compilerState, code: string): unit => {
  let indentation = Js.String2.repeat("  ", compiler.indent)
  compiler.output = compiler.output ++ indentation ++ code ++ "\n"
}

let emitNewline = (compiler: compilerState): unit => {
  compiler.output = compiler.output ++ "\n"
}

let increaseIndent = (compiler: compilerState): unit => {
  compiler.indent = compiler.indent + 1
}

let decreaseIndent = (compiler: compilerState): unit => {
  if compiler.indent > 0 {
    compiler.indent = compiler.indent - 1
  }
}

// Expression compilation
let rec compileExpr = (compiler: compilerState, expr: expr): string => {
  switch expr {
  | Literal(StringLit(s)) => {
      let escaped = s
        ->Js.String2.replaceByRe(%re("/\\/g"), "\\\\")
        ->Js.String2.replaceByRe(%re("/\"/g"), "\\\"")
        ->Js.String2.replaceByRe(%re("/\n/g"), "\\n")
        ->Js.String2.replaceByRe(%re("/\t/g"), "\\t")
      `"${escaped}"`
    }
  | Literal(NumberLit(n)) => Js.Float.toString(n)
  | Literal(BoolLit(true)) => "true"
  | Literal(BoolLit(false)) => "false"
  | Literal(NullLit) => "null"

  | Identifier(name) => sanitizeIdentifier(name)

  | Binary(left, op, right) => {
      let l = compileExpr(compiler, left)
      let r = compileExpr(compiler, right)
      let opStr = switch op {
      | Add => "+"
      | Sub => "-"
      | Mul => "*"
      | Div => "/"
      | Mod => "%"
      | Eq => "==="
      | Neq => "!=="
      | Lt => "<"
      | Lte => "<="
      | Gt => ">"
      | Gte => ">="
      | AndOp => "&&"
      | OrOp => "||"
      }
      `(${l} ${opStr} ${r})`
    }

  | Unary(op, operand) => {
      let o = compileExpr(compiler, operand)
      let opStr = switch op {
      | Neg => "-"
      | NotOp => "!"
      }
      `(${opStr}${o})`
    }

  | Call(callee, args) => {
      let fn = compileExpr(compiler, callee)
      let argStrs = args->Array.map(a => compileExpr(compiler, a))->Array.joinWith(", ")
      `${fn}(${argStrs})`
    }

  | Member(obj, prop) => {
      let o = compileExpr(compiler, obj)
      `${o}.${sanitizeIdentifier(prop)}`
    }

  | Index(obj, index) => {
      let o = compileExpr(compiler, obj)
      let i = compileExpr(compiler, index)
      `${o}[${i}]`
    }

  | Array_(elements) => {
      let elems = elements->Array.map(e => compileExpr(compiler, e))->Array.joinWith(", ")
      `[${elems}]`
    }

  | Object(entries) => {
      let props = entries->Array.map(((key, value)) => {
        let v = compileExpr(compiler, value)
        `"${key}": ${v}`
      })->Array.joinWith(", ")
      `{${props}}`
    }

  | Lambda(params, body) => {
      let paramStrs = params->Array.map(sanitizeIdentifier)->Array.joinWith(", ")
      let bodyStr = compileExpr(compiler, body)
      `((${paramStrs}) => ${bodyStr})`
    }

  | Conditional(cond, then_, else_) => {
      let c = compileExpr(compiler, cond)
      let t = compileExpr(compiler, then_)
      let e = compileExpr(compiler, else_)
      `(${c} ? ${t} : ${e})`
    }

  | Template(parts) => {
      let compiled = parts->Array.map(part => {
        switch part {
        | TextPart(s) => {
            let escaped = s
              ->Js.String2.replaceByRe(%re("/`/g"), "\\`")
              ->Js.String2.replaceByRe(%re("/\\$/g"), "\\$")
            escaped
          }
        | ExprPart(e) => "${" ++ compileExpr(compiler, e) ++ "}"
        }
      })->Array.joinWith("")
      "`" ++ compiled ++ "`"
    }

  | Pipe(left, right) => {
      // Compile pipe as function call
      switch right {
      | Call(fn, args) => {
          let fnStr = compileExpr(compiler, fn)
          let leftStr = compileExpr(compiler, left)
          let argStrs = args->Array.map(a => compileExpr(compiler, a))->Array.joinWith(", ")
          if argStrs == "" {
            `${fnStr}(${leftStr})`
          } else {
            `${fnStr}(${leftStr}, ${argStrs})`
          }
        }
      | Identifier(name) => {
          let leftStr = compileExpr(compiler, left)
          `${sanitizeIdentifier(name)}(${leftStr})`
        }
      | _ => {
          compiler.errors = compiler.errors->Array.concat([{message: "Invalid pipe target", line: None}])
          "/* error */"
        }
      }
    }
  }
}

and sanitizeIdentifier = (name: string): string => {
  // Reserved words in JavaScript that need escaping
  let reserved = [
    "break", "case", "catch", "continue", "debugger", "default", "delete",
    "do", "else", "finally", "for", "function", "if", "in", "instanceof",
    "new", "return", "switch", "this", "throw", "try", "typeof", "var",
    "void", "while", "with", "class", "const", "enum", "export", "extends",
    "import", "super", "implements", "interface", "let", "package", "private",
    "protected", "public", "static", "yield", "await"
  ]

  if reserved->Array.includes(name) {
    "_" ++ name
  } else {
    name
  }
}

// Statement compilation
let rec compileStmt = (compiler: compilerState, stmt: stmt): unit => {
  switch stmt {
  | ExprStmt(e) => {
      let code = compileExpr(compiler, e)
      emitLine(compiler, code ++ ";")
    }

  | LetStmt(name, _, value) => {
      let valueCode = compileExpr(compiler, value)
      emitLine(compiler, `let ${sanitizeIdentifier(name)} = ${valueCode};`)
    }

  | ConstStmt(name, _, value) => {
      let valueCode = compileExpr(compiler, value)
      emitLine(compiler, `const ${sanitizeIdentifier(name)} = ${valueCode};`)
    }

  | FnStmt(name, params, _, body) => {
      let paramStrs = params->Array.map(p => sanitizeIdentifier(p.name))->Array.joinWith(", ")
      emitLine(compiler, `function ${sanitizeIdentifier(name)}(${paramStrs}) {`)
      increaseIndent(compiler)
      compileBlock(compiler, body)
      decreaseIndent(compiler)
      emitLine(compiler, "}")
    }

  | IfStmt(cond, thenBlock, elseBlock) => {
      let condCode = compileExpr(compiler, cond)
      emitLine(compiler, `if (${condCode}) {`)
      increaseIndent(compiler)
      compileBlock(compiler, thenBlock)
      decreaseIndent(compiler)
      switch elseBlock {
      | Some(block) => {
          emitLine(compiler, "} else {")
          increaseIndent(compiler)
          compileBlock(compiler, block)
          decreaseIndent(compiler)
          emitLine(compiler, "}")
        }
      | None => emitLine(compiler, "}")
      }
    }

  | ForStmt(name, iterable, body) => {
      let iterCode = compileExpr(compiler, iterable)
      emitLine(compiler, `for (const ${sanitizeIdentifier(name)} of ${iterCode}) {`)
      increaseIndent(compiler)
      compileBlock(compiler, body)
      decreaseIndent(compiler)
      emitLine(compiler, "}")
    }

  | WhileStmt(cond, body) => {
      let condCode = compileExpr(compiler, cond)
      emitLine(compiler, `while (${condCode}) {`)
      increaseIndent(compiler)
      compileBlock(compiler, body)
      decreaseIndent(compiler)
      emitLine(compiler, "}")
    }

  | ReturnStmt(valueOpt) =>
    switch valueOpt {
    | Some(e) => {
        let valueCode = compileExpr(compiler, e)
        emitLine(compiler, `return ${valueCode};`)
      }
    | None => emitLine(compiler, "return;")
    }

  | YieldStmt(e) => {
      let valueCode = compileExpr(compiler, e)
      emitLine(compiler, `yield ${valueCode};`)
    }

  | ImportStmt(names, source) => {
      let nameStr = names->Array.map(sanitizeIdentifier)->Array.joinWith(", ")
      let sanitizedSource = source
        ->Js.String2.replaceByRe(%re("/\"/g"), "")
        ->Js.String2.replaceByRe(%re("/'/g"), "")
      emitLine(compiler, `import { ${nameStr} } from "${sanitizedSource}";`)
    }

  | ExportStmt(inner) => {
      emit(compiler, "export ")
      compileStmt(compiler, inner)
    }

  | TypeStmt(_, _) => {
      // Types are erased at runtime
      emitLine(compiler, "// type declaration (erased)")
    }

  | TemplateStmt(name, body) => {
      emitLine(compiler, `function ${sanitizeIdentifier(name)}(context) {`)
      increaseIndent(compiler)
      emitLine(compiler, "let __output = '';")
      compileBlock(compiler, body)
      emitLine(compiler, "return __output;")
      decreaseIndent(compiler)
      emitLine(compiler, "}")
    }

  | ContentStmt(frontmatter, body) => {
      emitLine(compiler, "// Content block")
      let titleCode = compileExpr(compiler, frontmatter.title)
      emitLine(compiler, `const __title = ${titleCode};`)
      switch frontmatter.meta {
      | Some(m) => {
          let metaCode = compileExpr(compiler, m)
          emitLine(compiler, `const __meta = ${metaCode};`)
        }
      | None => ()
      }
      compileBlock(compiler, body)
    }

  | A11yStmt(kind, value) => {
      let kindStr = switch kind {
      | BslA11y => "bsl"
      | GslA11y => "gsl"
      | AslA11y => "asl"
      | MakatonA11y => "makaton"
      }
      let valueCode = compileExpr(compiler, value)
      emitLine(compiler, `__a11y.${kindStr} = ${valueCode};`)
    }
  }
}

and compileBlock = (compiler: compilerState, block: block): unit => {
  block->Array.forEach(stmt => compileStmt(compiler, stmt))
}

// Main compile function
let compile = (program: program): result<string, array<compileError>> => {
  let compiler = createCompiler()

  // Emit runtime preamble
  emitLine(compiler, "// Generated by NoteG Compiler")
  emitLine(compiler, "// SPDX-License-Identifier: AGPL-3.0-or-later")
  emitLine(compiler, "'use strict';")
  emitNewline(compiler)

  // Emit runtime helpers
  emitLine(compiler, "const print = (...args) => console.log(...args);")
  emitLine(compiler, "const len = (x) => x.length;")
  emitLine(compiler, "const type = (x) => typeof x;")
  emitNewline(compiler)

  // Compile program
  compileBlock(compiler, program)

  if compiler.errors->Array.length > 0 {
    Error(compiler.errors)
  } else {
    Ok(compiler.output)
  }
}

// Compile to module format
let compileModule = (program: program): result<string, array<compileError>> => {
  let compiler = createCompiler()

  // Emit module preamble
  emitLine(compiler, "// Generated by NoteG Compiler (ES Module)")
  emitLine(compiler, "// SPDX-License-Identifier: AGPL-3.0-or-later")
  emitNewline(compiler)

  // Compile program
  compileBlock(compiler, program)

  if compiler.errors->Array.length > 0 {
    Error(compiler.errors)
  } else {
    Ok(compiler.output)
  }
}
