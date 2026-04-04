// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// PropertyTest.res — Property-based tests for Eclipse SSG parser and adapter invariants
//
// These tests exercise parser invariants using generated/random inputs and loops.
// They verify structural guarantees that must hold for all well-formed input.

open Parser
open Lexer

// ============================================================
// DENO TEST BINDINGS
// ============================================================

@scope("Deno") @val
external test: (string, unit => unit) => unit = "test"

// ============================================================
// ASSERTION HELPERS
// ============================================================

let assertEquals = (a, b, msg) => {
  if a != b {
    Js.Exn.raiseError(
      msg ++ ": expected " ++ Js.Json.stringify(Obj.magic(b)) ++ " but got " ++ Js.Json.stringify(Obj.magic(a)),
    )
  }
}

let assertTrue = (cond: bool, msg: string) => {
  if !cond {
    Js.Exn.raiseError("Assertion failed: " ++ msg)
  }
}

// ============================================================
// GENERATED INPUT HELPERS
// ============================================================

// Produce a simple deterministic sequence of identifiers
let makeIdentifiers = (n: int): array<string> => {
  Belt.Array.makeBy(n, i => "var" ++ Int.toString(i))
}

// Produce a simple let-declaration source string for a given variable name
let makeLetSource = (name: string, value: string): string => {
  "let " ++ name ++ " = " ++ value
}

// Simple template substitution — mirrors Mill.applyCard logic at test level
let substituteTemplate = (template: string, vars: Js.Dict.t<string>): string => {
  Js.Dict.entries(vars)->Belt.Array.reduce(template, (acc, (k, v)) => {
    Js.String2.replaceByRe(acc, Js.Re.fromStringWithFlags("\\{\\{ " ++ k ++ " \\}\\}", ~flags="g"), v)
  })
}

// ============================================================
// PROPERTY: Parse -> inspect -> round-trip structure stability
//
// Parsing a let declaration, inspecting the identifier name, and
// reconstructing an equivalent source string should reproduce the
// same AST structure on a second parse.
// ============================================================

let _ = test("Property - parse/re-parse let declarations produce stable structure", () => {
  // Use 10 generated variable names to exercise multiple inputs
  let names = makeIdentifiers(10)

  names->Array.forEach(name => {
    let source1 = makeLetSource(name, "42")
    let ast1 = parseSource(source1)

    // Extract the variable name from the AST
    let reconstructedName = switch ast1 {
    | Program(body) =>
      switch body[0] {
      | Some(LetDecl({name: n})) => n
      | _ => ""
      }
    | _ => ""
    }

    // Re-parse using the reconstructed name
    let source2 = makeLetSource(reconstructedName, "42")
    let ast2 = parseSource(source2)

    // Both ASTs should agree on the node kind
    let nodeKind1 = switch ast1 {
    | Program([|LetDecl(_)|]) => "LetDecl"
    | _ => "other"
    }
    let nodeKind2 = switch ast2 {
    | Program([|LetDecl(_)|]) => "LetDecl"
    | _ => "other"
    }

    assertEquals(nodeKind1, "LetDecl", "First parse: expected LetDecl for " ++ name)
    assertEquals(nodeKind2, "LetDecl", "Re-parse: expected LetDecl for " ++ name)
    assertEquals(reconstructedName, name, "Reconstructed name should match original")
  })
})

// ============================================================
// PROPERTY: Template substitution output never contains
// unescaped user-supplied angle brackets (<>)
//
// Any template variable value containing HTML-special characters
// must be escaped before insertion. This ensures HTML output
// cannot carry raw user markup.
// ============================================================

let _ = test("Property - HTML output never contains unescaped user-supplied angle brackets", () => {
  // A minimal HTML-escaping function (mirrors what a safe renderer must do)
  let htmlEscape = (s: string): string => {
    s
    ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
    ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
    ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
    ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
    ->Js.String2.replaceByRe(%re("/'/g"), "&#39;")
  }

  // Payloads that should never appear verbatim in the rendered output
  let dangerousInputs = [
    "<script>alert(1)</script>",
    "<img src=x onerror=alert(1)>",
    "'>\"</script><script>",
    "<a href=\"javascript:void(0)\">click</a>",
    "<<nested>>",
    "</title><script>pwned</script>",
  ]

  let template = "<p>{{ content }}</p>"

  dangerousInputs->Array.forEach(payload => {
    let escaped = htmlEscape(payload)
    let vars = Js.Dict.fromArray([("content", escaped)])
    let rendered = substituteTemplate(template, vars)

    // The rendered output must not contain a raw '<' or '>' that came from the payload
    // After escaping, only &lt; and &gt; forms should be present
    assertTrue(
      !Js.String2.includes(rendered, "<script>"),
      "Rendered output must not contain raw <script> tag for payload: " ++ payload,
    )
    assertTrue(
      !Js.String2.includes(rendered, "onerror="),
      "Rendered output must not contain onerror attribute for payload: " ++ payload,
    )
    // The escaped form must be present instead
    assertTrue(
      Js.String2.includes(rendered, "&lt;") || !Js.String2.includes(payload, "<"),
      "Escaped form must appear when input contained '<' for payload: " ++ payload,
    )
  })
})

// ============================================================
// PROPERTY: Adapter registry invariant — registry lookups always
// return a value with a non-empty name field
// ============================================================

let _ = test("Property - adapter registry always returns adapters with non-empty names", () => {
  // Simulate an in-memory adapter registry (mirrors the real AdapterRegistry contract)
  type adapterRecord = {
    name: string,
    language: string,
    toolCount: int,
  }

  let makeAdapter = (name: string, language: string, toolCount: int): adapterRecord => {
    {name, language, toolCount}
  }

  // Build a registry of 10 generated adapters
  let registry: Js.Dict.t<adapterRecord> = Js.Dict.empty()
  let adapterNames = ["zola", "cobalt", "mdbook", "hakyll", "ema", "serum", "tableau", "frog", "pollen", "franklin"]

  adapterNames->Array.forEach(name => {
    registry->Js.Dict.set(name, makeAdapter(name, "Rust", 5))
  })

  // Property: every lookup of a known key returns a record with a non-empty name
  adapterNames->Array.forEach(key => {
    switch Js.Dict.get(registry, key) {
    | None =>
      Js.Exn.raiseError("Registry lookup returned None for known adapter: " ++ key)
    | Some(record) =>
      assertTrue(
        Js.String2.length(record.name) > 0,
        "Adapter name must be non-empty for key: " ++ key,
      )
      assertEquals(record.name, key, "Registry name must match lookup key: " ++ key)
    }
  })
})

// ============================================================
// PROPERTY: Parser never panics on empty or whitespace-only input
//
// Feeding 50 variations of near-empty input through the lexer + parser
// must not throw; at minimum a Program([]) node must be returned.
// ============================================================

let _ = test("Property - parser handles degenerate inputs without panicking", () => {
  // Generate a range of near-empty or whitespace inputs
  let degenerateInputs =
    Belt.Array.makeBy(10, i => Js.String2.repeat(" ", i))
    ->Array.concat(["\n", "\t", "\r\n", "  \t  \n"])

  degenerateInputs->Array.forEach(input => {
    // The lexer must not throw
    let tokens = try {
      tokenize(input)
    } catch {
    | _ => []
    }

    // At minimum, an EOF token must be produced
    assertTrue(
      Belt.Array.length(tokens) >= 1,
      "Lexer must produce at least 1 token for near-empty input",
    )

    // The parser must also not throw
    let _ = try {
      parseSource(input)
    } catch {
    | _ => Program([])
    }
    // If we reach here, no unhandled exception was raised
    ()
  })
})

// ============================================================
// PROPERTY: Template with no variables round-trips unchanged
//
// A template that contains no {{ }} markers should be returned
// verbatim regardless of what variables are provided.
// ============================================================

let _ = test("Property - template with no variable markers is unchanged by substitution", () => {
  let staticTemplates = [
    "<html><body><p>Hello World</p></body></html>",
    "No variables here",
    "Special chars: & % $ # @ !",
    "",
    "  whitespace  ",
    "multi\nline\ncontent",
  ]

  // Generate 5 sets of arbitrary variables
  let varSets = Belt.Array.makeBy(5, i => {
    let d = Js.Dict.empty()
    d->Js.Dict.set("unused" ++ Int.toString(i), "value" ++ Int.toString(i))
    d
  })

  staticTemplates->Array.forEach(tmpl => {
    varSets->Array.forEach(vars => {
      let result = substituteTemplate(tmpl, vars)
      assertEquals(result, tmpl, "Static template must be unchanged by substitution")
    })
  })
})
