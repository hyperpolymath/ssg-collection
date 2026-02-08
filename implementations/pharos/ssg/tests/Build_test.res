// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG SSG - Build System Unit Tests

open Types
open Build

// Test helpers
let assertEqual = (name: string, expected: 'a, actual: 'a): unit => {
  if expected == actual {
    Js.log(`✓ ${name}`)
  } else {
    Js.log(`✗ ${name}`)
    Js.log(`  Expected: ${Js.Json.stringifyAny(expected)->Option.getOr("?")}`)
    Js.log(`  Actual: ${Js.Json.stringifyAny(actual)->Option.getOr("?")}`)
  }
}

let assertTrue = (name: string, condition: bool): unit => {
  if condition {
    Js.log(`✓ ${name}`)
  } else {
    Js.log(`✗ ${name}`)
  }
}

// VariableStore tests
let testVariableStore = () => {
  Js.log("\n=== VariableStore Tests ===")

  let store = VariableStore.empty()
  assertTrue("Empty store has no values", !VariableStore.has(store, "test"))

  let store2 = VariableStore.set(store, "name", Js.Json.string("NoteG"))
  assertTrue("Set value exists", VariableStore.has(store2, "name"))

  switch VariableStore.get(store2, "name") {
  | Some(v) => {
      switch Js.Json.classify(v) {
      | Js.Json.JSONString(s) => assertEqual("Get returns correct value", "NoteG", s)
      | _ => Js.log("✗ Get returns wrong type")
      }
    }
  | None => Js.log("✗ Get returns None")
  }

  assertTrue("Original store unchanged", !VariableStore.has(store, "name"))
}

// Template tests
let testTemplate = () => {
  Js.log("\n=== Template Tests ===")

  assertTrue("Valid template", Template.isValid("Hello {{ name }}!"))
  assertTrue("Invalid template (unbalanced)", !Template.isValid("Hello {{ name }!"))
  assertTrue("Empty template is valid", Template.isValid(""))

  let vars = VariableStore.empty()
  let vars2 = VariableStore.set(vars, "name", Js.Json.string("World"))
  let result = Template.substitute("Hello {{ name }}!", vars2)
  assertEqual("Substitution works", "Hello World!", result)

  let result2 = Template.substitute("No variables here", vars2)
  assertEqual("No substitution needed", "No variables here", result2)
}

// Mill state tests
let testMillState = () => {
  Js.log("\n=== Mill State Tests ===")

  let state = createMillState()
  assertEqual("Initial card is 0", 0, state.currentCard)
  assertEqual("Initial stack depth is 0", 0, state.stackDepth)
  assertTrue("No initial error", !state.errorFlag)
}

// Operation card tests
let testOperationCard = () => {
  Js.log("\n=== Operation Card Tests ===")

  let state = createMillState()
  let vars = VariableStore.empty()

  // Test LoadVariable
  let card1: operationCard = {op: LoadVariable, operand1: "x", operand2: "", resultRef: 0}
  let (success1, _) = executeCard(card1, state, vars)
  assertTrue("LoadVariable succeeds", success1)
  assertEqual("Card advanced", 1, state.currentCard)

  // Test LoopStart
  let card2: operationCard = {op: LoopStart, operand1: "", operand2: "", resultRef: 0}
  let (success2, _) = executeCard(card2, state, vars)
  assertTrue("LoopStart succeeds", success2)
  assertEqual("Stack depth increased", 1, state.stackDepth)

  // Test LoopEnd
  let card3: operationCard = {op: LoopEnd, operand1: "", operand2: "", resultRef: 0}
  let (success3, _) = executeCard(card3, state, vars)
  assertTrue("LoopEnd succeeds", success3)
  assertEqual("Stack depth decreased", 0, state.stackDepth)

  // Test stack underflow
  let (success4, _) = executeCard(card3, state, vars)
  assertTrue("LoopEnd fails on underflow", !success4)
  assertTrue("Error flag set", state.errorFlag)
}

// Build tests
let testBuild = () => {
  Js.log("\n=== Build Tests ===")

  let config: siteConfig = {
    name: "Test Site",
    baseUrl: "https://example.com",
    language: "en",
    outputDir: "public",
    contentDir: "content",
    templateDir: "templates",
    theme: None,
    plugins: None,
    a11y: None,
  }

  let content: array<contentItem> = [
    {
      path: "test.md",
      frontmatter: {
        title: "Test Page",
        date: None,
        author: None,
        tags: None,
        draft: None,
        template: None,
        a11y: None,
      },
      body: "Hello {{ site.name }}!",
      outputPath: "public/test.html",
    },
  ]

  switch build(config, content) {
  | Success({outputFiles, duration}) => {
      assertTrue("Build succeeded", true)
      assertEqual("One output file", 1, Array.length(outputFiles))
      assertTrue("Duration is positive", duration >= 0.0)
    }
  | Error({message, _}) => {
      Js.log(`✗ Build failed: ${message}`)
    }
  }
}

// Run all tests
let runTests = () => {
  Js.log("NoteG SSG - Build System Tests")
  Js.log("==============================")

  testVariableStore()
  testTemplate()
  testMillState()
  testOperationCard()
  testBuild()

  Js.log("\n=== Tests Complete ===")
}

runTests()
