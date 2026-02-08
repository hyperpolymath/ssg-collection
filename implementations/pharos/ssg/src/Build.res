// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG SSG - Mill-Based Build System

open Types

// Mill state management
let createMillState = (): millState => {
  currentCard: 0,
  stackDepth: 0,
  errorFlag: false,
  errorMessage: "",
}

// Variable store (functional approach)
module VariableStore = {
  type t = Js.Dict.t<Js.Json.t>

  let empty = (): t => Js.Dict.empty()

  let set = (store: t, key: string, value: Js.Json.t): t => {
    let newStore = Js.Dict.fromArray(Js.Dict.entries(store))
    Js.Dict.set(newStore, key, value)
    newStore
  }

  let get = (store: t, key: string): option<Js.Json.t> => {
    Js.Dict.get(store, key)
  }

  let has = (store: t, key: string): bool => {
    switch Js.Dict.get(store, key) {
    | Some(_) => true
    | None => false
    }
  }
}

// Template substitution engine
module Template = {
  // Regex for {{ variable }} patterns
  let variablePattern = %re("/\{\{\s*([a-zA-Z_][a-zA-Z0-9_.]*)\s*\}\}/g")

  let substitute = (template: string, variables: VariableStore.t): string => {
    let result = ref(template)

    // Find all variable references
    let rec processMatches = () => {
      switch Js.Re.exec_(variablePattern, result.contents) {
      | Some(match_) => {
          let fullMatch = Js.Re.Match.fullMatch(match_)
          let captures = Js.Re.Match.captures(match_)

          switch captures[1] {
          | Some(varName) => {
              let varNameStr = Js.Nullable.toOption(varName)
              switch varNameStr {
              | Some(name) => {
                  switch VariableStore.get(variables, name) {
                  | Some(value) => {
                      let valueStr = switch Js.Json.classify(value) {
                      | Js.Json.JSONString(s) => s
                      | Js.Json.JSONNumber(n) => Js.Float.toString(n)
                      | Js.Json.JSONTrue => "true"
                      | Js.Json.JSONFalse => "false"
                      | _ => Js.Json.stringify(value)
                      }
                      result := Js.String2.replace(result.contents, fullMatch, valueStr)
                    }
                  | None => ()
                  }
                }
              | None => ()
              }
            }
          | None => ()
          }
          processMatches()
        }
      | None => ()
      }
    }

    processMatches()
    result.contents
  }

  let isValid = (template: string): bool => {
    let openCount = ref(0)
    let closeCount = ref(0)
    let len = Js.String2.length(template)

    for i in 0 to len - 2 {
      let twoChars = Js.String2.slice(template, ~start=i, ~end_=i + 2)
      if twoChars == "{{" {
        openCount := openCount.contents + 1
      } else if twoChars == "}}" {
        closeCount := closeCount.contents + 1
      }
    }

    openCount.contents == closeCount.contents
  }
}

// Operation card execution
let executeCard = (card: operationCard, state: millState, variables: VariableStore.t): (bool, VariableStore.t) => {
  switch card.op {
  | LoadVariable => {
      state.currentCard = state.currentCard + 1
      (true, variables)
    }
  | StoreVariable => {
      state.currentCard = state.currentCard + 1
      let newVars = VariableStore.set(variables, card.operand1, Js.Json.string(card.operand2))
      (true, newVars)
    }
  | TemplateSubstitute => {
      state.currentCard = state.currentCard + 1
      (true, variables)
    }
  | ContentTransform => {
      state.currentCard = state.currentCard + 1
      (true, variables)
    }
  | OutputGenerate => {
      state.currentCard = state.currentCard + 1
      (true, variables)
    }
  | ConditionalBranch => {
      state.currentCard = state.currentCard + 1
      (true, variables)
    }
  | LoopStart => {
      if state.stackDepth < 32 {
        state.stackDepth = state.stackDepth + 1
        state.currentCard = state.currentCard + 1
        (true, variables)
      } else {
        state.errorFlag = true
        state.errorMessage = "Maximum loop depth exceeded"
        (false, variables)
      }
    }
  | LoopEnd => {
      if state.stackDepth > 0 {
        state.stackDepth = state.stackDepth - 1
        state.currentCard = state.currentCard + 1
        (true, variables)
      } else {
        state.errorFlag = true
        state.errorMessage = "Stack underflow"
        (false, variables)
      }
    }
  }
}

// Build pipeline
let build = (config: siteConfig, content: array<contentItem>): buildResult => {
  let startTime = Js.Date.now()
  let state = createMillState()
  let variables = ref(VariableStore.empty())

  // Set site variables
  variables := VariableStore.set(variables.contents, "site.name", Js.Json.string(config.name))
  variables := VariableStore.set(variables.contents, "site.baseUrl", Js.Json.string(config.baseUrl))

  let outputFiles = []

  // Process each content item
  content->Array.forEach(item => {
    if !state.errorFlag {
      // Set page variables
      variables := VariableStore.set(variables.contents, "page.title", Js.Json.string(item.frontmatter.title))

      // Apply template substitution
      let output = Template.substitute(item.body, variables.contents)

      // Add to output files
      let _ = Js.Array2.push(outputFiles, item.outputPath)
      ()
    }
  })

  let endTime = Js.Date.now()
  let duration = (endTime -. startTime) /. 1000.0

  if state.errorFlag {
    Error({message: state.errorMessage, file: None, line: None})
  } else {
    Success({outputFiles, duration})
  }
}
