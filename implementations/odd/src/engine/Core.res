// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Core Engine - odd-ssg
 * Mill-Based Synthesis Engine for Static Site Generation
 *
 * Implements the Analytical Engine paradigm:
 * - Operation Cards: Template instructions
 * - Variable Cards: Data bindings
 * - Number Cards: Content values
 * - Mill: Processing unit
 * - Store: Variable persistence
 */

module Dict = Belt.MutableMap.String

type operation = Load | Store | Transform | Emit | Branch | Loop

type operationCard = {
  operation: operation,
  operands: array<string>,
  metadata: option<Js.Dict.t<Js.Json.t>>,
}

type variableType = String | Number | Boolean | Array | Object

type variableCard = {
  name: string,
  type_: variableType,
  value: Js.Json.t,
  readonly: bool,
}

type numberCard = {
  address: int,
  value: Js.Json.t,
  precision: option<int>,
}

type millStatus = Idle | Running | Halted | Error

type mill = {
  mutable accumulator: Js.Json.t,
  registers: Dict.t<Js.Json.t>,
  mutable status: millStatus,
}

type store = {
  variables: Dict.t<variableCard>,
}

let createMill = (): mill => {
  accumulator: Js.Json.null,
  registers: Dict.make(),
  status: Idle,
}

let executeMill = async (mill: mill, card: operationCard): Js.Json.t => {
  mill.status = Running
  try {
    switch card.operation {
    | Load =>
      switch card.operands->Belt.Array.get(0) {
      | Some(key) =>
        switch Dict.get(mill.registers, key) {
        | Some(v) => mill.accumulator = v
        | None => ()
        }
      | None => ()
      }
    | Store =>
      switch card.operands->Belt.Array.get(0) {
      | Some(key) => Dict.set(mill.registers, key, mill.accumulator)
      | None => ()
      }
    | Transform =>
      switch card.metadata {
      | Some(meta) =>
        switch Js.Dict.get(meta, "transform") {
        | Some(_fn) => () // Transform function would be applied here
        | None => ()
        }
      | None => ()
      }
    | Emit => ()
    | Branch | Loop => ()
    }
    mill.status = Idle
    mill.accumulator
  } catch {
  | _ =>
    mill.status = Error
    raise(Js.Exn.raiseError("Mill execution error"))
  }
}

let resetMill = (mill: mill): unit => {
  mill.accumulator = Js.Json.null
  Dict.clear(mill.registers)
  mill.status = Idle
}

let createStore = (): store => {
  variables: Dict.make(),
}

let loadFromStore = (store: store, name: string): option<variableCard> => {
  Dict.get(store.variables, name)
}

let saveToStore = (store: store, card: variableCard): unit => {
  switch Dict.get(store.variables, card.name) {
  | Some(existing) if existing.readonly =>
    raise(Js.Exn.raiseError(`Cannot modify readonly variable: ${card.name}`))
  | _ => Dict.set(store.variables, card.name, card)
  }
}

let clearStore = (store: store): unit => {
  Dict.clear(store.variables)
}

type engineConfig = {
  strict: bool,
  maxIterations: int,
  timeout: int,
}

let defaultConfig: engineConfig = {
  strict: true,
  maxIterations: 10000,
  timeout: 30000,
}

type engine = {
  mill: mill,
  store: store,
  config: engineConfig,
}

let createEngine = (~config=defaultConfig, ()): engine => {
  mill: createMill(),
  store: createStore(),
  config,
}

let executeEngine = async (engine: engine, cards: array<operationCard>): array<Js.Json.t> => {
  let results = []
  let iterations = ref(0)

  for i in 0 to Array.length(cards) - 1 {
    iterations := iterations.contents + 1
    if iterations.contents > engine.config.maxIterations {
      raise(Js.Exn.raiseError("Max iterations exceeded"))
    }
    switch cards->Belt.Array.get(i) {
    | Some(card) =>
      let result = await executeMill(engine.mill, card)
      Js.Array2.push(results, result)->ignore
    | None => ()
    }
  }

  results
}

let loadVariables = (engine: engine, variables: array<variableCard>): unit => {
  variables->Belt.Array.forEach(v => saveToStore(engine.store, v))
}

let getState = (engine: engine): {"mill": millStatus, "variables": int} => {
  {
    "mill": engine.mill.status,
    "variables": Dict.size(engine.store.variables),
  }
}

let resetEngine = (engine: engine): unit => {
  resetMill(engine.mill)
  clearStore(engine.store)
}
