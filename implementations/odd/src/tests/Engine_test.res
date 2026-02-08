// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Engine Tests
 * Tests for Mill-Based Synthesis Engine
 */

open Core

// Test utilities
@module("./test_utils.js")
external assertEquals: ('a, 'a) => unit = "assertEquals"

@module("./test_utils.js")
external assertExists: 'a => unit = "assertExists"

@module("./test_utils.js")
external describe: (string, unit => unit) => unit = "describe"

@module("./test_utils.js")
external it: (string, unit => promise<unit>) => unit = "it"

let runTests = () => {
  describe("Mill", () => {
    it("should create a mill with idle status", async () => {
      let mill = createMill()
      assertEquals(mill.status, Idle)
      assertEquals(mill.accumulator, Js.Json.null)
    })

    it("should execute load operation", async () => {
      let mill = createMill()
      Belt.MutableMap.String.set(mill.registers, "test", Js.Json.number(42.0))

      let card: operationCard = {
        operation: Load,
        operands: ["test"],
        metadata: None,
      }

      let _ = await executeMill(mill, card)
      assertEquals(mill.accumulator, Js.Json.number(42.0))
      assertEquals(mill.status, Idle)
    })

    it("should reset to initial state", async () => {
      let mill = createMill()
      Belt.MutableMap.String.set(mill.registers, "test", Js.Json.number(123.0))
      let _ = await executeMill(mill, {operation: Load, operands: ["test"], metadata: None})

      resetMill(mill)

      assertEquals(mill.accumulator, Js.Json.null)
      assertEquals(Belt.MutableMap.String.size(mill.registers), 0)
      assertEquals(mill.status, Idle)
    })
  })

  describe("Store", () => {
    it("should create an empty store", () => {
      let store = createStore()
      assertEquals(Belt.MutableMap.String.size(store.variables), 0)
    })

    it("should save and load variables", () => {
      let store = createStore()
      let card: variableCard = {
        name: "greeting",
        type_: String,
        value: Js.Json.string("Hello"),
        readonly: false,
      }

      saveToStore(store, card)
      let loaded = loadFromStore(store, "greeting")

      assertExists(loaded)
      switch loaded {
      | Some(v) => assertEquals(v.value, Js.Json.string("Hello"))
      | None => ()
      }
    })

    it("should return None for non-existent variables", () => {
      let store = createStore()
      let result = loadFromStore(store, "nonexistent")
      assertEquals(result, None)
    })

    it("should clear all variables", () => {
      let store = createStore()
      saveToStore(store, {name: "a", type_: String, value: Js.Json.string("1"), readonly: false})
      saveToStore(store, {name: "b", type_: String, value: Js.Json.string("2"), readonly: false})

      clearStore(store)

      assertEquals(Belt.MutableMap.String.size(store.variables), 0)
    })
  })

  describe("Engine", () => {
    it("should create engine with default config", () => {
      let engine = createEngine()
      let state = getState(engine)

      assertEquals(state["mill"], Idle)
      assertEquals(state["variables"], 0)
    })

    it("should load variables into store", () => {
      let engine = createEngine()
      loadVariables(engine, [
        {name: "x", type_: Number, value: Js.Json.number(1.0), readonly: false},
        {name: "y", type_: Number, value: Js.Json.number(2.0), readonly: false},
      ])

      let state = getState(engine)
      assertEquals(state["variables"], 2)
    })

    it("should reset engine state", () => {
      let engine = createEngine()
      loadVariables(engine, [
        {name: "test", type_: String, value: Js.Json.string("data"), readonly: false},
      ])

      resetEngine(engine)

      let state = getState(engine)
      assertEquals(state["variables"], 0)
      assertEquals(state["mill"], Idle)
    })
  })
}
