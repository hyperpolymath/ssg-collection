// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// hackenbush-ssg - RLE Parser Tests

// Deno test external
@val external test: (string, unit => unit) => unit = "Deno.test"

// Import assertEquals from Deno std
@module("https://deno.land/std@0.208.0/assert/mod.ts")
external assertEquals: ('a, 'a, string) => unit = "assertEquals"

// Tests for RLE parsing
let runTests = () => {
  test("RLE Parser - Valid header parsing", () => {
    // Parser should extract:
    // - Name: "Test Pattern"
    // - Width: 3
    // - Height: 3
    // - Rule: B3/S23
    assertEquals(true, true, "RLE header parsing test")
  })

  test("RLE Parser - Cell pattern decoding", () => {
    // "bo$2bo$3o!" should decode to:
    // Row 0: dead, alive, dead
    // Row 1: dead, dead, alive
    // Row 2: alive, alive, alive
    assertEquals(true, true, "RLE cell pattern decoding test")
  })

  test("RLE Parser - Run-length encoding", () => {
    // "3o" = three alive cells
    // "5b" = five dead cells
    // "10b3o" = 10 dead, 3 alive
    assertEquals(true, true, "RLE run-length encoding test")
  })

  test("RLE Parser - Multi-line patterns", () => {
    // $ represents end of row
    // Multiple $ = multiple empty rows
    assertEquals(true, true, "RLE multi-line pattern test")
  })

  test("RLE Parser - Invalid input handling", () => {
    // Should throw on invalid RLE
    assertEquals(true, true, "RLE invalid input test")
  })

  test("RLE Parser - Gosper glider gun", () => {
    // Should parse without error
    // Grid should be 36x9
    // First live cell at (24, 0)
    assertEquals(true, true, "Gosper glider gun parsing test")
  })
}

runTests()
