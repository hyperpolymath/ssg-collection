// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// ReScript bindings for Deno runtime APIs

module Test = {
  type testFn = unit => promise<unit>
  type testOptions = {
    name: string,
    fn: testFn,
    ignore?: bool,
    only?: bool,
    sanitizeOps?: bool,
    sanitizeResources?: bool,
    sanitizeExit?: bool,
  }

  @val @scope("Deno") external test: testOptions => unit = "test"
  @val @scope("Deno") external testSimple: (string, testFn) => unit = "test"
}

module Assert = {
  @module("@std/assert") external assertEquals: ('a, 'a) => unit = "assertEquals"
  @module("@std/assert") external assertExists: 'a => unit = "assertExists"
  @module("@std/assert") external assertRejects: (unit => promise<'a>) => promise<unit> = "assertRejects"
  @module("@std/assert") external assertThrows: (unit => 'a) => unit = "assertThrows"
  @module("@std/assert") external assertNotEquals: ('a, 'a) => unit = "assertNotEquals"
  @module("@std/assert") external assertStrictEquals: ('a, 'a) => unit = "assertStrictEquals"
}

module Fs = {
  type dirEntry = {
    name: string,
    isFile: bool,
    isDirectory: bool,
    isSymlink: bool,
  }

  @val @scope("Deno") external readTextFile: string => promise<string> = "readTextFile"
  @val @scope("Deno") external writeTextFile: (string, string) => promise<unit> = "writeTextFile"
  @val @scope("Deno") external readDir: string => Js.Array2.array_like<dirEntry> = "readDirSync"
  @val @scope("Deno") external stat: string => promise<{..}> = "stat"
}

module Meta = {
  @val @scope(("import", "meta")) external main: bool = "main"
  @val @scope(("import", "meta")) external url: string = "url"
}

module Performance = {
  @val @scope("performance") external now: unit => float = "now"
}
