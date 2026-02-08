// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Deno test framework bindings for ReScript

type testOptions = {
  name: string,
  ignore?: bool,
  only?: bool,
  sanitizeOps?: bool,
  sanitizeResources?: bool,
  sanitizeExit?: bool,
}

// Basic test function
@val external test: (string, unit => promise<unit>) => unit = "Deno.test"

// Test with options
@val external testWithOptions: (testOptions, unit => promise<unit>) => unit = "Deno.test"

// Synchronous test
@val external testSync: (string, unit => unit) => unit = "Deno.test"

// Assertions
module Assert = {
  @module("https://deno.land/std@0.210.0/assert/mod.ts")
  external assertEquals: ('a, 'a, ~msg: string=?) => unit = "assertEquals"

  @module("https://deno.land/std@0.210.0/assert/mod.ts")
  external assertExists: ('a, ~msg: string=?) => unit = "assertExists"

  @module("https://deno.land/std@0.210.0/assert/mod.ts")
  external assertStringIncludes: (string, string, ~msg: string=?) => unit = "assertStringIncludes"

  @module("https://deno.land/std@0.210.0/assert/mod.ts")
  external assertRejects: (unit => promise<'a>) => promise<unit> = "assertRejects"

  @module("https://deno.land/std@0.210.0/assert/mod.ts")
  external assertThrows: (unit => 'a) => unit = "assertThrows"

  @module("https://deno.land/std@0.210.0/assert/mod.ts")
  external assert_: (bool, ~msg: string=?) => unit = "assert"
}

// FS helpers
module Fs = {
  @module("https://deno.land/std@0.210.0/fs/mod.ts")
  external exists: string => promise<bool> = "exists"
}
