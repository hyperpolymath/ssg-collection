// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
// pharos-ssg - E2E Build Tests

// Deno test external
@val external test: (string, unit => promise<unit>) => unit = "Deno.test"

// Deno.stat for file existence checks
type denoStat
@val external stat: string => promise<denoStat> = "Deno.stat"

// Import assertExists from Deno std
@module("https://deno.land/std@0.208.0/assert/mod.ts")
external assertExists: denoStat => unit = "assertExists"

// Test helper
let testFileExists = async (name: string, path: string) => {
  test(name, async () => {
    let result = await stat(path)
    assertExists(result)
  })
}

// Build system tests
let _ = testFileExists("Build system - Justfile exists", "justfile")
let _ = testFileExists("Build system - Containerfile exists", "Containerfile")

// Pharo engine tests
let _ = testFileExists("Engine - Pharo sources exist", "engine/src/PharosSite.class.st")
let _ = testFileExists("Engine - Site builder exists", "engine/src/PharosBuilder.class.st")

// SSG ReScript module tests
let _ = testFileExists("SSG - Types exist", "ssg/src/Types.res")

// Accessibility tests
let _ = testFileExists("Accessibility - Schema exists", "a11y/schema.json")

// SCM file tests
let _ = testFileExists("SCM - ECOSYSTEM.scm exists", "ECOSYSTEM.scm")
let _ = testFileExists("SCM - META.scm exists", "META.scm")
let _ = testFileExists("SCM - STATE.scm exists", "STATE.scm")
