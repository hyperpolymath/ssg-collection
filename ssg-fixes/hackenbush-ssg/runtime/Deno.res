// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Deno API bindings for ReScript

// Deno.args
@val external args: array<string> = "Deno.args"

// Deno.readTextFile
@val external readTextFile: string => promise<string> = "Deno.readTextFile"

// Deno.writeFile
@val external writeFile: (string, Js.TypedArray2.Uint8Array.t) => promise<unit> = "Deno.writeFile"

// Deno.writeTextFile
@val external writeTextFile: (string, string) => promise<unit> = "Deno.writeTextFile"

// Deno.mkdir
type mkdirOptions = {recursive: bool}
@val external mkdir: (string, mkdirOptions) => promise<unit> = "Deno.mkdir"

// Console logging
@val external log: string => unit = "console.log"
@val external logInt: int => unit = "console.log"
