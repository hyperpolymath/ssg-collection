// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
// Deno API bindings for ReScript

// File operations
@val external readFileSync: string => Js.TypedArray2.Uint8Array.t = "Deno.readFileSync"
@val external writeTextFileSync: (string, string) => unit = "Deno.writeTextFileSync"
@val external mkdirSync: (string, {..}) => unit = "Deno.mkdirSync"

// Console logging
@val external log: string => unit = "console.log"
