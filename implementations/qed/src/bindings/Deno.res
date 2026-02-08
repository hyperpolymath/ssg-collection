// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Deno runtime bindings for ReScript

// Deno namespace bindings
module DirEntry = {
  type t = {
    name: string,
    isFile: bool,
    isDirectory: bool,
    isSymlink: bool,
  }
}

module MemoryUsage = {
  type t = {
    rss: float,
    heapTotal: float,
    heapUsed: float,
    external_: float,
  }
}

module CommandOutput = {
  type t = {
    success: bool,
    code: int,
    stdout: Js.TypedArray2.Uint8Array.t,
    stderr: Js.TypedArray2.Uint8Array.t,
  }
}

module Command = {
  type t

  type options = {
    args?: array<string>,
    cwd?: string,
    env?: Js.Dict.t<string>,
    stdin?: string,
    stdout?: string,
    stderr?: string,
  }

  @new external make: (string, options) => t = "Deno.Command"
  @send external output: t => promise<CommandOutput.t> = "output"
}

module Build = {
  type t = {
    target: string,
    arch: string,
    os: string,
  }

  @val external build: t = "Deno.build"
}

module Version = {
  type t = {
    deno: string,
    v8: string,
    typescript: string,
  }

  @val external version: t = "Deno.version"
}

// File system operations
@val external readDir: string => Js.AsyncIterator.t<DirEntry.t> = "Deno.readDir"
@val external readTextFile: string => promise<string> = "Deno.readTextFile"
@val external writeTextFile: (string, string) => promise<unit> = "Deno.writeTextFile"
@val external mkdir: (string, {"recursive": bool}) => promise<unit> = "Deno.mkdir"
@val external remove: (string, {"recursive": bool}) => promise<unit> = "Deno.remove"
@val external stat: string => promise<'a> = "Deno.stat"

// Memory
@val external memoryUsage: unit => MemoryUsage.t = "Deno.memoryUsage"

// TextEncoder/TextDecoder
module TextEncoder = {
  type t

  @new external make: unit => t = "TextEncoder"
  @send external encode: (t, string) => Js.TypedArray2.Uint8Array.t = "encode"
}

module TextDecoder = {
  type t

  @new external make: unit => t = "TextDecoder"
  @send external decode: (t, Js.TypedArray2.Uint8Array.t) => string = "decode"
}

// Performance
module Performance = {
  @val external now: unit => float = "performance.now"
}

// Helper to iterate async iterator
let asyncIteratorToArray = async (iterator: Js.AsyncIterator.t<'a>): array<'a> => {
  let results = []
  let rec loop = async () => {
    let next = await iterator->Js.AsyncIterator.next
    switch next {
    | {done: true, value: _} => ()
    | {done: false, value: Some(v)} =>
      results->Js.Array2.push(v)->ignore
      await loop()
    | {done: false, value: None} => await loop()
    }
  }
  await loop()
  results
}

// Read directory as array
let readDirAsArray = async (path: string): array<DirEntry.t> => {
  let iterator = readDir(path)
  await asyncIteratorToArray(iterator)
}
