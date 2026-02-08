// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Deno.res - ReScript bindings for Deno runtime APIs

// Environment
module Env = {
  @scope(("Deno", "env")) @val
  external get: string => option<string> = "get"

  @scope(("Deno", "env")) @val
  external set: (string, string) => unit = "set"
}

// File system directory entry
type dirEntry = {
  name: string,
  isFile: bool,
  isDirectory: bool,
  isSymlink: bool,
}

// Async iterator for directory reading
type asyncIterable<'a>

@send
external asyncIteratorSymbol: asyncIterable<'a> => Js.Nullable.t<'a> = "@@asyncIterator"

// Read directory (returns async iterable)
@scope("Deno") @val
external readDir: string => asyncIterable<dirEntry> = "readDir"

// Text encoder/decoder
type textEncoder
type textDecoder

@new
external makeTextEncoder: unit => textEncoder = "TextEncoder"

@new
external makeTextDecoder: unit => textDecoder = "TextDecoder"

@send
external encode: (textEncoder, string) => Js.TypedArray2.Uint8Array.t = "encode"

@send
external decode: (textDecoder, Js.TypedArray2.Uint8Array.t) => string = "decode"

// Args
@scope("Deno") @val
external args: array<string> = "args"

// Exit
@scope("Deno") @val
external exit: int => unit = "exit"

// Stdin/Stdout
type readable<'a>
type writable<'a>

type stdin = {readable: readable<Js.TypedArray2.Uint8Array.t>}
type stdout = {writable: writable<Js.TypedArray2.Uint8Array.t>}

@scope("Deno") @val
external stdin: stdin = "stdin"

@scope("Deno") @val
external stdout: stdout = "stdout"

@send
external write: (stdout, Js.TypedArray2.Uint8Array.t) => promise<int> = "write"

// File operations
@scope("Deno") @val
external readTextFile: string => promise<string> = "readTextFile"

@scope("Deno") @val
external writeTextFile: (string, string) => promise<unit> = "writeTextFile"

// Network
type conn
type listener = {
  accept: unit => promise<conn>,
}
type httpConn
type requestEvent = {
  request: Fetch.Request.t,
  respondWith: Fetch.Response.t => promise<unit>,
}

type listenOptions = {
  port: int,
  hostname: string,
}

@scope("Deno") @val
external listen: listenOptions => listener = "listen"

@scope("Deno") @val
external serveHttp: conn => httpConn = "serveHttp"

// Dynamic import
@val
external import_: string => promise<'a> = "import"

// Meta
type importMeta = {main: bool}
@scope("import") @val
external meta: importMeta = "meta"
