// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Config.res - Server configuration

open Types
open Bindings.Deno

let parseIntOr = (s: option<string>, default: int): int => {
  switch s {
  | Some(str) =>
    switch Belt.Int.fromString(str) {
    | Some(n) => n
    | None => default
    }
  | None => default
  }
}

let getOrDefault = (opt: option<string>, default: string): string => {
  switch opt {
  | Some(s) => s
  | None => default
  }
}

let make = (): config => {
  {
    port: parseIntOr(Env.get("RATS_PORT"), 3000),
    host: getOrDefault(Env.get("RATS_HOST"), "127.0.0.1"),
    logLevel: getOrDefault(Env.get("RATS_LOG_LEVEL"), "info"),
  }
}
