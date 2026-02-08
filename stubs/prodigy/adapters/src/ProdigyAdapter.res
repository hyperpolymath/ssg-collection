// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * prodigy-ssg MCP Adapter
 *
 * Connects prodigy-ssg (Prolog) to the poly-ssg-mcp hub.
 * This is the ONLY place non-Prolog code is allowed in this satellite.
 */

module Adapter = {
  type connectionState = Connected | Disconnected

  type commandResult = {
    success: bool,
    stdout: string,
    stderr: string,
    code: int,
  }

  type tool = {
    name: string,
    description: string,
    inputSchema: Js.Json.t,
    execute: Js.Json.t => Js.Promise.t<commandResult>,
  }

  let name = "prodigy-ssg"
  let language = "Prolog"
  let description = "Logic-based static site generator in Prolog with declarative site definitions"

  let mutable state: connectionState = Disconnected

  @module("child_process")
  external execSync: (string, 'options) => string = "execSync"

  let runCommand = (cmd: string, ~cwd: option<string>=?): commandResult => {
    try {
      let options = switch cwd {
      | Some(dir) => {"cwd": dir, "encoding": "utf-8"}
      | None => {"encoding": "utf-8"}
      }
      let stdout = execSync(cmd, options)
      {success: true, stdout, stderr: "", code: 0}
    } catch {
    | Js.Exn.Error(e) =>
      let message = switch Js.Exn.message(e) {
      | Some(m) => m
      | None => "Unknown error"
      }
      {success: false, stdout: "", stderr: message, code: 1}
    }
  }

  let connect = (): Js.Promise.t<bool> => {
    Js.Promise.make((~resolve, ~reject as _) => {
      let result = runCommand("swipl --version")
      if result.success {
        state = Connected
        resolve(true)
      } else {
        state = Disconnected
        resolve(false)
      }
    })
  }

  let disconnect = (): Js.Promise.t<unit> => {
    Js.Promise.make((~resolve, ~reject as _) => {
      state = Disconnected
      resolve()
    })
  }

  let isConnected = (): bool => {
    switch state {
    | Connected => true
    | Disconnected => false
    }
  }

  let tools: array<tool> = [
    {
      name: "prodigy_build",
      description: "Build the prodigy-ssg site using Prolog rules",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string", "description": "Path to site root" }
        }
      }`),
      execute: (params) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let path = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "path") {
            | Some(v) => Js.Json.decodeString(v)->Belt.Option.getWithDefault(".")
            | None => "."
            }
          | None => "."
          }
          let result = runCommand("swipl -g build -t halt src/prodigy.pl", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "prodigy_query",
      description: "Query site metadata using Prolog",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string" },
          "query": { "type": "string", "description": "Prolog query (alphanumeric, underscores, parens, commas only)" }
        }
      }`),
      execute: (params) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let path = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "path") {
            | Some(v) => Js.Json.decodeString(v)->Belt.Option.getWithDefault(".")
            | None => "."
            }
          | None => "."
          }
          let rawQuery = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "query") {
            | Some(v) => Js.Json.decodeString(v)->Belt.Option.getWithDefault("true")
            | None => "true"
            }
          | None => "true"
          }
          // Security: Reject queries with shell metacharacters to prevent command injection
          let hasShellMetachars =
            Js.String2.includes(rawQuery, "'") ||
            Js.String2.includes(rawQuery, "\"") ||
            Js.String2.includes(rawQuery, "`") ||
            Js.String2.includes(rawQuery, "$") ||
            Js.String2.includes(rawQuery, ";") ||
            Js.String2.includes(rawQuery, "|") ||
            Js.String2.includes(rawQuery, "&") ||
            Js.String2.includes(rawQuery, ">") ||
            Js.String2.includes(rawQuery, "<") ||
            Js.String2.includes(rawQuery, "\\")
          if hasShellMetachars {
            resolve({success: false, stdout: "", stderr: "Invalid query: contains disallowed shell characters", code: 1})
          } else {
            let result = runCommand(`swipl -g '${rawQuery}' -t halt src/prodigy.pl`, ~cwd=Some(path))
            resolve(result)
          }
        })
      },
    },
    {
      name: "prodigy_version",
      description: "Get prodigy-ssg and SWI-Prolog version",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let result = runCommand("swipl --version")
          resolve(result)
        })
      },
    },
  ]
}

let name = Adapter.name
let language = Adapter.language
let description = Adapter.description
let connect = Adapter.connect
let disconnect = Adapter.disconnect
let isConnected = Adapter.isConnected
let tools = Adapter.tools
