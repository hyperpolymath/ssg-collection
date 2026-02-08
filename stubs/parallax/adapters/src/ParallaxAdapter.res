// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * parallax-ssg MCP Adapter
 *
 * Connects parallax-ssg (Chapel) to the poly-ssg-mcp hub.
 * This is the ONLY place non-Chapel code is allowed in this satellite.
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

  let name = "parallax-ssg"
  let language = "Chapel"
  let description = "Parallel-first static site generator in Chapel for high-performance builds"

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
      let result = runCommand("chpl --version")
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
      name: "parallax_build",
      description: "Build the parallax-ssg site with parallel processing",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string", "description": "Path to site root" },
          "threads": { "type": "number", "description": "Number of parallel threads" }
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
          let result = runCommand("./parallax-ssg --build", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "parallax_compile",
      description: "Compile the Chapel source",
      inputSchema: %raw(`{ "type": "object", "properties": { "path": { "type": "string" } } }`),
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
          let result = runCommand("chpl src/parallel-press.chpl -o parallax-ssg", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "parallax_version",
      description: "Get parallax-ssg and Chapel version",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let result = runCommand("chpl --version")
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
