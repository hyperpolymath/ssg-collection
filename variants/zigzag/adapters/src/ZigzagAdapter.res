// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * zigzag-ssg MCP Adapter
 *
 * Connects zigzag-ssg (Zig) to the poly-ssg-mcp hub.
 * This is the ONLY place non-Zig code is allowed in this satellite.
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

  let name = "zigzag-ssg"
  let language = "Zig"
  let description = "High-performance static site generator in Zig with zero-overhead abstractions"

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
      let result = runCommand("zig version")
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
      name: "zigzag_build",
      description: "Build the zigzag-ssg site",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string", "description": "Path to site root" },
          "release": { "type": "boolean", "description": "Build in release mode" }
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
          let release = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "release") {
            | Some(v) => Js.Json.decodeBoolean(v)->Belt.Option.getWithDefault(false)
            | None => false
            }
          | None => false
          }
          let mode = release ? "-Doptimize=ReleaseFast" : ""
          let result = runCommand(`zig build ${mode}`, ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "zigzag_run",
      description: "Build and run the zigzag-ssg",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string" },
          "args": { "type": "array", "items": { "type": "string" } }
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
          let result = runCommand("zig build run", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "zigzag_test",
      description: "Run zigzag-ssg tests",
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
          let result = runCommand("zig build test", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "zigzag_version",
      description: "Get zigzag-ssg and Zig version",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let result = runCommand("zig version")
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
