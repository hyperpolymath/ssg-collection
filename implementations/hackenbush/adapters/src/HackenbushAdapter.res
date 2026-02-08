// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * hackenbush-ssg MCP Adapter
 *
 * Connects hackenbush-ssg to the poly-ssg-mcp hub.
 * This is the ONLY place non-native code is allowed in this satellite.
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

  let name = "hackenbush-ssg"
  let language = "Game of Life / Cellular Automata"
  let description = "Static site generator using Conway's Game of Life patterns for generative content"

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
      let result = runCommand("node --version")
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
      name: "hackenbush_build",
      description: "Build the hackenbush-ssg site using Game of Life patterns",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string", "description": "Path to site root" },
          "pattern": { "type": "string", "description": "Initial Life pattern (e.g., glider, blinker, pulsar)" }
        }
      }`),
      execute: (params) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let path = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "path") {
            | Some(v) =>
              switch Js.Json.decodeString(v) {
              | Some(s) => s
              | None => "."
              }
            | None => "."
            }
          | None => "."
          }
          let result = runCommand("node src/main.js build", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "hackenbush_generate",
      description: "Generate content from a Game of Life pattern",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string", "description": "Path to site root" },
          "generations": { "type": "number", "description": "Number of generations to simulate" },
          "width": { "type": "number", "description": "Grid width" },
          "height": { "type": "number", "description": "Grid height" }
        }
      }`),
      execute: (params) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let path = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "path") {
            | Some(v) =>
              switch Js.Json.decodeString(v) {
              | Some(s) => s
              | None => "."
              }
            | None => "."
            }
          | None => "."
          }
          let result = runCommand("node src/life.js generate", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "hackenbush_version",
      description: "Get hackenbush-ssg version",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          resolve({success: true, stdout: "hackenbush-ssg v0.1.0", stderr: "", code: 0})
        })
      },
    },
  ]
}

// Export for MCP hub
let name = Adapter.name
let language = Adapter.language
let description = Adapter.description
let connect = Adapter.connect
let disconnect = Adapter.disconnect
let isConnected = Adapter.isConnected
let tools = Adapter.tools
