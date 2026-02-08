// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * terrapin-ssg MCP Adapter
 *
 * Connects terrapin-ssg (Logo) to the poly-ssg-mcp hub.
 * This is the ONLY place non-Logo code is allowed in this satellite.
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

  let name = "terrapin-ssg"
  let language = "Logo"
  let description = "Educational static site generator in Logo with turtle graphics for SVG generation"

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
      // Check for UCBLogo (the standard Logo interpreter)
      let result = runCommand("logo --version || ucblogo --version || echo 'Logo available'")
      state = Connected
      resolve(true)
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
      name: "terrapin_build",
      description: "Build the terrapin-ssg site using Logo turtle graphics",
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
          let result = runCommand("logo src/terrapin-ssg.logo", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "terrapin_draw",
      description: "Generate SVG artwork using turtle graphics",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string" },
          "pattern": { "type": "string", "enum": ["spiral", "square", "star", "tree"], "description": "Pattern to draw (spiral, square, star, tree)" }
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
          let rawPattern = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "pattern") {
            | Some(v) => Js.Json.decodeString(v)->Belt.Option.getWithDefault("spiral")
            | None => "spiral"
            }
          | None => "spiral"
          }
          // Security: Only allow whitelisted patterns to prevent command injection
          let allowedPatterns = ["spiral", "square", "star", "tree"]
          let pattern = if Js.Array.includes(rawPattern, allowedPatterns) {
            rawPattern
          } else {
            "spiral" // Default to safe pattern
          }
          let result = runCommand(`logo -e 'load "src/terrapin-ssg.logo draw_${pattern}'`, ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "terrapin_version",
      description: "Get terrapin-ssg and Logo interpreter version",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          resolve({
            success: true,
            stdout: "terrapin-ssg v0.1.0 - Logo SSG with turtle graphics\nUses UCBLogo interpreter",
            stderr: "",
            code: 0,
          })
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
