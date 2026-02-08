// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * rescribe-ssg MCP Adapter
 *
 * Connects rescribe-ssg (ReScript) to the poly-ssg-mcp hub.
 * NOTE: This adapter is ALSO in ReScript - the satellite is 100% ReScript.
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

  let name = "rescribe-ssg"
  let language = "ReScript"
  let description = "Type-safe static site generator in ReScript with JavaScript interop"

  let state: ref<connectionState> = ref(Disconnected)

  @module("child_process")
  external execSync: (string, 'options) => string = "execSync"

  let runCommand = (cmd: string, ~cwd: option<string>=?): commandResult => {
    try {
      let options = {"cwd": Belt.Option.getWithDefault(cwd, "."), "encoding": "utf-8"}
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
      let result = runCommand("npx rescript --version")
      if result.success {
        state := Connected
        resolve(true)
      } else {
        state := Disconnected
        resolve(false)
      }
    })
  }

  let disconnect = (): Js.Promise.t<unit> => {
    Js.Promise.make((~resolve, ~reject as _) => {
      state := Disconnected
      resolve()
    })
  }

  let isConnected = (): bool => {
    switch state.contents {
    | Connected => true
    | Disconnected => false
    }
  }

  let tools: array<tool> = [
    {
      name: "rescribe_build",
      description: "Build the rescribe-ssg site",
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
          let result = runCommand("npm run build && node lib/es6/src/RescribeSsg.mjs build", ~cwd=path)
          resolve(result)
        })
      },
    },
    {
      name: "rescribe_compile",
      description: "Compile ReScript to JavaScript",
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
          let result = runCommand("npx rescript", ~cwd=path)
          resolve(result)
        })
      },
    },
    {
      name: "rescribe_clean",
      description: "Clean build artifacts",
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
          let result = runCommand("npx rescript clean", ~cwd=path)
          resolve(result)
        })
      },
    },
    {
      name: "rescribe_version",
      description: "Get rescribe-ssg and ReScript version",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let result = runCommand("npx rescript --version")
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
