// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * wagasm-ssg MCP Adapter
 *
 * Connects wagasm-ssg (Pure WAT) to the poly-ssg-mcp hub.
 * This is the ONLY place non-WAT code is allowed in this satellite.
 *
 * EXPERIMENTAL: Proves that an SSG can be written in pure WebAssembly Text.
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

  let name = "wagasm-ssg"
  let language = "WebAssembly Text (WAT)"
  let description = "EXPERIMENTAL: Pure WebAssembly Text SSG proving WAT viability as a source language"

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
      // Check for WABT (WebAssembly Binary Toolkit) - wat2wasm
      let result = runCommand("wat2wasm --version")
      if result.success {
        state = Connected
        resolve(true)
      } else {
        // Also check deno for the host runtime
        let denoResult = runCommand("deno --version")
        state = denoResult.success ? Connected : Disconnected
        resolve(denoResult.success)
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
      name: "wagasm_compile",
      description: "Compile WAT to WASM binary",
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
          let result = runCommand("wat2wasm src/wagasm-ssg.wat -o src/wagasm-ssg.wasm", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "wagasm_build",
      description: "Build the wagasm-ssg site (compile WAT + run with Deno host)",
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
          // First compile WAT to WASM, then run with Deno host
          let result = runCommand(
            "wat2wasm src/wagasm-ssg.wat -o src/wagasm-ssg.wasm && deno run --allow-read --allow-write runtime/host.ts",
            ~cwd=Some(path),
          )
          resolve(result)
        })
      },
    },
    {
      name: "wagasm_validate",
      description: "Validate WAT syntax",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string" }
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
          let result = runCommand("wat2wasm --debug-names src/wagasm-ssg.wat -o /dev/null", ~cwd=Some(path))
          resolve(result)
        })
      },
    },
    {
      name: "wagasm_version",
      description: "Get wagasm-ssg, WABT, and Deno versions",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let watResult = runCommand("wat2wasm --version")
          let denoResult = runCommand("deno --version")
          resolve({
            success: true,
            stdout: `wagasm-ssg v0.1.0 (EXPERIMENTAL)\nWABT: ${watResult.stdout}\nDeno: ${denoResult.stdout}`,
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
