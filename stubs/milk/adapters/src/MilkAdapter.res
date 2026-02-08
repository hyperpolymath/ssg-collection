// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * milk-ssg MCP Adapter
 *
 * Connects milk-ssg (COW esoteric language) to the poly-ssg-mcp hub.
 * This is the ONLY place non-COW code is allowed in this satellite.
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

  let name = "milk-ssg"
  let language = "COW"
  let description = "Esoteric static site generator in COW - proving SSGs can be written in any language"

  let mutable state: connectionState = Disconnected

  @module("child_process")
  external execSync: (string, 'options) => string = "execSync"

  // Security: Sanitize path input to prevent command injection
  let sanitizePath = (input: string): option<string> => {
    // Only allow alphanumeric, dots, hyphens, underscores, and path separators
    let validPattern = %re("/^[a-zA-Z0-9._\-\/]+$/")
    if Js.Re.test_(validPattern, input) && !Js.String2.includes(input, "..") {
      Some(input)
    } else {
      None
    }
  }

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

  // COW interpreter check - uses a COW interpreter or the built-in one
  let connect = (): Js.Promise.t<bool> => {
    Js.Promise.make((~resolve, ~reject as _) => {
      // Check if cow interpreter exists, or use our bundled one
      let result = runCommand("which cow || echo 'using bundled interpreter'")
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
      name: "milk_build",
      description: "Build the milk-ssg site by interpreting COW code",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string", "description": "Path to site root" }
        }
      }`),
      execute: (params) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let rawPath = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "path") {
            | Some(v) => Js.Json.decodeString(v)->Belt.Option.getWithDefault(".")
            | None => "."
            }
          | None => "."
          }
          // Security: Validate path before use
          switch sanitizePath(rawPath) {
          | Some(path) =>
            let result = runCommand("cow src/milk-ssg.cow > _site/index.html", ~cwd=Some(path))
            resolve(result)
          | None =>
            resolve({success: false, stdout: "", stderr: "Invalid path: contains forbidden characters", code: 1})
          }
        })
      },
    },
    {
      name: "milk_interpret",
      description: "Interpret COW code and show output",
      inputSchema: %raw(`{
        "type": "object",
        "properties": {
          "path": { "type": "string" },
          "file": { "type": "string", "description": "COW file to interpret" }
        }
      }`),
      execute: (params) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          let rawPath = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "path") {
            | Some(v) => Js.Json.decodeString(v)->Belt.Option.getWithDefault(".")
            | None => "."
            }
          | None => "."
          }
          let rawFile = switch Js.Json.decodeObject(params) {
          | Some(obj) =>
            switch Js.Dict.get(obj, "file") {
            | Some(v) => Js.Json.decodeString(v)->Belt.Option.getWithDefault("src/milk-ssg.cow")
            | None => "src/milk-ssg.cow"
            }
          | None => "src/milk-ssg.cow"
          }
          // Security: Validate both path and file to prevent command injection
          switch (sanitizePath(rawPath), sanitizePath(rawFile)) {
          | (Some(path), Some(file)) =>
            // Additional check: file must end with .cow
            if Js.String2.endsWith(file, ".cow") {
              let result = runCommand(`cow ${file}`, ~cwd=Some(path))
              resolve(result)
            } else {
              resolve({success: false, stdout: "", stderr: "Invalid file: must be a .cow file", code: 1})
            }
          | _ =>
            resolve({success: false, stdout: "", stderr: "Invalid path or file: contains forbidden characters", code: 1})
          }
        })
      },
    },
    {
      name: "milk_version",
      description: "Get milk-ssg info (COW esoteric language)",
      inputSchema: %raw(`{ "type": "object", "properties": {} }`),
      execute: (_) => {
        Js.Promise.make((~resolve, ~reject as _) => {
          resolve({
            success: true,
            stdout: "milk-ssg v0.1.0 - COW esoteric language SSG\nCommands: moo, mOo, moO, mOO, Moo, MOo, MoO, MOO, OOO, MMM, OOM, oom",
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
