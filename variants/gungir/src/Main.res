// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// gungir-ssg - Static site generator (ReScript)

module Config = {
  type t = {
    inputDir: string,
    outputDir: string,
    templateDir: option<string>,
    baseUrl: option<string>,
  }

  let default: t = {
    inputDir: "content",
    outputDir: "public",
    templateDir: Some("templates"),
    baseUrl: None,
  }

  let fromEnv = (): t => {
    // Would read from environment/config file
    default
  }
}

module Build = {
  type result = {
    success: bool,
    filesProcessed: int,
    duration: float,
  }

  let run = async (config: Config.t): result => {
    let startTime = Js.Date.now()
    Js.Console.log(`Building from ${config.inputDir} to ${config.outputDir}...`)

    // Build logic would go here

    {
      success: true,
      filesProcessed: 0,
      duration: Js.Date.now() -. startTime,
    }
  }
}

module Serve = {
  let start = async (~port: int=8000, ~dir: string="public"): unit => {
    Js.Console.log(`Serving ${dir} on http://localhost:${port->Belt.Int.toString}`)
    // Server logic would use Deno.serve
  }
}

// Main entry
let main = async () => {
  Js.Console.log("gungir-ssg v0.3.0")

  let config = Config.fromEnv()
  let result = await Build.run(config)

  if result.success {
    Js.Console.log(`Build complete: ${result.filesProcessed->Belt.Int.toString} files in ${result.duration->Js.Float.toFixedWithPrecision(~digits=0)}ms`)
  } else {
    Js.Console.error("Build failed")
  }
}

let _ = main()
