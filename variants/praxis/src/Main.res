// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// praxis-ssg - Static site generator (ReScript)

module Config = {
  type t = {
    source: string,
    output: string,
    template: string,
  }

  let default: t = {
    source: "src",
    output: "build",
    template: "templates",
  }
}

module Generator = {
  let generate = async (config: Config.t): bool => {
    Js.Console.log(`Generating site from ${config.source}`)
    true
  }
}

let name = "praxis-ssg"
let version = "0.2.0"

let main = async () => {
  Js.Console.log(`${name} v${version}`)
  let config = Config.default
  let success = await Generator.generate(config)
  if success {
    Js.Console.log("Site generated successfully")
  }
}

let _ = main()
