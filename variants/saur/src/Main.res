// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// saur-ssg - Static site generator (ReScript)

module Config = {
  type t = {
    contentDir: string,
    publicDir: string,
    themesDir: string,
    baseUrl: string,
  }

  let default: t = {
    contentDir: "content",
    publicDir: "public",
    themesDir: "themes",
    baseUrl: "/",
  }
}

module Site = {
  type page = {
    path: string,
    title: string,
    content: string,
  }

  let build = async (config: Config.t): array<page> => {
    Js.Console.log(`Building site from ${config.contentDir}`)
    []
  }

  let serve = async (~port: int=8080): unit => {
    Js.Console.log(`Development server at http://localhost:${port->Belt.Int.toString}`)
  }
}

let name = "saur-ssg"
let version = "0.1.0"

let main = async () => {
  Js.Console.log(`${name} v${version}`)
  let config = Config.default
  let pages = await Site.build(config)
  Js.Console.log(`Generated ${pages->Js.Array2.length->Belt.Int.toString} pages`)
}

let _ = main()
