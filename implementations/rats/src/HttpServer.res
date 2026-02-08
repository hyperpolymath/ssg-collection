// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// HttpServer.res - HTTP server for health checks

open Types

// External: Start HTTP server (JS interop for Deno.serve)
@val
external startHttpServer: (int, string, (Fetch.Request.t) => promise<Fetch.Response.t>) => unit =
  "startHttpServer"

// Health check response
let healthResponse = (): Js.Json.t => {
  Js.Json.object_(
    Js.Dict.fromArray([
      ("status", Js.Json.string("healthy")),
      ("adapters", Js.Json.number(float_of_int(AdapterRegistry.adapterCount()))),
      ("tools", Js.Json.number(float_of_int(AdapterRegistry.toolCount()))),
    ]),
  )
}

// Adapters list response
let adaptersResponse = (): Js.Json.t => {
  let list =
    AdapterRegistry.getAllAdapters()->Array.map(((name, adapter)) => {
      Js.Json.object_(
        Js.Dict.fromArray([
          ("name", Js.Json.string(name)),
          ("displayName", Js.Json.string(adapter.name)),
          ("language", Js.Json.string(adapter.language)),
          ("tools", Js.Json.number(float_of_int(Array.length(adapter.tools)))),
          ("connected", Js.Json.boolean(adapter.isConnected())),
        ]),
      )
    })

  Js.Json.array(list)
}

// Handle HTTP request
let handleRequest = async (request: Fetch.Request.t): Fetch.Response.t => {
  let url = Fetch.Request.url(request)

  // Parse pathname from URL
  let pathname = switch Js.String2.split(url, "?")->Array.get(0) {
  | Some(path) =>
    switch Js.String2.split(path, "://")->Array.get(1) {
    | Some(rest) =>
      switch Js.String2.indexOf(rest, "/") {
      | -1 => "/"
      | idx => Js.String2.substr(rest, ~from=idx)
      }
    | None => "/"
    }
  | None => "/"
  }

  switch pathname {
  | "/health" =>
    Fetch.Response.make(
      Js.Json.stringify(healthResponse()),
      ~init={
        status: 200,
        headers: Fetch.Headers.fromObject({"Content-Type": "application/json"}),
      },
    )

  | "/adapters" =>
    Fetch.Response.make(
      Js.Json.stringify(adaptersResponse()),
      ~init={
        status: 200,
        headers: Fetch.Headers.fromObject({"Content-Type": "application/json"}),
      },
    )

  | _ => Fetch.Response.make("Not Found", ~init={status: 404})
  }
}

// Start the HTTP server
let start = (config: config) => {
  startHttpServer(config.port, config.host, handleRequest)
  Logger.info(`HTTP server listening on http://${config.host}:${Belt.Int.toString(config.port)}`)
  Logger.info(`Health check: http://${config.host}:${Belt.Int.toString(config.port)}/health`)
}
