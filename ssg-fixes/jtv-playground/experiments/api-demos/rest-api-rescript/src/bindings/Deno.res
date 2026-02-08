// Deno HTTP Server Bindings for ReScript
// SPDX-License-Identifier: AGPL-3.0-or-later

module Request = {
  type t

  @get external method_: t => string = "method"
  @get external url: t => string = "url"
  @send external json: t => promise<Js.Json.t> = "json"
  @send external text: t => promise<string> = "text"
}

module Response = {
  type init = {
    status?: int,
    headers?: Js.Dict.t<string>,
  }

  @new external make: (string, ~init: init=?) => 'a = "Response"

  let json = (~status=200, data: Js.Json.t): 'a => {
    let body = Js.Json.stringify(data)
    let headers = Js.Dict.fromArray([("Content-Type", "application/json")])
    make(body, ~init={status, headers})
  }

  let text = (~status=200, body: string): 'a => {
    make(body, ~init={status})
  }

  let notFound = (): 'a => {
    json(~status=404, Js.Json.object_(Js.Dict.fromArray([
      ("error", Js.Json.string("Not Found"))
    ])))
  }

  let badRequest = (message: string): 'a => {
    json(~status=400, Js.Json.object_(Js.Dict.fromArray([
      ("error", Js.Json.string(message))
    ])))
  }

  let serverError = (message: string): 'a => {
    json(~status=500, Js.Json.object_(Js.Dict.fromArray([
      ("error", Js.Json.string(message))
    ])))
  }
}

module URL = {
  type t

  @new external make: string => t = "URL"
  @get external pathname: t => string = "pathname"
  @get external searchParams: t => 'a = "searchParams"
}

type serveOptions = {
  port?: int,
  hostname?: string,
}

type serveHandler = Request.t => promise<Response.t>

@module("https://deno.land/std@0.208.0/http/server.ts")
external serve: (serveHandler, ~options: serveOptions=?) => promise<unit> = "serve"

// Alternative: Use Deno.serve directly
module DenoServe = {
  type server

  @scope("Deno") @val
  external serve: ({..}, Request.t => promise<'response>) => server = "serve"
}
