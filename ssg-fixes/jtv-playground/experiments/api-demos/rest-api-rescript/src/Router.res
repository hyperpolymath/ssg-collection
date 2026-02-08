// Simple Router for Deno HTTP Server
// SPDX-License-Identifier: AGPL-3.0-or-later

open Deno

type route = {
  method_: string,
  pattern: string,
  handler: Request.t => promise<Response.t>,
}

type params = Js.Dict.t<string>

type routeMatch = {
  handler: Request.t => promise<Response.t>,
  params: params,
}

let parsePathParams = (pattern: string, pathname: string): option<params> => {
  let patternParts = Js.String2.split(pattern, "/")
  let pathParts = Js.String2.split(pathname, "/")

  if Array.length(patternParts) != Array.length(pathParts) {
    None
  } else {
    let params = Js.Dict.empty()
    let matched = ref(true)

    for i in 0 to Array.length(patternParts) - 1 {
      let patternPart = patternParts[i]->Option.getOr("")
      let pathPart = pathParts[i]->Option.getOr("")

      if Js.String2.startsWith(patternPart, ":") {
        let paramName = Js.String2.sliceToEnd(patternPart, ~start=1)
        Js.Dict.set(params, paramName, pathPart)
      } else if patternPart != pathPart {
        matched := false
      }
    }

    if matched.contents {
      Some(params)
    } else {
      None
    }
  }
}

let matchRoute = (routes: array<route>, method_: string, pathname: string): option<routeMatch> => {
  routes->Array.findMap(route => {
    if route.method_ == method_ {
      switch parsePathParams(route.pattern, pathname) {
      | Some(params) => Some({handler: route.handler, params})
      | None => None
      }
    } else {
      None
    }
  })
}

// Route context stored in a mutable ref for handler access
let currentParams: ref<params> = ref(Js.Dict.empty())

let getParam = (name: string): option<string> => {
  Js.Dict.get(currentParams.contents, name)
}

let getParamExn = (name: string): string => {
  switch getParam(name) {
  | Some(v) => v
  | None => Js.Exn.raiseError(`Missing param: ${name}`)
  }
}
