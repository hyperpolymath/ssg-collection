// SPDX-License-Identifier: PMPL-1.0-or-later
// WokeLang SSG Frontend - Routing with cadre-tea-router

open CadreTeaRouter

// ============================================================================
// Route Definitions
// ============================================================================

type route =
  | Home
  | Docs(option<string>)
  | Examples
  | Playground
  | NotFound

// ============================================================================
// Route Parser
// ============================================================================

let parseUrl = (url: string): route => {
  let segments = url
    ->String.split("/")
    ->Array.filter(s => s != "")

  switch segments {
  | [] => Home
  | ["docs"] => Docs(None)
  | ["docs", slug] => Docs(Some(slug))
  | ["examples"] => Examples
  | ["playground"] => Playground
  | _ => NotFound
  }
}

// ============================================================================
// Route to URL
// ============================================================================

let routeToUrl = (route: route): string => {
  switch route {
  | Home => "/"
  | Docs(None) => "/docs"
  | Docs(Some(slug)) => "/docs/" ++ slug
  | Examples => "/examples"
  | Playground => "/playground"
  | NotFound => "/404"
  }
}

// ============================================================================
// Navigation
// ============================================================================

let navigateTo = (route: route): unit => {
  let url = routeToUrl(route)
  Router.pushUrl(url)
}

// ============================================================================
// URL Change Subscription
// ============================================================================

let urlChanges = (handler: string => 'msg): Tea.Sub.t<'msg> => {
  Router.onUrlChange(url => handler(url))
}
