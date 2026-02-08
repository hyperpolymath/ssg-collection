// SPDX-License-Identifier: PMPL-1.0-or-later
// WokeLang SSG Frontend - Main Application

open Tea

// ============================================================================
// Model
// ============================================================================

type route =
  | Home
  | Docs(option<string>)
  | Examples
  | Playground
  | NotFound

type model = {
  route: route,
  pageContent: string,
  loading: bool,
}

let init = (): (model, Cmd.t<msg>) => {
  let initialModel = {
    route: Home,
    pageContent: "",
    loading: false,
  }
  (initialModel, Cmd.none)
}

// ============================================================================
// Update
// ============================================================================

type msg =
  | UrlChanged(route)
  | PageLoaded(string)
  | LoadPage(string)
  | NoOp

let update = (model: model, msg: msg): (model, Cmd.t<msg>) => {
  switch msg {
  | UrlChanged(newRoute) =>
    let (updatedModel, cmd) = switch newRoute {
    | Home => ({...model, route: Home}, loadPageCmd("/"))
    | Docs(slug) => ({...model, route: Docs(slug)}, loadPageCmd("/docs/" ++ Belt.Option.getWithDefault(slug, "index")))
    | Examples => ({...model, route: Examples}, loadPageCmd("/examples"))
    | Playground => ({...model, route: Playground}, Cmd.none)
    | NotFound => ({...model, route: NotFound}, Cmd.none)
    }
    (updatedModel, cmd)

  | LoadPage(url) =>
    ({...model, loading: true}, fetchPageCmd(url))

  | PageLoaded(content) =>
    ({...model, pageContent: content, loading: false}, Cmd.none)

  | NoOp =>
    (model, Cmd.none)
  }
}

// ============================================================================
// View
// ============================================================================

let view = (model: model): Html.html<msg> => {
  Layout.render(
    ~route=model.route,
    ~content=model.pageContent,
    ~loading=model.loading,
  )
}

// ============================================================================
// Subscriptions
// ============================================================================

let subscriptions = (_model: model): Sub.t<msg> => {
  Router.urlChanges(url => {
    let route = Router.parseUrl(url)
    UrlChanged(route)
  })
}

// ============================================================================
// Commands
// ============================================================================

let loadPageCmd = (path: string): Cmd.t<msg> => {
  LoadPage(path)->Cmd.msg
}

let fetchPageCmd = (url: string): Cmd.t<msg> => {
  // In production, this would fetch from the server
  // For now, simulate with local content
  Cmd.none
}

// ============================================================================
// Main
// ============================================================================

let main = () => {
  Tea.Program.standardProgram({
    init: init,
    update: update,
    view: view,
    subscriptions: subscriptions,
  })
}
