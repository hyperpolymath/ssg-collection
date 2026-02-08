// SPDX-License-Identifier: PMPL-1.0-or-later
// WokeLang SSG Frontend - Layout with a2ml positioning

open Tea.Html
open A2ml

// ============================================================================
// Main Layout
// ============================================================================

let render = (~route: App.route, ~content: string, ~loading: bool): html<App.msg> => {
  div(
    [Pos.container],
    [
      header(),
      main(~route, ~content, ~loading),
      footer(),
    ],
  )
}

// ============================================================================
// Header
// ============================================================================

let header = (): html<App.msg> => {
  div(
    [
      Pos.sticky({top: 0}),
      Pos.zIndex(100),
      Style.bg("var(--color-primary)"),
      Style.color("white"),
      Pos.padding({all: 20}),
    ],
    [
      div(
        [
          Pos.flex,
          Pos.justifyBetween,
          Pos.alignCenter,
          Pos.maxWidth(1200),
          Pos.margin({horizontal: "auto"}),
        ],
        [
          h1(
            [Style.fontSize(28), Style.fontWeight(700)],
            [text("WokeLang")],
          ),
          nav(),
        ],
      ),
    ],
  )
}

let nav = (): html<App.msg> => {
  div(
    [
      Pos.flex,
      Pos.gap(20),
    ],
    [
      navLink(~route=App.Home, ~label="Home"),
      navLink(~route=App.Docs(None), ~label="Documentation"),
      navLink(~route=App.Examples, ~label="Examples"),
      navLink(~route=App.Playground, ~label="Playground"),
    ],
  )
}

let navLink = (~route: App.route, ~label: string): html<App.msg> => {
  a(
    [
      href(Router.routeToUrl(route)),
      Style.color("white"),
      Style.textDecoration("none"),
      Style.fontWeight(500),
      Events.onClick(e => {
        Event.preventDefault(e)
        Router.navigateTo(route)
        App.NoOp
      }),
    ],
    [text(label)],
  )
}

// ============================================================================
// Main Content Area
// ============================================================================

let main = (~route: App.route, ~content: string, ~loading: bool): html<App.msg> => {
  div(
    [
      Pos.minHeight("calc(100vh - 200px)"),
      Pos.padding({vertical: 40, horizontal: 20}),
      Pos.maxWidth(1200),
      Pos.margin({horizontal: "auto"}),
    ],
    [
      if loading {
        loadingSpinner()
      } else {
        switch route {
        | App.Home => homePage(content)
        | App.Docs(slug) => docsPage(content, slug)
        | App.Examples => examplesPage(content)
        | App.Playground => playgroundPage()
        | App.NotFound => notFoundPage()
        }
      },
    ],
  )
}

// ============================================================================
// Page Components
// ============================================================================

let homePage = (content: string): html<App.msg> => {
  div(
    [],
    [
      // Hero section
      div(
        [
          Pos.textAlign("center"),
          Pos.padding({vertical: 60, horizontal: 20}),
        ],
        [
          h1(
            [Style.fontSize(48), Style.marginBottom(20)],
            [text("Welcome to WokeLang")],
          ),
          p(
            [Style.fontSize(24), Style.color("var(--color-text-secondary)")],
            [text("A Human-Centered Programming Language")],
          ),
        ],
      ),

      // Features grid
      div(
        [
          Pos.grid({columns: 3, gap: 30}),
          Pos.marginTop(40),
        ],
        [
          featureCard(
            ~icon="🔐",
            ~title="Consent-Driven Computing",
            ~description="Programs request permission before accessing resources",
          ),
          featureCard(
            ~icon="🙏",
            ~title="Gratitude System",
            ~description="Dependencies are acknowledged explicitly",
          ),
          featureCard(
            ~icon="💭",
            ~title="Emotive Programming",
            ~description="Code can express tone and emotion",
          ),
        ],
      ),

      // Content from markdown
      div(
        [Pos.marginTop(60)],
        [dangerouslySetInnerHTML(content)],
      ),
    ],
  )
}

let docsPage = (content: string, _slug: option<string>): html<App.msg> => {
  div(
    [Pos.flex, Pos.gap(40)],
    [
      // Sidebar
      aside(
        [
          Pos.width(250),
          Pos.flexShrink(0),
        ],
        [
          h3([], [text("Documentation")]),
          // Doc navigation would go here
        ],
      ),

      // Content
      article(
        [Pos.flex1],
        [dangerouslySetInnerHTML(content)],
      ),
    ],
  )
}

let examplesPage = (content: string): html<App.msg> => {
  div(
    [],
    [
      h1([], [text("Examples")]),
      dangerouslySetInnerHTML(content),
    ],
  )
}

let playgroundPage = (): html<App.msg> => {
  div(
    [],
    [
      h1([], [text("WokeLang Playground")]),
      p([], [text("Interactive playground coming soon!")]),
      p([], [text("This will be hosted at playground.wokelang.org")]),
    ],
  )
}

let notFoundPage = (): html<App.msg> => {
  div(
    [Pos.textAlign("center"), Pos.padding({all: 60})],
    [
      h1([Style.fontSize(72)], [text("404")]),
      p([Style.fontSize(24)], [text("Page not found")]),
    ],
  )
}

// ============================================================================
// UI Components
// ============================================================================

let featureCard = (~icon: string, ~title: string, ~description: string): html<App.msg> => {
  div(
    [
      Pos.padding({all: 30}),
      Style.border("1px solid var(--color-border)"),
      Style.borderRadius(8),
      Style.bg("white"),
    ],
    [
      div([Style.fontSize(48), Pos.marginBottom(15)], [text(icon)]),
      h3([Style.marginBottom(10)], [text(title)]),
      p([Style.color("var(--color-text-secondary)")], [text(description)]),
    ],
  )
}

let loadingSpinner = (): html<App.msg> => {
  div(
    [
      Pos.flex,
      Pos.justifyCenter,
      Pos.alignCenter,
      Pos.padding({all: 60}),
    ],
    [text("Loading...")],
  )
}

// ============================================================================
// Footer
// ============================================================================

let footer = (): html<App.msg> => {
  div(
    [
      Pos.padding({all: 30}),
      Style.bg("var(--color-bg-secondary)"),
      Pos.textAlign("center"),
      Style.borderTop("1px solid var(--color-border)"),
    ],
    [
      p(
        [Style.color("var(--color-text-secondary)")],
        [text("© 2026 Jonathan D.A. Jewell. Released under PMPL-1.0-or-later.")],
      ),
    ],
  )
}
