// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// build.res — Mill-based synthesis for Eclipse SSG

open Types

// ============================================================
// MILL-BASED SYNTHESIS
// ============================================================

// Mill operations inspired by the Analytical Engine
module Mill = {
  // Process content through the mill
  let process = (content: content, store: variableStore): result<string, string> => {
    // Extract variables from frontmatter
    let vars = Js.Dict.fromArray([
      ("title", StringValue(content.frontmatter.title)),
      ("body", StringValue(content.body)),
      ("date", StringValue(content.frontmatter.date->Belt.Option.getWithDefault(""))),
      ("draft", BoolValue(content.frontmatter.draft)),
    ])

    // Merge with global store
    store->Js.Dict.entries->Belt.Array.forEach(((k, v)) => {
      vars->Js.Dict.set(k, v)
    })

    Ok(content.body)
  }

  // Apply operation card
  let applyCard = (card: operationCard, store: variableStore): result<string, string> => {
    // Substitute variables in template
    let result = card.variables->Belt.Array.reduce(card.template, (acc, varName) => {
      switch store->Js.Dict.get(varName) {
      | Some(StringValue(s)) =>
        acc->Js.String2.replace("{{ " ++ varName ++ " }}", s)
      | Some(NumberValue(n)) =>
        acc->Js.String2.replace("{{ " ++ varName ++ " }}", Belt.Float.toString(n))
      | Some(BoolValue(b)) =>
        acc->Js.String2.replace("{{ " ++ varName ++ " }}", b ? "true" : "false")
      | _ => acc
      }
    })
    Ok(result)
  }
}

// ============================================================
// BUILD PIPELINE
// ============================================================

module Build = {
  // Load configuration
  let loadConfig = (path: string): result<config, string> => {
    // Would load from noteg.config.json
    Ok({
      site: {
        title: "Eclipse SSG Site",
        baseUrl: "https://example.com",
        language: "en",
        description: Some("A site built with eclipse-ssg"),
        author: None,
        theme: None,
      },
      build: {
        mode: Production,
        outputDir: "public",
        includeDrafts: false,
        minify: true,
        sourceMaps: false,
      },
      content: {
        directory: "content",
        extensions: ["md", "adoc"],
      },
      templates: {
        directory: "templates",
        engine: "operation-card",
      },
      a11y: {
        enabled: true,
        signLanguages: [],
      },
    })
  }

  // Parse frontmatter from content
  let parseFrontmatter = (raw: string): result<(frontmatter, string), string> => {
    // Simple YAML frontmatter parser
    if raw->Js.String2.startsWith("---") {
      let parts = raw->Js.String2.split("---")
      if parts->Belt.Array.length >= 3 {
        Ok(({
          title: "Untitled",
          date: None,
          draft: false,
          tags: [],
          categories: [],
          extra: Js.Dict.empty(),
        }, parts->Belt.Array.getExn(2)))
      } else {
        Error("Invalid frontmatter")
      }
    } else {
      Ok(({
        title: "Untitled",
        date: None,
        draft: false,
        tags: [],
        categories: [],
        extra: Js.Dict.empty(),
      }, raw))
    }
  }

  // Build single page
  let buildPage = (content: content, config: config): result<page, string> => {
    let store = Js.Dict.empty()
    store->Js.Dict.set("site_title", StringValue(config.site.title))
    store->Js.Dict.set("base_url", StringValue(config.site.baseUrl))

    switch Mill.process(content, store) {
    | Ok(processed) => Ok({
        content: {...content, body: processed},
        url: "/" ++ content.path,
        outputPath: config.build.outputDir ++ "/" ++ content.path ++ ".html",
      })
    | Error(e) => Error(e)
    }
  }

  // Full build
  let build = (config: config): buildResult => {
    // Would scan content directory and build all pages
    Success([])
  }
}

// ============================================================
// EXPORTS
// ============================================================

let version = "0.2.0"

let buildSite = (configPath: string): buildResult => {
  switch Build.loadConfig(configPath) {
  | Ok(config) => Build.build(config)
  | Error(e) => Error(e)
  }
}
