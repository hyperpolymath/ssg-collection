// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Mill-Based Synthesis Build System
 * Processes content through the analytical engine paradigm
 */

open Core

type buildConfig = {
  contentDir: string,
  templateDir: string,
  outputDir: string,
  baseUrl: option<string>,
  drafts: bool,
  verbose: bool,
}

type contentFile = {
  path: string,
  frontmatter: Js.Dict.t<Js.Json.t>,
  content: string,
  outputPath: string,
}

type buildResult = {
  success: bool,
  files: array<string>,
  errors: array<string>,
  duration: int,
}

// Deno FFI bindings (minimal JavaScript glue needed)
@module("./deno_fs.js")
external readDir: string => promise<array<{"name": string, "isFile": bool}>> = "readDir"

@module("./deno_fs.js")
external readTextFile: string => promise<string> = "readTextFile"

@module("./deno_fs.js")
external writeTextFile: (string, string) => promise<unit> = "writeTextFile"

@module("./deno_fs.js")
external mkdir: (string, {"recursive": bool}) => promise<unit> = "mkdir"

@module("./deno_fs.js")
external now: unit => int = "now"

/**
 * Parse YAML frontmatter from content
 */
let parseFrontmatter = (raw: string): {"frontmatter": Js.Dict.t<Js.Json.t>, "content": string} => {
  let frontmatterRegex = %re("/^---\n([\s\S]*?)\n---\n([\s\S]*)$/")
  switch Js.Re.exec_(frontmatterRegex, raw) {
  | Some(result) =>
    let captures = Js.Re.captures(result)
    let frontmatterStr = captures->Belt.Array.get(1)->Belt.Option.flatMap(Js.Nullable.toOption)
    let contentStr = captures->Belt.Array.get(2)->Belt.Option.flatMap(Js.Nullable.toOption)

    let frontmatter = Js.Dict.empty()
    switch frontmatterStr {
    | Some(fmStr) =>
      let lines = Js.String2.split(fmStr, "\n")
      lines->Belt.Array.forEach(line => {
        let colonIdx = Js.String2.indexOf(line, ":")
        if colonIdx > 0 {
          let key = Js.String2.trim(Js.String2.slice(line, ~from=0, ~to_=colonIdx))
          let valueStr = Js.String2.trim(Js.String2.sliceToEnd(line, ~from=colonIdx + 1))

          // Parse basic types
          let value = switch valueStr {
          | "true" => Js.Json.boolean(true)
          | "false" => Js.Json.boolean(false)
          | v =>
            switch Belt.Float.fromString(v) {
            | Some(n) => Js.Json.number(n)
            | None => Js.Json.string(v)
            }
          }
          Js.Dict.set(frontmatter, key, value)
        }
      })
    | None => ()
    }

    {
      "frontmatter": frontmatter,
      "content": contentStr->Belt.Option.getWithDefault(raw),
    }
  | None => {
      "frontmatter": Js.Dict.empty(),
      "content": raw,
    }
  }
}

/**
 * Apply template substitution using {{ variable }} syntax
 */
let applyTemplate = (template: string, variables: Js.Dict.t<Js.Json.t>): string => {
  let templateRegex = %re("/\{\{\s*(\w+(?:\.\w+)*)\s*\}\}/g")

  Js.String2.unsafeReplaceBy1(template, templateRegex, (match_, path, _, _) => {
    let parts = Js.String2.split(path, ".")
    let rec lookup = (obj: Js.Json.t, remaining: array<string>): string => {
      switch remaining->Belt.Array.get(0) {
      | None =>
        switch Js.Json.decodeString(obj) {
        | Some(s) => s
        | None =>
          switch Js.Json.decodeNumber(obj) {
          | Some(n) => Js.Float.toString(n)
          | None =>
            switch Js.Json.decodeBoolean(obj) {
            | Some(b) => b ? "true" : "false"
            | None => match_
            }
          }
        }
      | Some(part) =>
        switch Js.Json.decodeObject(obj) {
        | Some(dict) =>
          switch Js.Dict.get(dict, part) {
          | Some(nextObj) => lookup(nextObj, Belt.Array.sliceToEnd(remaining, 1))
          | None => match_
          }
        | None => match_
        }
      }
    }

    switch Js.Dict.get(variables, parts->Belt.Array.getExn(0)) {
    | Some(value) => lookup(value, Belt.Array.sliceToEnd(parts, 1))
    | None => match_
    }
  })
}

/**
 * Build a single content file
 */
let buildFile = async (
  file: contentFile,
  template: string,
  engine: engine,
): string => {
  // Load content variables into engine
  let variables = [
    {name: "content", type_: String, value: Js.Json.string(file.content), readonly: false},
    {
      name: "title",
      type_: String,
      value: Js.Dict.get(file.frontmatter, "title")->Belt.Option.getWithDefault(Js.Json.string("")),
      readonly: false,
    },
    {
      name: "date",
      type_: String,
      value: Js.Dict.get(file.frontmatter, "date")->Belt.Option.getWithDefault(Js.Json.string("")),
      readonly: false,
    },
    {name: "path", type_: String, value: Js.Json.string(file.path), readonly: false},
  ]

  // Add all frontmatter as variables
  Js.Dict.entries(file.frontmatter)->Belt.Array.forEach(((key, value)) => {
    Js.Array2.push(
      variables,
      {name: key, type_: String, value, readonly: false},
    )->ignore
  })

  loadVariables(engine, variables)

  // Build template context by merging frontmatter with content and path
  let context = Js.Dict.empty()
  Js.Dict.entries(file.frontmatter)->Belt.Array.forEach(((key, value)) => {
    Js.Dict.set(context, key, value)
  })
  Js.Dict.set(context, "content", Js.Json.string(file.content))
  Js.Dict.set(context, "path", Js.Json.string(file.path))

  applyTemplate(template, context)
}

/**
 * Main build function
 */
let build = async (config: buildConfig): buildResult => {
  let startTime = now()
  let result = ref({
    success: true,
    files: [],
    errors: [],
    duration: 0,
  })

  let engine = createEngine()

  try {
    // Read content directory
    let entries = await readDir(config.contentDir)
    let contentFiles = []

    for i in 0 to Array.length(entries) - 1 {
      switch entries->Belt.Array.get(i) {
      | Some(entry) =>
        if (
          entry["isFile"] &&
          (Js.String2.endsWith(entry["name"], ".md") ||
            Js.String2.endsWith(entry["name"], ".markdown"))
        ) {
          let path = `${config.contentDir}/${entry["name"]}`
          let raw = await readTextFile(path)
          let parsed = parseFrontmatter(raw)

          // Skip drafts if not enabled
          let isDraft =
            Js.Dict.get(parsed["frontmatter"], "draft")
            ->Belt.Option.flatMap(Js.Json.decodeBoolean)
            ->Belt.Option.getWithDefault(false)

          if !isDraft || config.drafts {
            let outputName =
              entry["name"]
              ->Js.String2.replace(".markdown", ".html")
              ->Js.String2.replace(".md", ".html")

            Js.Array2.push(
              contentFiles,
              {
                path,
                frontmatter: parsed["frontmatter"],
                content: parsed["content"],
                outputPath: `${config.outputDir}/${outputName}`,
              },
            )->ignore
          }
        }
      | None => ()
      }
    }

    // Read default template
    let template = ref("<html><body>{{ content }}</body></html>")
    try {
      template := await readTextFile(`${config.templateDir}/default.html`)
    } catch {
    | _ =>
      if config.verbose {
        Js.Console.log("Using default template")
      }
    }

    // Ensure output directory exists
    await mkdir(config.outputDir, {"recursive": true})

    // Process each file
    for i in 0 to Array.length(contentFiles) - 1 {
      switch contentFiles->Belt.Array.get(i) {
      | Some(file) =>
        try {
          let html = await buildFile(file, template.contents, engine)
          await writeTextFile(file.outputPath, html)
          Js.Array2.push(result.contents.files, file.outputPath)->ignore

          if config.verbose {
            Js.Console.log(`Built: ${file.outputPath}`)
          }
        } catch {
        | Js.Exn.Error(e) =>
          let msg = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
          Js.Array2.push(result.contents.errors, `Error building ${file.path}: ${msg}`)->ignore
          result := {...result.contents, success: false}
        }
      | None => ()
      }
    }
  } catch {
  | Js.Exn.Error(e) =>
    let msg = Js.Exn.message(e)->Belt.Option.getWithDefault("Unknown error")
    Js.Array2.push(result.contents.errors, `Build failed: ${msg}`)->ignore
    result := {...result.contents, success: false}
  }

  {...result.contents, duration: now() - startTime}
}
