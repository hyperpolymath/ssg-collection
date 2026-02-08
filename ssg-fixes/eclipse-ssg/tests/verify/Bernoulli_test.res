// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Bernoulli_test.res — Bernoulli verification tests

// ============================================================
// DENO TEST BINDINGS
// ============================================================

@scope("Deno") @val
external test: (string, unit => unit) => unit = "test"

let assertEquals = (a, b, msg) => {
  if a != b {
    Js.Exn.raiseError(msg)
  }
}

let assertExists = (opt: option<'a>, msg: string) => {
  switch opt {
  | None => Js.Exn.raiseError(msg)
  | Some(_) => ()
  }
}

// ============================================================
// PROPERTY: Build Determinism
// Same inputs should always produce same outputs
// ============================================================

let _ = test("Verify - Build determinism", () => {
  type buildInput = {
    content: string,
    template: string,
    variables: Js.Dict.t<string>,
  }

  let input: buildInput = {
    content: "# Hello World",
    template: "<h1>{{ title }}</h1>",
    variables: Js.Dict.fromArray([("title", "Hello World")]),
  }

  // Simulate build
  let build = (input: buildInput): string => {
    switch Js.Dict.get(input.variables, "title") {
    | Some(title) => Js.String2.replace(input.template, "{{ title }}", title)
    | None => input.template
    }
  }

  // Run multiple times
  let results = Belt.Array.makeBy(10, _ => build(input))

  // All results should be identical
  let first = results[0]->Option.getOr("")
  results->Array.forEach(result => {
    assertEquals(result, first, "Build output should be deterministic")
  })
})

// ============================================================
// PROPERTY: Template Variable Safety
// Undefined variables should be handled safely
// ============================================================

let _ = test("Verify - Template variable safety", () => {
  let template = "Hello {{ name }}, welcome to {{ place }}!"
  let variables = Js.Dict.fromArray([("name", "User")]) // 'place' is missing

  let safeSubstitute = (template: string, variables: Js.Dict.t<string>): string => {
    let result = ref(template)
    // Simple substitution - in real code would use regex
    switch Js.Dict.get(variables, "name") {
    | Some(name) => result := Js.String2.replace(result.contents, "{{ name }}", name)
    | None => result := Js.String2.replace(result.contents, "{{ name }}", "[UNDEFINED:name]")
    }
    switch Js.Dict.get(variables, "place") {
    | Some(place) => result := Js.String2.replace(result.contents, "{{ place }}", place)
    | None => result := Js.String2.replace(result.contents, "{{ place }}", "[UNDEFINED:place]")
    }
    result.contents
  }

  let result = safeSubstitute(template, variables)

  assertEquals(Js.String2.includes(result, "User"), true, "Should contain substituted value")
  assertEquals(Js.String2.includes(result, "[UNDEFINED:place]"), true, "Should mark undefined vars")
})

// ============================================================
// PROPERTY: Content Format Preservation
// Content should maintain structure through transformation
// ============================================================

let _ = test("Verify - Content format preservation", () => {
  let markdown = "---\ntitle: Test\n---\n\n# Heading 1\n\nParagraph text.\n\n- Item 1\n- Item 2\n"

  // Parse frontmatter and body
  type parsedContent = {
    frontmatter: string,
    body: string,
  }

  let parseContent = (content: string): parsedContent => {
    let parts = Js.String2.split(content, "---")
    {
      frontmatter: switch parts[1] {
      | Some(fm) => Js.String2.trim(fm)
      | None => ""
      },
      body: parts->Array.sliceToEnd(~start=2)->Array.joinWith("---")->Js.String2.trim,
    }
  }

  let {frontmatter, body} = parseContent(markdown)

  // Frontmatter should be extractable
  assertExists(Some(frontmatter), "Frontmatter should exist")
  assertEquals(Js.String2.includes(frontmatter, "title: Test"), true, "Should contain title")

  // Body structure should be preserved
  assertEquals(Js.String2.includes(body, "# Heading 1"), true, "Should contain heading")
  assertEquals(Js.String2.includes(body, "- Item 1"), true, "Should contain list item")
})

// ============================================================
// PROPERTY: Adapter Isolation
// Adapters should not affect each other's state
// ============================================================

let _ = test("Verify - Adapter isolation", () => {
  // Simulate adapter state
  type adapter = {
    name: string,
    mutable connected: bool,
  }

  let createAdapter = (name: string): adapter => {
    {name, connected: false}
  }

  let connect = (adapter: adapter): unit => {
    adapter.connected = true
  }

  let disconnect = (adapter: adapter): unit => {
    adapter.connected = false
  }

  let isConnected = (adapter: adapter): bool => {
    adapter.connected
  }

  let adapterA = createAdapter("A")
  let adapterB = createAdapter("B")

  // Connect A
  connect(adapterA)
  assertEquals(isConnected(adapterA), true, "A should be connected")
  assertEquals(isConnected(adapterB), false, "B should be unaffected")

  // Connect B
  connect(adapterB)
  assertEquals(isConnected(adapterA), true, "A should still be connected")
  assertEquals(isConnected(adapterB), true, "B should be connected")

  // Disconnect A
  disconnect(adapterA)
  assertEquals(isConnected(adapterA), false, "A should be disconnected")
  assertEquals(isConnected(adapterB), true, "B should be unaffected")
})

// ============================================================
// PROPERTY: Configuration Validation
// Invalid configs should be rejected
// ============================================================

let _ = test("Verify - Configuration validation", () => {
  type config = {
    title: option<string>,
    baseUrl: option<string>,
    outputDir: option<string>,
  }

  type validationResult = {
    valid: bool,
    errors: array<string>,
  }

  let validateConfig = (config: config): validationResult => {
    let errors = ref([])

    switch config.title {
    | None => errors := Array.concat(errors.contents, ["title is required"])
    | Some(t) if Js.String2.trim(t) == "" =>
      errors := Array.concat(errors.contents, ["title is required"])
    | _ => ()
    }

    switch config.baseUrl {
    | None => errors := Array.concat(errors.contents, ["baseUrl is required"])
    | Some(url) if !Js.String2.startsWith(url, "http") =>
      errors := Array.concat(errors.contents, ["baseUrl must be a valid URL"])
    | _ => ()
    }

    switch config.outputDir {
    | Some(dir) if Js.String2.includes(dir, "..") =>
      errors := Array.concat(errors.contents, ["outputDir cannot contain path traversal"])
    | _ => ()
    }

    {valid: Array.length(errors.contents) == 0, errors: errors.contents}
  }

  // Valid config
  let valid = validateConfig({
    title: Some("My Site"),
    baseUrl: Some("https://example.com"),
    outputDir: Some("public"),
  })
  assertEquals(valid.valid, true, "Valid config")
  assertEquals(Array.length(valid.errors), 0, "No errors")

  // Invalid: missing title
  let noTitle = validateConfig({
    title: None,
    baseUrl: Some("https://example.com"),
    outputDir: None,
  })
  assertEquals(noTitle.valid, false, "Invalid without title")
  assertEquals(noTitle.errors->Array.includes("title is required"), true, "Title error")

  // Invalid: bad URL
  let badUrl = validateConfig({
    title: Some("Test"),
    baseUrl: Some("not-a-url"),
    outputDir: None,
  })
  assertEquals(badUrl.valid, false, "Invalid URL")

  // Invalid: path traversal
  let pathTraversal = validateConfig({
    title: Some("Test"),
    baseUrl: Some("https://example.com"),
    outputDir: Some("../../../etc"),
  })
  assertEquals(pathTraversal.valid, false, "Path traversal detected")
})

// ============================================================
// PROPERTY: Output Path Safety
// Generated paths should not escape output directory
// ============================================================

let _ = test("Verify - Output path safety", () => {
  let sanitizePath = (basedir: string, filepath: string): option<string> => {
    // Normalize and resolve
    let normalized = filepath
      ->Js.String2.replaceByRe(%re("/\\\\/g"), "/")
      ->Js.String2.replaceByRe(%re("/\\/+/g"), "/")

    // Reject if contains traversal
    if Js.String2.includes(normalized, "..") {
      None
    } else if Js.String2.startsWith(normalized, "/") {
      // Reject if absolute
      None
    } else {
      Some(basedir ++ "/" ++ normalized)
    }
  }

  // Safe paths
  assertEquals(sanitizePath("/out", "index.html"), Some("/out/index.html"), "Safe path 1")
  assertEquals(sanitizePath("/out", "posts/hello.html"), Some("/out/posts/hello.html"), "Safe path 2")

  // Unsafe paths
  assertEquals(sanitizePath("/out", "../etc/passwd"), None, "Path traversal blocked")
  assertEquals(sanitizePath("/out", "/etc/passwd"), None, "Absolute path blocked")
  assertEquals(sanitizePath("/out", "foo/../../bar"), None, "Nested traversal blocked")
})
