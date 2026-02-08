// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// types.res — Eclipse SSG type definitions

// ============================================================
// CORE TYPES
// ============================================================

type contentFormat =
  | Markdown
  | AsciiDoc
  | OrgMode
  | PlainText

type buildMode =
  | Development
  | Production
  | Verification

// ============================================================
// CONFIGURATION
// ============================================================

type siteConfig = {
  title: string,
  baseUrl: string,
  language: string,
  description: option<string>,
  author: option<string>,
  theme: option<string>,
}

type buildConfig = {
  mode: buildMode,
  outputDir: string,
  includeDrafts: bool,
  minify: bool,
  sourceMaps: bool,
}

type contentConfig = {
  directory: string,
  extensions: array<string>,
}

type templateConfig = {
  directory: string,
  engine: string,
}

type a11yConfig = {
  enabled: bool,
  signLanguages: array<string>,
}

type config = {
  site: siteConfig,
  build: buildConfig,
  content: contentConfig,
  templates: templateConfig,
  a11y: a11yConfig,
}

// ============================================================
// CONTENT TYPES
// ============================================================

type frontmatter = {
  title: string,
  date: option<string>,
  draft: bool,
  tags: array<string>,
  categories: array<string>,
  extra: Js.Dict.t<Js.Json.t>,
}

type content = {
  path: string,
  format: contentFormat,
  frontmatter: frontmatter,
  body: string,
  rawContent: string,
}

type page = {
  content: content,
  url: string,
  outputPath: string,
}

// ============================================================
// TEMPLATE TYPES (Operation Cards)
// ============================================================

type variableValue =
  | StringValue(string)
  | NumberValue(float)
  | BoolValue(bool)
  | ArrayValue(array<variableValue>)
  | ObjectValue(Js.Dict.t<variableValue>)

type variableStore = Js.Dict.t<variableValue>

type operationCard = {
  name: string,
  template: string,
  variables: array<string>,
  output: string,
}

// ============================================================
// BUILD TYPES
// ============================================================

type buildResult =
  | Success(array<page>)
  | Error(string)

type buildStats = {
  pagesBuilt: int,
  duration: float,
  outputSize: int,
}

// ============================================================
// ADAPTER TYPES
// ============================================================

type adapterInfo = {
  name: string,
  language: string,
  description: string,
  version: option<string>,
}

type adapterTool = {
  name: string,
  description: string,
  inputSchema: Js.Json.t,
}

type adapterStatus =
  | Connected
  | Disconnected
  | Error(string)

// ============================================================
// ACCESSIBILITY TYPES
// ============================================================

type signLanguage =
  | BSL  // British Sign Language
  | GSL  // German Sign Language
  | ASL  // American Sign Language
  | Makaton

type a11yMetadata = {
  signLanguages: array<signLanguage>,
  altText: option<string>,
  transcript: option<string>,
  audioDescription: option<string>,
}
