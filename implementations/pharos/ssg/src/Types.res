// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG SSG - Type Definitions

// Content frontmatter schema
type frontmatter = {
  title: string,
  date: option<string>,
  author: option<string>,
  tags: option<array<string>>,
  draft: option<bool>,
  template: option<string>,
  a11y: option<a11yMetadata>,
}

// Accessibility metadata
and a11yMetadata = {
  bsl: option<bslData>,
  gsl: option<gslData>,
  asl: option<aslData>,
  makaton: option<makatonData>,
  altText: option<string>,
  readingLevel: option<string>,
  audioDescription: option<string>,
}

// British Sign Language metadata
and bslData = {
  videoUrl: option<string>,
  glosses: option<array<string>>,
  interpreter: option<string>,
}

// German Sign Language metadata
and gslData = {
  videoUrl: option<string>,
  glosses: option<array<string>>,
}

// American Sign Language metadata
and aslData = {
  videoUrl: option<string>,
  glosses: option<array<string>>,
}

// Makaton metadata
and makatonData = {
  symbols: option<array<string>>,
  signs: option<array<string>>,
}

// Site configuration schema
type siteConfig = {
  name: string,
  baseUrl: string,
  language: string,
  outputDir: string,
  contentDir: string,
  templateDir: string,
  theme: option<string>,
  plugins: option<array<string>>,
  a11y: option<a11yConfig>,
}

// Accessibility configuration
and a11yConfig = {
  enableBsl: bool,
  enableGsl: bool,
  enableAsl: bool,
  enableMakaton: bool,
  wcagLevel: string, // "A", "AA", "AAA"
}

// Content item
type contentItem = {
  path: string,
  frontmatter: frontmatter,
  body: string,
  outputPath: string,
}

// Template context
type templateContext = {
  site: siteConfig,
  page: contentItem,
  variables: Js.Dict.t<Js.Json.t>,
}

// Build result
type buildResult =
  | Success({outputFiles: array<string>, duration: float})
  | Error({message: string, file: option<string>, line: option<int>})

// Operation card for mill-based synthesis
type operationKind =
  | LoadVariable
  | StoreVariable
  | TemplateSubstitute
  | ContentTransform
  | OutputGenerate
  | ConditionalBranch
  | LoopStart
  | LoopEnd

type operationCard = {
  op: operationKind,
  operand1: string,
  operand2: string,
  resultRef: int,
}

// Mill state
type millState = {
  mutable currentCard: int,
  mutable stackDepth: int,
  mutable errorFlag: bool,
  mutable errorMessage: string,
}

// Verification result (Bernoulli)
type verificationResult = {
  passed: bool,
  confidence: float,
  trials: int,
  failures: int,
  message: string,
}
