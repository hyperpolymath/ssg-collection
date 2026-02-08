// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Type definitions for odd-ssg
 * Content schema, site configuration, and adapter interfaces
 */

// ============================================================================
// Site Configuration
// ============================================================================

type authorConfig = {
  name: string,
  email: option<string>,
  url: option<string>,
}

type buildOptions = {
  contentDir: string,
  templateDir: string,
  outputDir: string,
  drafts: bool,
  minify: bool,
  sitemap: bool,
  rss: bool,
}

type wcagLevel = A | AA | AAA

type accessibilityConfig = {
  bsl: bool,
  asl: bool,
  gsl: bool,
  makaton: bool,
  easyRead: bool,
  wcagLevel: wcagLevel,
  statementUrl: option<string>,
}

type siteConfig = {
  title: string,
  description: option<string>,
  baseUrl: string,
  language: option<string>,
  author: option<authorConfig>,
  build: option<buildOptions>,
  accessibility: option<accessibilityConfig>,
  metadata: option<Js.Dict.t<Js.Json.t>>,
}

// ============================================================================
// Content Schema
// ============================================================================

type signLanguageRefs = {
  bsl: option<string>,
  asl: option<string>,
  gsl: option<string>,
}

type contentAccessibility = {
  signLanguage: option<signLanguageRefs>,
  makaton: option<string>,
  easyRead: option<string>,
  audioDescription: option<string>,
  readingLevel: option<float>,
}

type contentFrontmatter = {
  title: string,
  date: option<string>,
  updated: option<string>,
  description: option<string>,
  author: option<authorConfig>,
  tags: option<array<string>>,
  draft: bool,
  slug: option<string>,
  template: option<string>,
  a11y: option<contentAccessibility>,
}

// ============================================================================
// Adapter Interface
// ============================================================================

type toolInputSchema = {
  @as("type") type_: string,
  properties: Js.Dict.t<Js.Json.t>,
  required: option<array<string>>,
}

type toolResult = {
  success: bool,
  stdout: string,
  stderr: string,
  code: int,
}

type adapterTool = {
  name: string,
  description: string,
  inputSchema: toolInputSchema,
  execute: Js.Dict.t<Js.Json.t> => promise<toolResult>,
}

type ssgAdapter = {
  name: string,
  language: string,
  description: string,
  connect: unit => promise<bool>,
  disconnect: unit => promise<unit>,
  isConnected: unit => bool,
  tools: array<adapterTool>,
}

// ============================================================================
// Template Types
// ============================================================================

type templateContext = {
  site: siteConfig,
  page: contentFrontmatter,
  pageContent: string,
  collections: option<Js.Dict.t<array<contentFrontmatter>>>,
}

type templateEngine = {
  name: string,
  extensions: array<string>,
  render: (string, templateContext) => promise<string>,
}

// ============================================================================
// Build Pipeline
// ============================================================================

type pipelineContext = {
  config: siteConfig,
  content: Js.Dict.t<{"frontmatter": contentFrontmatter, "raw": string, "content": string}>,
  templates: Js.Dict.t<string>,
  output: Js.Dict.t<string>,
  errors: array<string>,
  warnings: array<string>,
  metadata: Js.Dict.t<Js.Json.t>,
}

type pipelineStage = {
  name: string,
  order: int,
  enabled: bool,
  execute: pipelineContext => promise<pipelineContext>,
}
