# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# config.ncl — site configuration schema

let SiteConfig = {
  title : String,
  base_url : String,
  language : String | default = "en",
  description : String | optional,
  author : String | optional,
  theme : String | optional,

  build : {
    drafts : Bool | default = false,
    future : Bool | default = false,
    output_dir : String | default = "public",
    minify : Bool | default = true,
  },

  content : {
    directory : String | default = "content",
    extensions : Array String | default = ["md", "adoc", "org"],
  },

  templates : {
    directory : String | default = "templates",
    engine : String | default = "operation-card",
  },

  a11y : {
    enabled : Bool | default = true,
    sign_languages : Array String | default = [],
  },
} in

{
  # Validate configuration
  validate : SiteConfig -> SiteConfig,
  validate = fun config => config,

  # Default configuration
  default_config : SiteConfig,
  default_config = {
    title = "Eclipse SSG Site",
    base_url = "https://example.com",
    language = "en",
    description = "A site built with eclipse-ssg",
    build = {
      drafts = false,
      future = false,
      output_dir = "public",
      minify = true,
    },
    content = {
      directory = "content",
      extensions = ["md", "adoc", "org"],
    },
    templates = {
      directory = "templates",
      engine = "operation-card",
    },
    a11y = {
      enabled = true,
      sign_languages = [],
    },
  },

  # Export schema for external validation
  schema = SiteConfig,
}
