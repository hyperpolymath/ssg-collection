// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Content parsing and processing

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use pulldown_cmark::{html, Options, Parser};
use serde::{Deserialize, Serialize};
use std::path::Path;
use syntect::highlighting::ThemeSet;
use syntect::html::highlighted_html_for_string;
use syntect::parsing::SyntaxSet;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FrontMatter {
    pub title: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub date: Option<DateTime<Utc>>,
    #[serde(default)]
    pub draft: bool,
    #[serde(default)]
    pub tags: Vec<String>,
    #[serde(default)]
    pub template: String,
}

impl Default for FrontMatter {
    fn default() -> Self {
        Self {
            title: String::new(),
            description: String::new(),
            date: None,
            draft: false,
            tags: Vec::new(),
            template: "page.html".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Page {
    pub front_matter: FrontMatter,
    pub content: String,
    pub html: String,
    pub slug: String,
}

impl Page {
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {}", path.display()))?;

        let (front_matter, markdown) = parse_front_matter(&content)?;

        let html = markdown_to_html(&markdown)?;

        let slug = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("index")
            .to_string();

        Ok(Self {
            front_matter,
            content: markdown,
            html,
            slug,
        })
    }
}

fn parse_front_matter(content: &str) -> Result<(FrontMatter, String)> {
    if let Some(content) = content.strip_prefix("---\n") {
        if let Some(end) = content.find("\n---\n") {
            let yaml = &content[..end];
            let markdown = &content[end + 5..];

            let front_matter: FrontMatter = serde_yaml::from_str(yaml)
                .with_context(|| "Failed to parse front matter")?;

            return Ok((front_matter, markdown.to_string()));
        }
    }

    // No front matter found
    Ok((FrontMatter::default(), content.to_string()))
}

fn markdown_to_html(markdown: &str) -> Result<String> {
    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TABLES);
    options.insert(Options::ENABLE_FOOTNOTES);
    options.insert(Options::ENABLE_TASKLISTS);
    options.insert(Options::ENABLE_HEADING_ATTRIBUTES);

    let parser = Parser::new_ext(markdown, options);
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);

    Ok(html_output)
}

pub fn highlight_code(code: &str, language: &str) -> Result<String> {
    let syntax_set = SyntaxSet::load_defaults_newlines();
    let theme_set = ThemeSet::load_defaults();

    let syntax = syntax_set
        .find_syntax_by_extension(language)
        .or_else(|| syntax_set.find_syntax_by_name(language))
        .unwrap_or_else(|| syntax_set.find_syntax_plain_text());

    let html = highlighted_html_for_string(code, &syntax_set, syntax, &theme_set.themes["base16-ocean.dark"])
        .with_context(|| "Failed to highlight code")?;

    Ok(html)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_front_matter() {
        let content = r#"---
title: Test Page
description: A test page
---
# Hello World

This is content."#;

        let (fm, md) = parse_front_matter(content).unwrap();
        assert_eq!(fm.title, "Test Page");
        assert_eq!(fm.description, "A test page");
        assert!(md.contains("# Hello World"));
    }

    #[test]
    fn test_markdown_to_html() {
        let md = "# Hello\n\nThis is **bold**.";
        let html = markdown_to_html(md).unwrap();
        assert!(html.contains("<h1>"));
        assert!(html.contains("<strong>bold</strong>"));
    }
}
