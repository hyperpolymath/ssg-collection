// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Site builder - orchestrates the build process

use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

use crate::config::Config;
use crate::content::Page;
use crate::feeds;
use crate::sitemap;
use crate::templates::Templates;

pub fn init_site(path: &str) -> Result<()> {
    let base = Path::new(path);

    // Create directory structure
    fs::create_dir_all(base.join("content"))?;
    fs::create_dir_all(base.join("content/pages"))?;
    fs::create_dir_all(base.join("content/docs"))?;
    fs::create_dir_all(base.join("templates"))?;
    fs::create_dir_all(base.join("static"))?;
    fs::create_dir_all(base.join("static/css"))?;
    fs::create_dir_all(base.join("static/js"))?;

    // Create config file
    let config = crate::config::Config::default_wokelang();
    let config_yaml = serde_yaml::to_string(&config)?;
    fs::write(base.join("config.yaml"), config_yaml)?;

    // Create sample content
    let sample_index = r#"---
title: Welcome to WokeLang
description: A Human-Centered Programming Language
template: index.html
---

# Welcome to WokeLang

WokeLang is a programming language designed with human values at its core:

- **Consent-driven computing** - Programs request permission before accessing resources
- **Gratitude system** - Dependencies are acknowledged explicitly
- **Emotive programming** - Code can express tone and emotion
- **Accessibility** - Natural language syntax for everyone

## Quick Start

```woke
to main() {
    print("Hello, World!");
}
```

Learn more in the [documentation](/docs/getting-started).
"#;

    fs::write(base.join("content/index.md"), sample_index)?;

    // Create sample template
    let sample_template = r#"<!DOCTYPE html>
<html lang="{{ site.language }}">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{ page.title }} | {{ site.title }}</title>
    <meta name="description" content="{{ page.description | default(value=site.description) }}">
    <link rel="stylesheet" href="/css/style.css">
</head>
<body>
    <header>
        <h1>{{ site.title }}</h1>
        <nav>
            <a href="/">Home</a>
            <a href="/docs/">Documentation</a>
            <a href="/playground/">Playground</a>
        </nav>
    </header>
    <main>
        {{ page.html | safe }}
    </main>
    <footer>
        <p>&copy; 2026 {{ site.author }}. Released under PMPL-1.0-or-later.</p>
    </footer>
</body>
</html>
"#;

    fs::write(base.join("templates/index.html"), sample_template)?;
    fs::write(base.join("templates/page.html"), sample_template)?;

    println!("✓ Site initialized at {}", path);
    Ok(())
}

pub fn build_site(config: &Config, source: &str, output: &str) -> Result<()> {
    let source_path = Path::new(source);
    let output_path = Path::new(output);

    // Create output directory
    fs::create_dir_all(output_path)?;

    // Load templates
    let templates = Templates::load(&config.build.templates_dir)?;

    // Collect all pages
    let mut pages = Vec::new();
    for entry in WalkDir::new(source_path)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "md"))
    {
        let page = Page::from_file(entry.path())?;
        pages.push((entry.path().to_path_buf(), page));
    }

    // Render pages
    for (path, page) in &pages {
        let relative = path
            .strip_prefix(source_path)
            .unwrap_or(path);

        let output_file = if relative.file_stem().and_then(|s| s.to_str()) == Some("index") {
            output_path.join(relative.parent().unwrap_or(Path::new(""))).join("index.html")
        } else {
            output_path
                .join(relative.parent().unwrap_or(Path::new("")))
                .join(format!("{}.html", page.slug))
        };

        fs::create_dir_all(output_file.parent().unwrap())?;

        let html = templates.render(&page.front_matter.template, page, config)?;
        fs::write(output_file, html)?;
    }

    // Copy static assets
    if Path::new(&config.build.static_dir).exists() {
        copy_dir_all(&config.build.static_dir, output_path)?;
    }

    // Generate feeds
    let all_pages: Vec<&Page> = pages.iter().map(|(_, p)| p).collect();
    feeds::generate_rss(&all_pages, config, output_path)?;
    feeds::generate_atom(&all_pages, config, output_path)?;

    // Generate sitemap
    sitemap::generate(&all_pages, config, output_path)?;

    println!("✓ Site built successfully");
    Ok(())
}

fn copy_dir_all(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result<()> {
    fs::create_dir_all(&dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        if ty.is_dir() {
            copy_dir_all(entry.path(), dst.as_ref().join(entry.file_name()))?;
        } else {
            fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}
