// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Template rendering with Tera

use anyhow::{Context, Result};
use serde::Serialize;
use tera::{Tera, Context as TeraContext};
use std::path::Path;

use crate::config::Config;
use crate::content::Page;

pub struct Templates {
    tera: Tera,
}

#[derive(Serialize)]
struct PageContext<'a> {
    page: &'a Page,
    site: &'a crate::config::SiteConfig,
}

impl Templates {
    pub fn load(templates_dir: &str) -> Result<Self> {
        let pattern = format!("{}/**/*.html", templates_dir);
        let tera = Tera::new(&pattern)
            .with_context(|| format!("Failed to load templates from {}", templates_dir))?;

        Ok(Self { tera })
    }

    pub fn render(&self, template_name: &str, page: &Page, config: &Config) -> Result<String> {
        let mut context = TeraContext::new();

        // Add page data
        context.insert("page", &PageContextData {
            title: &page.front_matter.title,
            description: &page.front_matter.description,
            html: &page.html,
            slug: &page.slug,
            date: page.front_matter.date.as_ref().map(|d| d.to_rfc3339()),
            tags: &page.front_matter.tags,
        });

        // Add site config
        context.insert("site", &config.site);

        self.tera
            .render(template_name, &context)
            .with_context(|| format!("Failed to render template: {}", template_name))
    }
}

#[derive(Serialize)]
struct PageContextData<'a> {
    title: &'a str,
    description: &'a str,
    html: &'a str,
    slug: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    date: Option<String>,
    tags: &'a [String],
}
