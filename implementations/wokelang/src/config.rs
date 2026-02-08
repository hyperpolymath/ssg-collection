// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Site configuration

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub site: SiteConfig,
    #[serde(default)]
    pub build: BuildConfig,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SiteConfig {
    pub title: String,
    pub url: String,
    pub description: String,
    #[serde(default)]
    pub author: String,
    #[serde(default)]
    pub language: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct BuildConfig {
    #[serde(default = "default_content_dir")]
    pub content_dir: String,
    #[serde(default = "default_output_dir")]
    pub output_dir: String,
    #[serde(default = "default_templates_dir")]
    pub templates_dir: String,
    #[serde(default = "default_static_dir")]
    pub static_dir: String,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            content_dir: default_content_dir(),
            output_dir: default_output_dir(),
            templates_dir: default_templates_dir(),
            static_dir: default_static_dir(),
        }
    }
}

fn default_content_dir() -> String {
    "content".to_string()
}

fn default_output_dir() -> String {
    "public".to_string()
}

fn default_templates_dir() -> String {
    "templates".to_string()
}

fn default_static_dir() -> String {
    "static".to_string()
}

impl Config {
    pub fn load(path: &str) -> Result<Self> {
        let content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read config file: {}", path))?;
        let config: Config = serde_yaml::from_str(&content)
            .with_context(|| format!("Failed to parse config file: {}", path))?;
        Ok(config)
    }

    pub fn default_wokelang() -> Self {
        Self {
            site: SiteConfig {
                title: "WokeLang".to_string(),
                url: "https://wokelang.org".to_string(),
                description: "A Human-Centered Programming Language".to_string(),
                author: "Jonathan D.A. Jewell".to_string(),
                language: "en".to_string(),
            },
            build: BuildConfig::default(),
        }
    }
}
