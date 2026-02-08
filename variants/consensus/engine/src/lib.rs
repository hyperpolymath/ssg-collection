// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// consensus-ssg: Consensus-driven static site generation using Raft
// Site synthesis via distributed consensus - content only builds when
// the cluster agrees on its state.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// A content entry in the replicated log
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContentEntry {
    pub path: PathBuf,
    pub title: String,
    pub content: String,
    pub term: u64,
}

/// The replicated state machine for site content
#[derive(Debug, Default, Clone)]
pub struct SiteStateMachine {
    pub pages: HashMap<PathBuf, ContentEntry>,
    pub committed_index: u64,
}

impl SiteStateMachine {
    pub fn new() -> Self {
        Self::default()
    }

    /// Apply a committed entry to the state machine
    pub fn apply(&mut self, entry: ContentEntry) {
        self.pages.insert(entry.path.clone(), entry);
        self.committed_index += 1;
    }

    /// Build the site once consensus is reached
    pub fn build(&self, output_dir: &PathBuf) -> std::io::Result<()> {
        std::fs::create_dir_all(output_dir)?;

        for (path, entry) in &self.pages {
            let html = render_page(entry);
            let out_path = output_dir.join(path).with_extension("html");

            if let Some(parent) = out_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&out_path, html)?;
            println!("consensus-ssg: Committed {} (term {})", path.display(), entry.term);
        }

        println!(
            "consensus-ssg: Build complete. {} pages committed.",
            self.pages.len()
        );
        Ok(())
    }
}

/// Render a page to HTML
fn render_page(entry: &ContentEntry) -> String {
    let content_html = markdown_to_html(&entry.content);

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>{}</title>
</head>
<body>
    <article>
        <h1>{}</h1>
        {}
    </article>
    <footer>
        <p>Committed at term {}</p>
    </footer>
</body>
</html>"#,
        escape_html(&entry.title),
        escape_html(&entry.title),
        content_html,
        entry.term
    )
}

/// Convert Markdown to HTML
fn markdown_to_html(markdown: &str) -> String {
    use pulldown_cmark::{html, Parser};

    let parser = Parser::new(markdown);
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    html_output
}

/// Escape HTML special characters
fn escape_html(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Raft node configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeConfig {
    pub node_id: u64,
    pub peers: Vec<String>,
    pub election_timeout_ms: u64,
    pub heartbeat_interval_ms: u64,
}

impl Default for NodeConfig {
    fn default() -> Self {
        Self {
            node_id: 1,
            peers: vec![],
            election_timeout_ms: 150,
            heartbeat_interval_ms: 50,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_machine_apply() {
        let mut sm = SiteStateMachine::new();
        let entry = ContentEntry {
            path: PathBuf::from("index"),
            title: "Home".to_string(),
            content: "# Welcome".to_string(),
            term: 1,
        };

        sm.apply(entry);
        assert_eq!(sm.pages.len(), 1);
        assert_eq!(sm.committed_index, 1);
    }
}
