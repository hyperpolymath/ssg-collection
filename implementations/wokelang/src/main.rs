// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! WokeLang Static Site Generator
//!
//! A Rust-based SSG for wokelang.org, featuring:
//! - Frontmatter parsing (YAML)
//! - Markdown to HTML conversion
//! - Template system
//! - Asset pipeline with hash-based cache busting
//! - RSS/Atom feeds
//! - Sitemap generation
//! - Live reload for development

mod builder;
mod config;
mod content;
mod feeds;
mod server;
mod sitemap;
mod templates;

use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "wokelang-ssg")]
#[command(about = "Static site generator for wokelang.org", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new site
    Init {
        /// Site directory
        #[arg(default_value = ".")]
        path: String,
    },
    /// Build the site
    Build {
        /// Source directory
        #[arg(short, long, default_value = "content")]
        source: String,
        /// Output directory
        #[arg(short, long, default_value = "public")]
        output: String,
    },
    /// Serve the site with live reload
    Serve {
        /// Source directory
        #[arg(short, long, default_value = "content")]
        source: String,
        /// Output directory
        #[arg(short, long, default_value = "public")]
        output: String,
        /// Server port
        #[arg(short, long, default_value = "3000")]
        port: u16,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { path } => {
            println!("Initializing new site at: {}", path);
            builder::init_site(&path)?;
        }
        Commands::Build { source, output } => {
            println!("Building site from {} to {}", source, output);
            let config = config::Config::load("config.yaml")?;
            builder::build_site(&config, &source, &output)?;
        }
        Commands::Serve {
            source,
            output,
            port,
        } => {
            println!("Building and serving site on port {}", port);
            let config = config::Config::load("config.yaml")?;
            builder::build_site(&config, &source, &output)?;
            server::serve(&output, port).await?;
        }
    }

    Ok(())
}
