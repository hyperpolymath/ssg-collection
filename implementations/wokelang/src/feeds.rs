// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! RSS and Atom feed generation

use anyhow::Result;
use atom_syndication::{Entry, Feed, Link, Person};
use chrono::Utc;
use rss::{Channel, ChannelBuilder, Item, ItemBuilder};
use std::fs;
use std::path::Path;

use crate::config::Config;
use crate::content::Page;

pub fn generate_rss(pages: &[&Page], config: &Config, output_path: &Path) -> Result<()> {
    let mut items = Vec::new();

    for page in pages {
        if page.front_matter.draft {
            continue;
        }

        let item = ItemBuilder::default()
            .title(Some(page.front_matter.title.clone()))
            .description(Some(page.front_matter.description.clone()))
            .link(Some(format!("{}/{}", config.site.url, page.slug)))
            .pub_date(page.front_matter.date.map(|d| d.to_rfc2822()))
            .build();

        items.push(item);
    }

    let channel = ChannelBuilder::default()
        .title(&config.site.title)
        .link(&config.site.url)
        .description(&config.site.description)
        .language(Some(config.site.language.clone()))
        .items(items)
        .build();

    let rss_path = output_path.join("feed.xml");
    let rss_string = channel.to_string();
    fs::write(rss_path, rss_string)?;

    Ok(())
}

pub fn generate_atom(pages: &[&Page], config: &Config, output_path: &Path) -> Result<()> {
    let mut entries = Vec::new();

    for page in pages {
        if page.front_matter.draft {
            continue;
        }

        let entry = Entry {
            title: page.front_matter.title.clone().into(),
            id: format!("{}/{}", config.site.url, page.slug),
            updated: page.front_matter.date.unwrap_or_else(Utc::now),
            links: vec![Link {
                href: format!("{}/{}", config.site.url, page.slug),
                ..Default::default()
            }],
            summary: Some(page.front_matter.description.clone().into()),
            ..Default::default()
        };

        entries.push(entry);
    }

    let feed = Feed {
        title: config.site.title.clone().into(),
        id: config.site.url.clone(),
        updated: Utc::now(),
        authors: vec![Person {
            name: config.site.author.clone(),
            ..Default::default()
        }],
        links: vec![Link {
            href: config.site.url.clone(),
            ..Default::default()
        }],
        entries,
        ..Default::default()
    };

    let atom_path = output_path.join("atom.xml");
    let atom_string = feed.to_string();
    fs::write(atom_path, atom_string)?;

    Ok(())
}
