// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Sitemap generation

use anyhow::Result;
use quick_xml::events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event};
use quick_xml::Writer;
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

use crate::config::Config;
use crate::content::Page;

pub fn generate(pages: &[&Page], config: &Config, output_path: &Path) -> Result<()> {
    let file = File::create(output_path.join("sitemap.xml"))?;
    let mut writer = Writer::new(BufWriter::new(file));

    // XML declaration
    writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))?;

    // urlset element
    let mut urlset = BytesStart::new("urlset");
    urlset.push_attribute(("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9"));
    writer.write_event(Event::Start(urlset))?;

    // Add pages
    for page in pages {
        if page.front_matter.draft {
            continue;
        }

        // url element
        writer.write_event(Event::Start(BytesStart::new("url")))?;

        // loc element
        writer.write_event(Event::Start(BytesStart::new("loc")))?;
        let url = format!("{}/{}", config.site.url, page.slug);
        writer.write_event(Event::Text(BytesText::new(&url)))?;
        writer.write_event(Event::End(BytesEnd::new("loc")))?;

        // lastmod element (if date available)
        if let Some(date) = page.front_matter.date {
            writer.write_event(Event::Start(BytesStart::new("lastmod")))?;
            let date_str = date.format("%Y-%m-%d").to_string();
            writer.write_event(Event::Text(BytesText::new(&date_str)))?;
            writer.write_event(Event::End(BytesEnd::new("lastmod")))?;
        }

        // changefreq element
        writer.write_event(Event::Start(BytesStart::new("changefreq")))?;
        writer.write_event(Event::Text(BytesText::new("weekly")))?;
        writer.write_event(Event::End(BytesEnd::new("changefreq")))?;

        // priority element
        writer.write_event(Event::Start(BytesStart::new("priority")))?;
        let priority = if page.slug == "index" { "1.0" } else { "0.8" };
        writer.write_event(Event::Text(BytesText::new(priority)))?;
        writer.write_event(Event::End(BytesEnd::new("priority")))?;

        writer.write_event(Event::End(BytesEnd::new("url")))?;
    }

    // Close urlset
    writer.write_event(Event::End(BytesEnd::new("urlset")))?;

    Ok(())
}
