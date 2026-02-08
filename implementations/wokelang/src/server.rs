// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Development server with live reload

use anyhow::Result;
use axum::{routing::get_service, Router};
use std::net::SocketAddr;
use tower_http::services::ServeDir;
use tower_http::trace::TraceLayer;
use tower_livereload::LiveReloadLayer;

pub async fn serve(output_dir: &str, port: u16) -> Result<()> {
    let addr = SocketAddr::from(([127, 0, 0, 1], port));

    let serve_dir = ServeDir::new(output_dir);

    let app = Router::new()
        .fallback_service(get_service(serve_dir))
        .layer(LiveReloadLayer::new())
        .layer(TraceLayer::new_for_http());

    println!("🚀 Server running at http://127.0.0.1:{}", port);
    println!("   Serving: {}", output_dir);
    println!("   Live reload enabled");

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
