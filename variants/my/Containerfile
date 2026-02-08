# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile for my-ssg - SSG Adapter Collection

FROM docker.io/denoland/deno:alpine-2.1.4

LABEL org.opencontainers.image.title="my-ssg"
LABEL org.opencontainers.image.description="SSG Adapter Collection - 28 Static Site Generator adapters via MCP"
LABEL org.opencontainers.image.version="0.1.0"
LABEL org.opencontainers.image.authors="Jonathan D.A. Jewell"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/my-ssg"

WORKDIR /app

# Copy adapters and tests
COPY adapters/ /app/adapters/
COPY tests/ /app/tests/

# Create non-root user
RUN adduser -D -u 1000 ssg
USER ssg

# Verify adapters on startup
CMD ["deno", "eval", " \
    const files = [...Deno.readDirSync('adapters')].filter(f => f.name.endsWith('.js')); \
    console.log('my-ssg: ' + files.length + ' SSG adapters available'); \
    for (const f of files) console.log('  -', f.name.replace('.js', '')); \
"]
