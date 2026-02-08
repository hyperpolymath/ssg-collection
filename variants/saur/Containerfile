# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
#
# Containerfile for saur-ssg
# Build: podman build -t saur-ssg:latest .
# Run:   podman run --rm -it saur-ssg:latest

# ─────────────────────────────────────────────────────────────────────────────
# Stage 1: Base image with Deno
# ─────────────────────────────────────────────────────────────────────────────
FROM docker.io/denoland/deno:alpine AS base

LABEL org.opencontainers.image.title="saur-ssg"
LABEL org.opencontainers.image.description="MCP-compatible SSG adapters for 28 static site generators"
LABEL org.opencontainers.image.version="0.1.0"
LABEL org.opencontainers.image.authors="hyperpolymath"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/saur-ssg"

# ─────────────────────────────────────────────────────────────────────────────
# Stage 2: Development image
# ─────────────────────────────────────────────────────────────────────────────
FROM base AS development

# Install development tools
RUN apk add --no-cache \
    bash \
    git \
    curl \
    jq

# Install just (task runner)
RUN curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

WORKDIR /app

# Copy project files
COPY . .

# Cache Deno dependencies
RUN deno cache adapters/*.js 2>/dev/null || true

# Default command
CMD ["deno", "repl", "--allow-all"]

# ─────────────────────────────────────────────────────────────────────────────
# Stage 3: Production image (minimal)
# ─────────────────────────────────────────────────────────────────────────────
FROM base AS production

WORKDIR /app

# Copy only necessary files
COPY adapters/ ./adapters/
COPY LICENSE.txt README.adoc ./

# Create non-root user
RUN addgroup -g 1000 saur && \
    adduser -u 1000 -G saur -s /bin/sh -D saur && \
    chown -R saur:saur /app

USER saur

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD deno eval "console.log('healthy')" || exit 1

# Default command - list available adapters
CMD ["deno", "eval", "import { readdirSync } from 'node:fs'; console.log('Available adapters:', readdirSync('./adapters').filter(f => f.endsWith('.js')).map(f => f.replace('.js', '')).join(', '))"]

# ─────────────────────────────────────────────────────────────────────────────
# Stage 4: Test image
# ─────────────────────────────────────────────────────────────────────────────
FROM development AS test

# Run tests
RUN if [ -d "tests" ]; then deno test --allow-read --allow-run tests/; fi

# ─────────────────────────────────────────────────────────────────────────────
# Default target
# ─────────────────────────────────────────────────────────────────────────────
FROM production
