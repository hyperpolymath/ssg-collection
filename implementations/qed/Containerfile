# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile — qed-ssg development and runtime container

# =============================================================================
# STAGE 1: Base image with Deno
# =============================================================================
FROM docker.io/denoland/deno:alpine-1.40.0 AS base

LABEL org.opencontainers.image.title="qed-ssg"
LABEL org.opencontainers.image.description="MCP adapters for 28 static site generators"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/qed-ssg"
LABEL org.opencontainers.image.licenses="MIT OR AGPL-3.0-or-later"

WORKDIR /app

# =============================================================================
# STAGE 2: Development image
# =============================================================================
FROM base AS development

# Install development tools
RUN apk add --no-cache \
    bash \
    git \
    curl \
    just

# Copy source code
COPY . .

# Cache Deno dependencies
RUN deno cache adapters/*.js

# Development command
CMD ["deno", "test", "--watch", "adapters/"]

# =============================================================================
# STAGE 3: Test runner image
# =============================================================================
FROM base AS test

COPY . .

# Run all tests
RUN deno test --allow-read --allow-run adapters/

CMD ["deno", "test", "--allow-all"]

# =============================================================================
# STAGE 4: Production image (minimal)
# =============================================================================
FROM base AS production

# Create non-root user
RUN addgroup -g 1000 qedssg && \
    adduser -u 1000 -G qedssg -s /bin/sh -D qedssg

# Copy only necessary files
COPY --chown=qedssg:qedssg adapters/ /app/adapters/
COPY --chown=qedssg:qedssg config.ncl /app/

USER qedssg

# Cache dependencies
RUN deno cache adapters/*.js

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD deno eval "console.log('healthy')" || exit 1

# Default command: start REPL with adapters loaded
CMD ["deno", "repl", "--eval", "import * as adapters from './adapters/zola.js'"]

# =============================================================================
# STAGE 5: CI image (for GitHub Actions)
# =============================================================================
FROM base AS ci

RUN apk add --no-cache \
    bash \
    git \
    just

COPY . .

# Pre-cache all dependencies
RUN deno cache adapters/*.js

# Run CI checks
CMD ["sh", "-c", "deno lint adapters/ && deno test --allow-read --allow-run adapters/"]
