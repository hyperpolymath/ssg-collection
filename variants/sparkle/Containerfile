# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile — sparkle-ssg
# Multi-stage build for sparkle-ssg satellite

# ═══════════════════════════════════════════════════════════════════════════════
# Stage 1: Build/Verify
# ═══════════════════════════════════════════════════════════════════════════════
FROM docker.io/denoland/deno:alpine AS builder

WORKDIR /app

# Copy source files
COPY adapters/ ./adapters/
COPY deno.json ./

# Type check and lint
RUN deno check adapters/*.js && \
    deno lint adapters/

# ═══════════════════════════════════════════════════════════════════════════════
# Stage 2: Runtime
# ═══════════════════════════════════════════════════════════════════════════════
FROM docker.io/denoland/deno:alpine AS runtime

LABEL org.opencontainers.image.title="sparkle-ssg"
LABEL org.opencontainers.image.description="Unified MCP adapters for 28 static site generators"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/sparkle-ssg"
LABEL org.opencontainers.image.licenses="MIT OR AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="hyperpolymath"

WORKDIR /app

# Copy verified adapters
COPY --from=builder /app/adapters/ ./adapters/
COPY --from=builder /app/deno.json ./

# Security: Run as non-root
RUN addgroup -g 1000 sparkle && \
    adduser -u 1000 -G sparkle -s /bin/sh -D sparkle && \
    chown -R sparkle:sparkle /app

USER sparkle

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD deno eval "console.log('ok')" || exit 1

# Default command: show adapter list
CMD ["deno", "eval", "import('./adapters/mod.js').then(m => console.log(Object.keys(m.adapters)))"]
