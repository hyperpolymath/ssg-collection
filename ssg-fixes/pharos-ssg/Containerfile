# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
#
# NoteG SSG - Container Definition
# Multi-stage build for minimal production image

# ============================================================================
# Stage 1: Build Ada/SPARK engine
# ============================================================================
FROM docker.io/library/debian:bookworm-slim AS ada-builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    gnat \
    gprbuild \
    spark2014 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build/engine

COPY engine/ ./

RUN gprbuild -P noteg_engine.gpr -XMODE=release

# ============================================================================
# Stage 2: Build ReScript components
# ============================================================================
FROM docker.io/library/node:22-slim AS rescript-builder

WORKDIR /build

# Install dependencies
COPY ssg/package*.json ./ssg/
COPY noteg-lang/package*.json ./noteg-lang/
RUN cd ssg && npm ci --production=false
RUN cd noteg-lang && npm ci --production=false

# Copy source and build
COPY ssg/ ./ssg/
COPY noteg-lang/ ./noteg-lang/

RUN cd ssg && npx rescript build
RUN cd noteg-lang && npx rescript build

# ============================================================================
# Stage 3: Production runtime
# ============================================================================
FROM docker.io/denoland/deno:debian-1.39.0 AS runtime

LABEL org.opencontainers.image.title="NoteG SSG"
LABEL org.opencontainers.image.description="Mill-based static site generator"
LABEL org.opencontainers.image.version="0.1.0"
LABEL org.opencontainers.image.vendor="hyperpolymath"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

# Create non-root user
RUN groupadd -r noteg && useradd -r -g noteg noteg

WORKDIR /app

# Copy Ada engine binary
COPY --from=ada-builder /build/engine/bin/noteg /app/bin/noteg

# Copy ReScript builds
COPY --from=rescript-builder /build/ssg/src/*.mjs /app/ssg/
COPY --from=rescript-builder /build/noteg-lang/src/*.mjs /app/lang/
COPY --from=rescript-builder /build/noteg-lang/src/lsp/*.mjs /app/lang/lsp/

# Copy adapters and schemas
COPY adapters/ /app/adapters/
COPY a11y/ /app/a11y/

# Create directories for content and output
RUN mkdir -p /app/content /app/public /app/templates && \
    chown -R noteg:noteg /app

# Switch to non-root user
USER noteg

# Environment
ENV NOTEG_CONFIG=/app/noteg.config.json
ENV NOTEG_OUTPUT_DIR=/app/public
ENV NOTEG_CONTENT_DIR=/app/content
ENV DENO_DIR=/app/.deno

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD test -f /app/bin/noteg || exit 1

# Default command
ENTRYPOINT ["/app/bin/noteg"]
CMD ["build"]
