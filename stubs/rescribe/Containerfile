# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# Containerfile — rescribe-ssg
# Multi-stage build for ReScript Static Site Generator

# ============================================================================
# Stage 1: Build
# ============================================================================
FROM docker.io/library/node:20-alpine AS builder

LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/rescribe-ssg"
LABEL org.opencontainers.image.description="ReScript Static Site Generator"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

WORKDIR /build

# Copy package files first for better caching
COPY adapters/package*.json ./adapters/
RUN cd adapters && npm ci --only=production

# Copy ReScript source files
COPY adapters/rescript.json ./adapters/
COPY adapters/src/ ./adapters/src/
COPY src/ ./src/

# Build ReScript
RUN cd adapters && npm run build

# ============================================================================
# Stage 2: Runtime
# ============================================================================
FROM docker.io/library/node:20-alpine AS runtime

LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/rescribe-ssg"
LABEL org.opencontainers.image.description="ReScript Static Site Generator"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

# Create non-root user
RUN addgroup -g 1000 rescribe && \
    adduser -u 1000 -G rescribe -s /bin/sh -D rescribe

WORKDIR /app

# Copy built artifacts
COPY --from=builder /build/adapters/src/*.mjs ./adapters/src/
COPY --from=builder /build/src/ ./src/
COPY --from=builder /build/adapters/node_modules ./adapters/node_modules
COPY --from=builder /build/adapters/package.json ./adapters/

# Copy configuration files
COPY Justfile Mustfile ./
COPY *.scm ./

# Set ownership
RUN chown -R rescribe:rescribe /app

USER rescribe

# Default command
ENTRYPOINT ["node", "adapters/src/RescribeAdapter.mjs"]
CMD ["--help"]

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD node -e "console.log('healthy')" || exit 1
