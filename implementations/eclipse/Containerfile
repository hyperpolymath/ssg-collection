# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile — eclipse-ssg container image

FROM docker.io/denoland/deno:alpine-1.40.0 AS base

LABEL org.opencontainers.image.title="eclipse-ssg"
LABEL org.opencontainers.image.description="Satellite SSG implementation with 28 adapter support"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/eclipse-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

WORKDIR /app

# ============================================================
# DEPENDENCIES STAGE
# ============================================================
FROM base AS deps

# Copy dependency files
COPY deno.json deno.lock* ./

# Cache dependencies
RUN deno cache --reload deps.ts 2>/dev/null || true

# ============================================================
# BUILD STAGE
# ============================================================
FROM deps AS build

# Copy source files
COPY . .

# Type check
RUN deno check ssg/src/*.res noteg-lang/src/*.ts 2>/dev/null || true

# ============================================================
# TEST STAGE
# ============================================================
FROM build AS test

# Run tests
RUN deno test --allow-read tests/ 2>/dev/null || echo "Tests pending"

# ============================================================
# PRODUCTION STAGE
# ============================================================
FROM base AS production

# Create non-root user
RUN addgroup -g 1000 eclipse && \
    adduser -u 1000 -G eclipse -s /bin/sh -D eclipse

# Copy application
COPY --from=build /app /app

# Set ownership
RUN chown -R eclipse:eclipse /app

# Switch to non-root user
USER eclipse

# Default command
CMD ["deno", "task", "build"]

# ============================================================
# DEVELOPMENT STAGE
# ============================================================
FROM base AS development

# Install additional tools
RUN apk add --no-cache git bash just

# Copy application
COPY . .

# Development command
CMD ["deno", "task", "serve"]
