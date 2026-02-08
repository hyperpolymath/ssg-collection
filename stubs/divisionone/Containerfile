# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# Containerfile — Division One SSG Development Container
# Build: podman build -t divisionone-ssg .
# Run:   podman run -it --rm -v $(pwd):/app divisionone-ssg

FROM docker.io/denoland/deno:alpine-2.1.4

LABEL org.opencontainers.image.title="Division One SSG"
LABEL org.opencontainers.image.description="28 SSG adapters for the hyperpolymath ecosystem"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/divisionone-ssg"
LABEL org.opencontainers.image.licenses="MIT OR AGPL-3.0-or-later"

# Install additional tools
RUN apk add --no-cache \
    git \
    bash \
    just \
    && rm -rf /var/cache/apk/*

# Create non-root user
RUN addgroup -g 1000 ssg && \
    adduser -u 1000 -G ssg -h /home/ssg -s /bin/bash -D ssg

# Set working directory
WORKDIR /app

# Copy adapter files
COPY --chown=ssg:ssg adapters/ ./adapters/
COPY --chown=ssg:ssg justfile Mustfile ./

# Switch to non-root user
USER ssg

# Cache deno dependencies
RUN deno cache adapters/*.js 2>/dev/null || true

# Default command
CMD ["deno", "run", "--help"]

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD deno --version || exit 1
