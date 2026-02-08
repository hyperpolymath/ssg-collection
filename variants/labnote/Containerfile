# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile for labnote-ssg
# Build: podman build -t labnote-ssg .
# Run: podman run --rm labnote-ssg deno test

FROM docker.io/denoland/deno:1.40.0

LABEL org.opencontainers.image.title="labnote-ssg"
LABEL org.opencontainers.image.description="MCP adapter hub for 28 static site generators"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/labnote-ssg"
LABEL org.opencontainers.image.licenses="MIT OR AGPL-3.0-or-later"

WORKDIR /app

# Copy adapter files
COPY adapters/ ./adapters/

# Copy test files
COPY tests/ ./tests/

# Copy documentation
COPY README.adoc cookbook.adoc ./

# Cache dependencies (if any)
RUN deno cache adapters/*.js || true

# Default command: run tests
CMD ["deno", "test", "--allow-run", "--allow-read", "tests/"]
