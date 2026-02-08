# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile — orbital-ssg

FROM denoland/deno:2.1.4

LABEL org.opencontainers.image.title="orbital-ssg"
LABEL org.opencontainers.image.description="MCP adapters for 28 static site generators"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/orbital-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

WORKDIR /app

# Copy adapter files
COPY adapters/ ./adapters/
COPY tests/ ./tests/
COPY deno.json ./

# Cache dependencies
RUN deno cache adapters/*.js

# Set permissions
USER deno

# Default command
CMD ["deno", "test", "--allow-run", "--allow-read", "tests/"]
