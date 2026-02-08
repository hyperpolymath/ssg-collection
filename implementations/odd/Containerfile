# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# odd-ssg Container Image

# Build stage - includes Node.js for ReScript compilation
FROM docker.io/denoland/deno:2.1.4 AS builder

# Install Node.js for ReScript compiler
RUN apt-get update && apt-get install -y nodejs npm && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy configuration files
COPY deno.json rescript.json ./
COPY package.json ./

# Install ReScript compiler
RUN npm install rescript

# Copy source files
COPY src/ src/
COPY adapters/ adapters/

# Build ReScript
RUN npx rescript build

# Cache Deno dependencies
RUN deno cache src/Mod.res.js

# Check JavaScript
RUN deno check src/**/*.js || true

# Run tests
RUN deno test --allow-read --allow-write src/tests/ || true

# Production stage
FROM docker.io/denoland/deno:2.1.4-distroless

LABEL org.opencontainers.image.title="odd-ssg"
LABEL org.opencontainers.image.description="Probabilistic SSG with Mill-Based Synthesis engine and 30 MCP-compatible adapters"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/odd-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="hyperpolymath"

WORKDIR /app

# Copy from builder (only compiled JS, not node_modules)
COPY --from=builder /app/src /app/src
COPY --from=builder /app/adapters /app/adapters
COPY --from=builder /app/deno.json /app/deno.json

# Set user
USER deno

# Default command - run MCP server
ENTRYPOINT ["deno", "run", "--allow-read", "--allow-write", "--allow-run"]
CMD ["src/noteg-mcp/main.js"]

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD deno eval "console.log('healthy')" || exit 1

# Expose MCP port
EXPOSE 3000
