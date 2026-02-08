# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# hackenbush-ssg - Container Definition
# Uses Wolfi base for minimal, secure container

FROM cgr.dev/chainguard/wolfi-base:latest AS base

# Install Deno runtime
RUN apk add --no-cache deno

# Create non-root user
RUN adduser -D -u 1000 hackenbush
USER hackenbush
WORKDIR /home/hackenbush/app

# Copy runtime and patterns
COPY --chown=hackenbush:hackenbush runtime/ ./runtime/
COPY --chown=hackenbush:hackenbush src/ ./src/
COPY --chown=hackenbush:hackenbush patterns/ ./patterns/
COPY --chown=hackenbush:hackenbush deno.json ./

# Verify installation
RUN deno check runtime/host.ts

# Default command - run the Life simulator
ENTRYPOINT ["deno", "run", "--allow-read", "--allow-write", "runtime/host.ts"]
CMD ["evolve"]

# --- Development Stage ---
FROM base AS dev

# Copy additional development files
COPY --chown=hackenbush:hackenbush adapters/ ./adapters/
COPY --chown=hackenbush:hackenbush tests/ ./tests/

# Install ReScript for adapter development
RUN apk add --no-cache nodejs npm
RUN cd adapters && npm install

# Development entrypoint
CMD ["deno", "task", "run"]

# --- CI Stage ---
FROM base AS ci

# Copy test files
COPY --chown=hackenbush:hackenbush tests/ ./tests/

# Run tests as default
CMD ["deno", "test", "--allow-read", "--allow-write"]

# Labels for container metadata
LABEL org.opencontainers.image.title="hackenbush-ssg"
LABEL org.opencontainers.image.description="Game of Life Static Site Generator"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/hackenbush-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="hyperpolymath"
