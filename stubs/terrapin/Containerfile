# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# terrapin-ssg Containerfile - Logo Static Site Generator

FROM debian:bookworm-slim AS base

LABEL org.opencontainers.image.title="terrapin-ssg"
LABEL org.opencontainers.image.description="Educational static site generator in Logo"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/terrapin-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

# Install UCBLogo and just
RUN apt-get update && apt-get install -y --no-install-recommends \
    ucblogo \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install just (command runner)
RUN curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

WORKDIR /app

# Copy only Logo source and build files
COPY src/ src/
COPY Justfile Mustfile ./
COPY adapters/ adapters/

# Create output directory
RUN mkdir -p _site

# Default command: build the site
CMD ["just", "build"]

# === Multi-stage for development ===
FROM base AS dev

# Install additional tools for development
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    nodejs \
    npm \
    python3 \
    entr \
    && rm -rf /var/lib/apt/lists/*

# Copy all files for development
COPY . .

# Development command: watch mode
CMD ["just", "watch"]

# === Multi-stage for CI ===
FROM base AS ci

COPY . .

# Run full CI pipeline
CMD ["just", "ci"]
