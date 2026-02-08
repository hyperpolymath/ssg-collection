# SPDX-License-Identifier: AGPL-3.0-or-later
# prodigy-ssg Container Image
# Build: podman build -t prodigy-ssg:latest .
# Run:   podman run --rm -v ./content:/app/content:ro -v ./_site:/app/_site prodigy-ssg

FROM docker.io/library/debian:bookworm-slim AS builder

# Install SWI-Prolog
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        swi-prolog \
        ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy Prolog source files
COPY src/ ./src/
COPY engine/ ./engine/

# Verify Prolog syntax at build time
RUN swipl -g 'halt' src/prodigy.pl && \
    for f in engine/src/*.pl; do swipl -g 'halt' "$f"; done

# Production image
FROM docker.io/library/debian:bookworm-slim

# Security: Run as non-root user
RUN useradd -m -s /bin/bash prodigy

# Install runtime dependencies only
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        swi-prolog-nox && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy from builder
COPY --from=builder /app/src ./src
COPY --from=builder /app/engine ./engine

# Create directories for mounting
RUN mkdir -p content templates _site && \
    chown -R prodigy:prodigy /app

USER prodigy

# Default command: build site
ENTRYPOINT ["swipl", "-g", "consult('src/prodigy.pl'), build_site('_site'), halt", "-t", "halt(1)"]

# Labels
LABEL org.opencontainers.image.title="prodigy-ssg" \
      org.opencontainers.image.description="Prolog Static Site Generator" \
      org.opencontainers.image.source="https://github.com/hyperpolymath/prodigy-ssg" \
      org.opencontainers.image.licenses="AGPL-3.0-or-later"
