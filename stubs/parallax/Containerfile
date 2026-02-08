# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
# parallax-ssg Container Image

# Stage 1: Chapel build environment
FROM docker.io/chapel/chapel:1.33.0 AS builder

WORKDIR /build

# Copy Chapel source
COPY src/ src/

# Build optimized binary
RUN chpl src/parallel-press.chpl -o parallax-ssg --fast --optimize

# Stage 2: Runtime image
FROM docker.io/library/debian:bookworm-slim AS runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

# Copy built binary
COPY --from=builder /build/parallax-ssg /usr/local/bin/parallax-ssg

# Create non-root user
RUN useradd -m -s /bin/bash parallax
USER parallax

# Default command
ENTRYPOINT ["parallax-ssg"]
CMD ["build"]

# Labels
LABEL org.opencontainers.image.title="parallax-ssg"
LABEL org.opencontainers.image.description="Chapel Static Site Generator"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/parallax-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
