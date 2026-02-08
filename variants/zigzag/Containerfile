# SPDX-License-Identifier: AGPL-3.0-or-later
# Containerfile for zigzag-ssg
# Build: podman build -t zigzag-ssg .
# Run: podman run --rm -v $(pwd):/site zigzag-ssg build

# ============================================================================
# Stage 1: Build
# ============================================================================
FROM docker.io/library/alpine:3.19 AS builder

# Install Zig
RUN apk add --no-cache \
    curl \
    xz \
    && curl -L https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz | tar xJ \
    && mv zig-linux-x86_64-0.11.0 /opt/zig

ENV PATH="/opt/zig:${PATH}"

# Copy source
WORKDIR /build
COPY build.zig .
COPY src/ src/

# Build release binary
RUN zig build -Doptimize=ReleaseFast

# ============================================================================
# Stage 2: Runtime
# ============================================================================
FROM docker.io/library/alpine:3.19

# Minimal runtime dependencies
RUN apk add --no-cache \
    ca-certificates

# Copy binary from builder
COPY --from=builder /build/zig-out/bin/zigzag-ssg /usr/local/bin/

# Create non-root user
RUN adduser -D -u 1000 zigzag
USER zigzag

# Working directory for site
WORKDIR /site

# Default command
ENTRYPOINT ["/usr/local/bin/zigzag-ssg"]
CMD ["--help"]

# Labels
LABEL org.opencontainers.image.title="zigzag-ssg"
LABEL org.opencontainers.image.description="High-performance static site generator in Zig"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/zigzag-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
