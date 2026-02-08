# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile — baremetal-ssg

FROM denoland/deno:distroless-1.40.0

LABEL org.opencontainers.image.title="baremetal-ssg"
LABEL org.opencontainers.image.description="Bare-metal SSG adapters for 28 static site generators"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/baremetal-ssg"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

WORKDIR /app

# Copy adapter files
COPY adapters/ ./adapters/

# Copy configuration
COPY deno.json ./

# Expose MCP port
EXPOSE 3000

# Run the MCP server
CMD ["run", "--allow-all", "main.js"]
