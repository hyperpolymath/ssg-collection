# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Containerfile — iota-ssg

FROM denoland/deno:1.40.0

LABEL org.opencontainers.image.title="iota-ssg"
LABEL org.opencontainers.image.description="SSG Adapter Collection for MCP"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/iota-ssg"
LABEL org.opencontainers.image.licenses="MIT OR AGPL-3.0-or-later"

WORKDIR /app

# Copy adapter files
COPY adapters/ ./adapters/

# Copy documentation
COPY README.adoc LICENSE.txt ./

# Verify adapters can be loaded
RUN for f in adapters/*.js; do \
      name=$(basename "$f" .js); \
      [ "$name" = "README" ] && continue; \
      deno eval "import * as a from './$f'; console.log('Loaded:', a.name)" || exit 1; \
    done

# Default command shows available adapters
CMD ["deno", "eval", "console.log('iota-ssg - SSG Adapter Collection'); console.log('Adapters:', await Deno.readDir('./adapters').then(d => [...d].filter(f => f.name.endsWith('.js') && f.name !== 'README.js').map(f => f.name.replace('.js', ''))))"]
