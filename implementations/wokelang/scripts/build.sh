#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Build script for wokelang-ssg

set -e

echo "🏗️  Building wokelang-ssg..."

# TODO: Build frontend (ReScript) when a2ml is published to npm
# For now, skip ReScript build and use static HTML
# echo "📦 Building ReScript frontend..."
# cd frontend
# deno task build
# cd ..

# TODO: Run WokeLang SSG (when wokelang interpreter is ready)
# echo "🚀 Running WokeLang SSG..."
# wokelang ssg/main.woke

# For now, create dist/ with frontend build
echo "📁 Creating dist/ directory..."
mkdir -p dist
cp -r frontend/src/index.html dist/
cp -r frontend/lib/ dist/lib/ 2>/dev/null || echo "No lib/ directory yet"
cp -r frontend/static/* dist/ 2>/dev/null || echo "No static files yet"

# Create a basic index.html if it doesn't exist
if [ ! -f dist/index.html ]; then
  cat > dist/index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>WokeLang</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen-Sans, Ubuntu, Cantarell, sans-serif;
      max-width: 800px;
      margin: 0 auto;
      padding: 2rem;
      line-height: 1.6;
    }
    h1 {
      color: #4338ca;
    }
    code {
      background: #f3f4f6;
      padding: 0.2rem 0.4rem;
      border-radius: 0.25rem;
      font-family: monospace;
    }
  </style>
</head>
<body>
  <h1>WokeLang</h1>
  <p><strong>A Human-Centered Programming Language</strong></p>
  <p>WokeLang combines empathy, safety, and performance:</p>
  <ul>
    <li>🤝 <strong>Human Syntax</strong> - <code>to add(a, b) -&gt; a + b</code></li>
    <li>✅ <strong>Consent Gates</strong> - <code>only if okay "Delete?" { ... }</code></li>
    <li>🙏 <strong>Gratitude Blocks</strong> - <code>thanks to { "Alice" → "Fixed X!" }</code></li>
    <li>📏 <strong>Units of Measure</strong> - <code>5 measured in km</code></li>
    <li>⚡ <strong>Performance</strong> - Bytecode VM with optimization</li>
  </ul>
  <p>
    <a href="https://github.com/hyperpolymath/wokelang">View on GitHub</a> |
    <a href="https://github.com/hyperpolymath/wokelang-ssg">SSG Source</a>
  </p>
  <p><em>Site powered by wokelang-ssg (WokeLang + ReScript + TEA + a2ml)</em></p>
</body>
</html>
EOF
fi

echo "✅ Build complete! Output in dist/"
