#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# E2E Test: Verify HTML structure

set -e

echo "=== E2E Test: HTML Structure ==="

# Build the site
just build

# Check for DOCTYPE
if ! grep -q "<!DOCTYPE html>" _site/index.html; then
    echo "FAIL: Missing DOCTYPE declaration"
    exit 1
fi

# Check for html tag
if ! grep -q "<html>" _site/index.html; then
    echo "FAIL: Missing <html> tag"
    exit 1
fi

# Check for head section
if ! grep -q "<head>" _site/index.html; then
    echo "FAIL: Missing <head> section"
    exit 1
fi

# Check for body section
if ! grep -q "<body>" _site/index.html; then
    echo "FAIL: Missing <body> section"
    exit 1
fi

# Check for title
if ! grep -q "<title>" _site/index.html; then
    echo "FAIL: Missing <title> tag"
    exit 1
fi

# Check closing tags
if ! grep -q "</html>" _site/index.html; then
    echo "FAIL: Missing closing </html> tag"
    exit 1
fi

echo "PASS: HTML structure is valid"
