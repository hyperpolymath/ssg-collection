#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Install wagasm-ssg git hooks

set -e

HOOKS_DIR="$(cd "$(dirname "$0")" && pwd)"
GIT_HOOKS_DIR="$(git rev-parse --git-dir)/hooks"

echo "Installing wagasm-ssg git hooks..."

for hook in pre-commit pre-push commit-msg; do
    if [ -f "$HOOKS_DIR/$hook" ]; then
        cp "$HOOKS_DIR/$hook" "$GIT_HOOKS_DIR/$hook"
        chmod +x "$GIT_HOOKS_DIR/$hook"
        echo "  Installed: $hook"
    fi
done

echo ""
echo "Git hooks installed successfully!"
echo ""
echo "Hooks will enforce:"
echo "  - Language compliance (WAT only in src/)"
echo "  - Compilation verification"
echo "  - Build testing"
echo "  - Conventional commit messages"
