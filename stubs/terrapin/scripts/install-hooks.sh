#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Install git hooks for terrapin-ssg

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
GIT_HOOKS_DIR="$REPO_ROOT/.git/hooks"

echo "🐢 Installing terrapin-ssg git hooks..."

# Create hooks directory if it doesn't exist
mkdir -p "$GIT_HOOKS_DIR"

# Install pre-commit hook
if [ -f "$SCRIPT_DIR/hooks/pre-commit" ]; then
    cp "$SCRIPT_DIR/hooks/pre-commit" "$GIT_HOOKS_DIR/pre-commit"
    chmod +x "$GIT_HOOKS_DIR/pre-commit"
    echo "  ✓ Installed pre-commit hook"
fi

# Install pre-push hook
if [ -f "$SCRIPT_DIR/hooks/pre-push" ]; then
    cp "$SCRIPT_DIR/hooks/pre-push" "$GIT_HOOKS_DIR/pre-push"
    chmod +x "$GIT_HOOKS_DIR/pre-push"
    echo "  ✓ Installed pre-push hook"
fi

echo "✓ Git hooks installed successfully"
echo ""
echo "To uninstall, remove files from .git/hooks/"
