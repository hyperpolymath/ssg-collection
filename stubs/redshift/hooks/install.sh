#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# Git hooks installer for shift-ssg
# Usage: ./hooks/install.sh

set -e

HOOKS_DIR="$(dirname "$0")"
GIT_HOOKS_DIR=".git/hooks"

echo "🔧 Installing git hooks..."

# Check if .git directory exists
if [ ! -d ".git" ]; then
    echo "❌ Not a git repository. Run from project root."
    exit 1
fi

# Create hooks directory if needed
mkdir -p "$GIT_HOOKS_DIR"

# Install each hook
for hook in pre-commit pre-push commit-msg; do
    if [ -f "$HOOKS_DIR/$hook" ]; then
        cp "$HOOKS_DIR/$hook" "$GIT_HOOKS_DIR/$hook"
        chmod +x "$GIT_HOOKS_DIR/$hook"
        echo "✅ Installed: $hook"
    fi
done

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "  ✅ Git hooks installed successfully"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Installed hooks:"
echo "  • pre-commit  - Syntax, format, and security checks"
echo "  • pre-push    - Full validation before push"
echo "  • commit-msg  - Conventional commits validation"
echo ""
echo "To uninstall: rm .git/hooks/{pre-commit,pre-push,commit-msg}"
