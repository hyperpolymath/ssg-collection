#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# hooks/install.sh — chicxulub--ssg
# Install git hooks for local development

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_HOOKS_DIR="$(git rev-parse --git-dir)/hooks"

echo "Installing git hooks..."

# Create hooks directory if it doesn't exist
mkdir -p "$GIT_HOOKS_DIR"

# Install pre-commit hook
if [[ -f "$SCRIPT_DIR/pre-commit" ]]; then
    cp "$SCRIPT_DIR/pre-commit" "$GIT_HOOKS_DIR/pre-commit"
    chmod +x "$GIT_HOOKS_DIR/pre-commit"
    echo "✓ Installed pre-commit hook"
fi

# Install pre-push hook
if [[ -f "$SCRIPT_DIR/pre-push" ]]; then
    cp "$SCRIPT_DIR/pre-push" "$GIT_HOOKS_DIR/pre-push"
    chmod +x "$GIT_HOOKS_DIR/pre-push"
    echo "✓ Installed pre-push hook"
fi

echo ""
echo "Hooks installed successfully!"
echo ""
echo "Configuration:"
echo "  - Edit hooks/config.sh for project-wide settings"
echo "  - Create hooks/config.local.sh for personal overrides (gitignored)"
echo "  - Set SKIP_HOOKS=1 to skip all hooks"
echo "  - Set VERBOSE=1 for detailed output"
echo ""
echo "To uninstall: rm .git/hooks/pre-commit .git/hooks/pre-push"
