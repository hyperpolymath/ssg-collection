#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Install git hooks for prodigy-ssg

set -e

HOOKS_DIR="$(dirname "$0")/../hooks"
GIT_HOOKS_DIR="$(dirname "$0")/../.git/hooks"

echo "Installing prodigy-ssg git hooks..."

# Create .git/hooks if it doesn't exist
mkdir -p "$GIT_HOOKS_DIR"

# Install each hook
for hook in pre-commit pre-push post-merge; do
    if [ -f "$HOOKS_DIR/$hook" ]; then
        cp "$HOOKS_DIR/$hook" "$GIT_HOOKS_DIR/$hook"
        chmod +x "$GIT_HOOKS_DIR/$hook"
        echo "  ✓ Installed $hook"
    fi
done

echo "✅ Git hooks installed!"
echo ""
echo "Hooks will run automatically on:"
echo "  - pre-commit: Language and syntax checks"
echo "  - pre-push: Test execution and security audit"
echo "  - post-merge: Dependency updates"
