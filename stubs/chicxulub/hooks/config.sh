#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# hooks/config.sh — chicxulub--ssg
# Configuration file for git hooks
# Copy to hooks/config.local.sh for personal overrides (gitignored)

# ============================================================================
# PRE-COMMIT CONFIGURATION
# ============================================================================

# Enable/disable format checking (deno fmt --check)
ENABLE_FORMAT_CHECK=1

# Enable/disable linting (deno lint)
ENABLE_LINT=1

# Enable/disable type checking (deno check)
ENABLE_TYPECHECK=1

# Enable/disable secrets scanning
ENABLE_SECRETS_SCAN=1

# Enable/disable SPDX header checking
ENABLE_SPDX_CHECK=1

# ============================================================================
# PRE-PUSH CONFIGURATION
# ============================================================================

# Enable/disable running tests before push
ENABLE_TESTS=1

# Enable/disable full lint of all adapters
ENABLE_FULL_LINT=1

# Enable/disable adapter export validation
ENABLE_ADAPTER_VALIDATION=1

# Test timeout in seconds
TEST_TIMEOUT=300

# ============================================================================
# GENERAL CONFIGURATION
# ============================================================================

# Set to 1 to see verbose output
VERBOSE=0

# ============================================================================
# CUSTOM HOOKS
# ============================================================================

# Add custom pre-commit commands here
# custom_pre_commit() {
#     echo "Running custom pre-commit checks..."
#     # Your custom checks here
# }

# Add custom pre-push commands here
# custom_pre_push() {
#     echo "Running custom pre-push checks..."
#     # Your custom checks here
# }

# ============================================================================
# ENVIRONMENT-SPECIFIC OVERRIDES
# ============================================================================

# Load local overrides if they exist
if [[ -f "hooks/config.local.sh" ]]; then
    source "hooks/config.local.sh"
fi
