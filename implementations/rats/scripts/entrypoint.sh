#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# entrypoint.sh - Shell router for cross-shell support

set -euo pipefail

# Detect shell and route appropriately
SHELL_NAME=$(basename "${SHELL:-/bin/bash}")

case "$SHELL_NAME" in
    bash|sh)
        exec bash "$@"
        ;;
    zsh)
        exec zsh "$@"
        ;;
    fish)
        exec fish "$@"
        ;;
    nu|nushell)
        exec nu "$@"
        ;;
    oil|osh)
        exec osh "$@"
        ;;
    pwsh|powershell)
        exec pwsh "$@"
        ;;
    *)
        # Default to bash
        exec bash "$@"
        ;;
esac
