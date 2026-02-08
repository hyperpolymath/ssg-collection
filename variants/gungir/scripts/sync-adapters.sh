#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# sync-adapters.sh — Sync adapters from poly-ssg-mcp hub

set -euo pipefail

HUB_REPO="hyperpolymath/poly-ssg-mcp"
ADAPTERS_DIR="adapters"

echo "═══════════════════════════════════════════════════════════════"
echo "gungir-ssg Adapter Sync"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Source: https://github.com/${HUB_REPO}"
echo "Target: ${ADAPTERS_DIR}/"
echo ""

# Check for dry-run flag
DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
    echo "Mode: DRY RUN (no changes will be made)"
else
    echo "Mode: LIVE"
fi
echo ""

# Verify we're in the right directory
if [[ ! -d "${ADAPTERS_DIR}" ]]; then
    echo "Error: ${ADAPTERS_DIR}/ directory not found"
    echo "Run this script from the gungir-ssg root directory"
    exit 1
fi

# Count current adapters
CURRENT_COUNT=$(ls -1 "${ADAPTERS_DIR}"/*.js 2>/dev/null | wc -l)
echo "Current adapters: ${CURRENT_COUNT}"

if [[ "${DRY_RUN}" == true ]]; then
    echo ""
    echo "Dry run complete. Run without --dry-run to sync."
    exit 0
fi

echo ""
echo "To sync manually:"
echo "  1. Clone or pull poly-ssg-mcp"
echo "  2. Copy adapters/*.js to this project"
echo "  3. Run: just verify-all-adapters"
echo ""
echo "Automated sync requires additional setup."
echo "See: https://github.com/${HUB_REPO}/README.md"
