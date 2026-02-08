#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# E2E Test: Language compliance

set -e

echo "=== E2E Test: Language Compliance ==="

# Check for forbidden files in src/
forbidden=$(find src/ -type f \( -name "*.ts" -o -name "*.js" -o -name "*.rs" -o -name "*.py" -o -name "*.as" \) 2>/dev/null || true)
if [ -n "$forbidden" ]; then
    echo "FAIL: Found FORBIDDEN language files in src/:"
    echo "$forbidden"
    exit 1
fi

# Verify only .wat files in src/
wat_count=$(find src/ -name "*.wat" | wc -l)
if [ "$wat_count" -eq 0 ]; then
    echo "FAIL: No WAT files found in src/"
    exit 1
fi

# Check for AssemblyScript in package.json files
if grep -r "assemblyscript\|@assemblyscript" . --include="*.json" 2>/dev/null; then
    echo "FAIL: AssemblyScript detected - this is FORBIDDEN"
    exit 1
fi

echo "PASS: Language compliance verified"
echo "  - Only WAT files in src/"
echo "  - No AssemblyScript dependencies"
