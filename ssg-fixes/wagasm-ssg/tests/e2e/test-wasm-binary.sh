#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# E2E Test: WASM binary verification

set -e

echo "=== E2E Test: WASM Binary ==="

# Compile
just compile

# Check WASM file exists
if [ ! -f src/wagasm-ssg.wasm ]; then
    echo "FAIL: WASM file not generated"
    exit 1
fi

# Check WASM magic number (\0asm)
if ! xxd -l 4 src/wagasm-ssg.wasm | grep -q "0061 736d"; then
    echo "FAIL: Invalid WASM magic number"
    exit 1
fi

# Validate WASM
if ! wasm-validate src/wagasm-ssg.wasm 2>/dev/null; then
    echo "FAIL: WASM validation failed"
    exit 1
fi

# Check for expected exports
exports=$(wasm-objdump -x src/wagasm-ssg.wasm | grep "Export" || true)
if [ -z "$exports" ]; then
    echo "FAIL: No exports found in WASM module"
    exit 1
fi

echo "PASS: WASM binary is valid"
echo "  - Magic number verified"
echo "  - Validation passed"
echo "  - Exports present"
