#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# bootstrap.sh - Tool bootstrapping for rats-ssg

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# Check if command exists
has() { command -v "$1" >/dev/null 2>&1; }

# Install with asdf if available
asdf_install() {
    local tool=$1
    local version=$2
    if has asdf; then
        info "Installing $tool $version via asdf..."
        asdf plugin add "$tool" 2>/dev/null || true
        asdf install "$tool" "$version"
        asdf local "$tool" "$version"
    else
        warn "asdf not available, skipping $tool installation"
    fi
}

info "Bootstrapping rats-ssg development environment..."

# Check Deno
if has deno; then
    DENO_VERSION=$(deno --version | head -1 | cut -d' ' -f2)
    info "Deno $DENO_VERSION found"
else
    asdf_install deno 2.1.4
fi

# Check Just
if has just; then
    JUST_VERSION=$(just --version | cut -d' ' -f2)
    info "Just $JUST_VERSION found"
else
    asdf_install just 1.38.0
fi

# Check Nickel
if has nickel; then
    NICKEL_VERSION=$(nickel --version | cut -d' ' -f2)
    info "Nickel $NICKEL_VERSION found"
else
    asdf_install nickel 1.9.0
fi

# Check ReScript (via npm/deno)
if [ -f "node_modules/.bin/rescript" ] || has rescript; then
    info "ReScript found"
else
    info "Installing ReScript..."
    deno install -g npm:rescript@11 2>/dev/null || warn "ReScript install skipped"
fi

info "Bootstrap complete!"
info "Run 'just setup' to initialize the project"
