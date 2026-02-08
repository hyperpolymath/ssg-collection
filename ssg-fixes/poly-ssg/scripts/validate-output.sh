#!/bin/bash
# Output Validation Script
# Validates generated HTML against security and correctness requirements
#
# Usage: validate-output.sh <html-file>
#        validate-output.sh <directory>

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

ERRORS=0
WARNINGS=0

log_error() { echo -e "${RED}[ERROR]${NC} $*"; ((ERRORS++)); }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; ((WARNINGS++)); }
log_ok() { echo -e "${GREEN}[OK]${NC} $*"; }

validate_file() {
    local file="$1"
    local filename=$(basename "$file")

    echo "Validating: $filename"

    # Check file exists and is not empty
    if [[ ! -f "$file" ]]; then
        log_error "File not found: $file"
        return 1
    fi

    if [[ ! -s "$file" ]]; then
        log_warn "File is empty: $file"
    fi

    local content
    content=$(cat "$file")

    # =========================================================================
    # SECURITY CHECKS - These are ERRORS (treat as failures)
    # =========================================================================

    # Check for unescaped script tags
    if echo "$content" | grep -qi '<script[^>]*>'; then
        log_error "$filename: Contains <script> tag - potential XSS"
    fi

    # Check for event handlers
    if echo "$content" | grep -Eqi 'on(click|load|error|mouseover|focus|blur|submit|change|keyup|keydown)='; then
        log_error "$filename: Contains event handler attribute - potential XSS"
    fi

    # Check for javascript: URLs
    if echo "$content" | grep -qi 'javascript:'; then
        log_error "$filename: Contains javascript: URL - potential XSS"
    fi

    # Check for data: URLs in dangerous contexts
    if echo "$content" | grep -Eqi '(src|href)=["\x27]data:'; then
        log_warn "$filename: Contains data: URL - review for safety"
    fi

    # Check for path traversal in paths
    if echo "$content" | grep -Eqi '(src|href)=["\x27][^"]*\.\./'; then
        log_error "$filename: Contains path traversal sequence"
    fi

    # Check for shell metacharacters that might indicate injection
    if echo "$content" | grep -Eqi '\$\(|`[^`]*`|\$\{'; then
        log_warn "$filename: Contains shell-like syntax - verify escaping"
    fi

    # =========================================================================
    # HTML VALIDITY CHECKS
    # =========================================================================

    # Check for balanced tags (basic check)
    local open_tags close_tags
    open_tags=$(echo "$content" | grep -oE '<(p|div|span|ul|ol|li|h[1-6]|blockquote|pre|code|table|tr|td|th|thead|tbody)[^>]*>' | wc -l)
    close_tags=$(echo "$content" | grep -oE '</(p|div|span|ul|ol|li|h[1-6]|blockquote|pre|code|table|tr|td|th|thead|tbody)>' | wc -l)

    if [[ $open_tags -ne $close_tags ]]; then
        log_warn "$filename: Tag balance mismatch (open: $open_tags, close: $close_tags)"
    fi

    # Check for DOCTYPE and html structure
    if echo "$content" | grep -qi '<!DOCTYPE'; then
        if ! echo "$content" | grep -qi '<html'; then
            log_warn "$filename: Has DOCTYPE but no <html> tag"
        fi
        if ! echo "$content" | grep -qi '<head'; then
            log_warn "$filename: Has DOCTYPE but no <head> tag"
        fi
        if ! echo "$content" | grep -qi '<body'; then
            log_warn "$filename: Has DOCTYPE but no <body> tag"
        fi
    fi

    # =========================================================================
    # ENCODING CHECKS
    # =========================================================================

    # Check for proper UTF-8 (no invalid sequences)
    if ! iconv -f UTF-8 -t UTF-8 "$file" > /dev/null 2>&1; then
        log_error "$filename: Invalid UTF-8 encoding"
    fi

    # Check for null bytes
    if grep -q $'\x00' "$file" 2>/dev/null; then
        log_error "$filename: Contains null bytes"
    fi

    # =========================================================================
    # ACCESSIBILITY CHECKS
    # =========================================================================

    # Check images have alt text
    if echo "$content" | grep -qiE '<img[^>]+>' && ! echo "$content" | grep -qiE '<img[^>]+alt='; then
        log_warn "$filename: Image without alt attribute"
    fi

    # Check links have content
    if echo "$content" | grep -qiE '<a[^>]*></a>'; then
        log_warn "$filename: Empty link found"
    fi

    return 0
}

# Main
if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <file-or-directory>"
    exit 1
fi

TARGET="$1"

if [[ -d "$TARGET" ]]; then
    find "$TARGET" -name "*.html" -type f | while read -r file; do
        validate_file "$file"
    done
elif [[ -f "$TARGET" ]]; then
    validate_file "$TARGET"
else
    echo "Error: $TARGET not found"
    exit 1
fi

echo ""
echo "Validation complete: $ERRORS errors, $WARNINGS warnings"

# Exit with error if any errors found
if [[ $ERRORS -gt 0 ]]; then
    exit 1
fi

# Exit with warning status if only warnings
if [[ $WARNINGS -gt 0 ]]; then
    exit 2
fi

exit 0
