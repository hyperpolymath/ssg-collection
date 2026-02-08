#!/bin/bash
# poly-ssg Master Test Harness
# Runs all engine test suites with harsh validation
#
# Usage: ./scripts/test-harness.sh [engine-name]
#        ./scripts/test-harness.sh           # Run all engines
#        ./scripts/test-harness.sh zigzag    # Run specific engine

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
ENGINES_DIR="$PROJECT_ROOT/engines"
CORPUS_DIR="$PROJECT_ROOT/test-corpus"
RESULTS_DIR="$PROJECT_ROOT/test-results"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Track failed engines
declare -a FAILED_ENGINES=()

log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $*"; ((PASSED_TESTS++)); ((TOTAL_TESTS++)); }
log_fail() { echo -e "${RED}[FAIL]${NC} $*"; ((FAILED_TESTS++)); ((TOTAL_TESTS++)); }
log_skip() { echo -e "${YELLOW}[SKIP]${NC} $*"; ((SKIPPED_TESTS++)); ((TOTAL_TESTS++)); }
log_header() { echo -e "\n${BOLD}═══════════════════════════════════════════════════════════════${NC}"; echo -e "${BOLD} $*${NC}"; echo -e "${BOLD}═══════════════════════════════════════════════════════════════${NC}\n"; }

# Create results directory
mkdir -p "$RESULTS_DIR"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_FILE="$RESULTS_DIR/test-run-$TIMESTAMP.log"

# Start logging
exec > >(tee -a "$RESULTS_FILE") 2>&1

log_header "POLY-SSG MASTER TEST HARNESS"
echo "Started: $(date)"
echo "Corpus: $CORPUS_DIR"
echo "Engines: $ENGINES_DIR"

# Validate test corpus exists
if [[ ! -d "$CORPUS_DIR" ]]; then
    echo -e "${RED}ERROR: Test corpus not found at $CORPUS_DIR${NC}"
    exit 1
fi

# Count test files
VALID_COUNT=$(find "$CORPUS_DIR/valid" -name "*.md" 2>/dev/null | wc -l)
EDGE_COUNT=$(find "$CORPUS_DIR/edge-cases" -name "*.md" 2>/dev/null | wc -l)
MALFORMED_COUNT=$(find "$CORPUS_DIR/malformed" -name "*.md" 2>/dev/null | wc -l)
UNICODE_COUNT=$(find "$CORPUS_DIR/unicode" -name "*.md" 2>/dev/null | wc -l)
INJECTION_COUNT=$(find "$CORPUS_DIR/injection" -name "*.md" 2>/dev/null | wc -l)
STRESS_COUNT=$(find "$CORPUS_DIR/stress" -name "*.md" 2>/dev/null | wc -l)

echo ""
echo "Test Corpus:"
echo "  Valid files:     $VALID_COUNT"
echo "  Edge cases:      $EDGE_COUNT"
echo "  Malformed:       $MALFORMED_COUNT"
echo "  Unicode:         $UNICODE_COUNT"
echo "  Injection:       $INJECTION_COUNT"
echo "  Stress tests:    $STRESS_COUNT"

run_engine_tests() {
    local engine_name="$1"
    local engine_dir="$ENGINES_DIR/$engine_name"
    local test_script="$engine_dir/scripts/test-all.sh"

    log_header "Testing: $engine_name"

    if [[ ! -d "$engine_dir" ]]; then
        log_skip "$engine_name - directory not found"
        return 1
    fi

    if [[ ! -f "$test_script" ]]; then
        log_skip "$engine_name - no test script (scripts/test-all.sh)"
        return 1
    fi

    if [[ ! -x "$test_script" ]]; then
        chmod +x "$test_script"
    fi

    # Run the test script
    local start_time=$(date +%s)
    local exit_code=0

    cd "$engine_dir"

    # Export test corpus location
    export POLY_SSG_CORPUS="$CORPUS_DIR"
    export POLY_SSG_ROOT="$PROJECT_ROOT"

    if bash "$test_script"; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        log_pass "$engine_name completed in ${duration}s"
    else
        exit_code=$?
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        log_fail "$engine_name failed (exit code: $exit_code) after ${duration}s"
        FAILED_ENGINES+=("$engine_name")
    fi

    cd "$PROJECT_ROOT"
    return $exit_code
}

# Determine which engines to test
if [[ $# -gt 0 ]]; then
    ENGINES=("$@")
else
    # All engines
    ENGINES=(
        "forth-estate"
        "zigzag-ssg"
        "yocaml-ssg"
        "prodigy-ssg"
        "casket-ssg"
        "ddraig-ssg"
        "webforge-ssg"
        "rescribe-ssg"
        "wagasm-ssg"
        "parallel-press-ssg"
        "terrapin-ssg"
        "milk-ssg"
    )
fi

# Run tests for each engine
for engine in "${ENGINES[@]}"; do
    run_engine_tests "$engine" || true
done

# Summary
log_header "TEST SUMMARY"
echo "Total Tests:   $TOTAL_TESTS"
echo -e "Passed:        ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed:        ${RED}$FAILED_TESTS${NC}"
echo -e "Skipped:       ${YELLOW}$SKIPPED_TESTS${NC}"
echo ""

if [[ ${#FAILED_ENGINES[@]} -gt 0 ]]; then
    echo -e "${RED}Failed Engines:${NC}"
    for engine in "${FAILED_ENGINES[@]}"; do
        echo "  - $engine"
    done
    echo ""
fi

echo "Completed: $(date)"
echo "Results saved to: $RESULTS_FILE"

# Exit with failure if any tests failed
if [[ $FAILED_TESTS -gt 0 ]]; then
    exit 1
fi
