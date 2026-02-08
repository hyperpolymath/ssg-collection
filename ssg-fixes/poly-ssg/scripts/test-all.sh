#!/bin/bash
# poly-ssg test runner CLI
#
# A billion possibilities in one script
#
# Usage:
#   ./test-all.sh [options] [command] [args...]
#
# Commands:
#   test [engine] [suite]     Run tests (default)
#   build <engine> [opts]     Build with engine
#   bench <engine>            Benchmark engine
#   validate <engine>         Validate engine manifest
#   list                      List engines
#   info <engine>             Show engine info
#   matrix                    Run full test matrix
#
# Options:
#   -v, --verbose             Verbose output
#   -q, --quiet               Quiet mode
#   -p, --parallel            Run in parallel
#   -j, --jobs N              Number of parallel jobs
#   -o, --output DIR          Output directory
#   -s, --source DIR          Source directory
#   -f, --format FMT          Output format (html|json|xml)
#   -e, --env ENV             Environment (dev|prod|test)
#   --drafts                  Include drafts
#   --no-color                Disable colors
#   --dry-run                 Show what would be done
#   -h, --help                Show this help

set -e

# ============================================================================
# Configuration
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
ENGINES_DIR="$ROOT_DIR/engines"
CORPUS_DIR="$ROOT_DIR/test-corpus"

# Defaults
VERBOSE=false
QUIET=false
PARALLEL=false
JOBS=4
OUTPUT_DIR="output"
SOURCE_DIR="test-corpus/posts"
FORMAT="html"
ENV="development"
INCLUDE_DRAFTS=false
NO_COLOR=false
DRY_RUN=false

# ============================================================================
# Colors (billion shades)
# ============================================================================

setup_colors() {
    if [[ "$NO_COLOR" == "true" ]] || [[ ! -t 1 ]]; then
        RED="" GREEN="" YELLOW="" BLUE="" MAGENTA="" CYAN="" BOLD="" DIM="" RESET=""
    else
        RED='\033[0;31m'
        GREEN='\033[0;32m'
        YELLOW='\033[1;33m'
        BLUE='\033[0;34m'
        MAGENTA='\033[0;35m'
        CYAN='\033[0;36m'
        BOLD='\033[1m'
        DIM='\033[2m'
        RESET='\033[0m'
    fi
}

# ============================================================================
# Logging
# ============================================================================

log() { [[ "$QUIET" != "true" ]] && echo -e "${GREEN}▸${RESET} $*"; }
log_info() { [[ "$QUIET" != "true" ]] && echo -e "${BLUE}ℹ${RESET} $*"; }
log_warn() { echo -e "${YELLOW}⚠${RESET} $*" >&2; }
log_error() { echo -e "${RED}✗${RESET} $*" >&2; }
log_success() { [[ "$QUIET" != "true" ]] && echo -e "${GREEN}✓${RESET} $*"; }
log_debug() { [[ "$VERBOSE" == "true" ]] && echo -e "${DIM}  $*${RESET}"; }

die() { log_error "$*"; exit 1; }

# ============================================================================
# Help
# ============================================================================

show_help() {
    cat << 'EOF'
poly-ssg test runner CLI

USAGE:
    ./test-all.sh [OPTIONS] [COMMAND] [ARGS...]

COMMANDS:
    test [engine] [suite]     Run tests (default command)
                              suite: all|markdown|frontmatter|full
    build <engine>            Build site with engine
    bench <engine>            Benchmark engine performance
    validate <engine>         Validate engine manifest
    list                      List available engines
    info <engine>             Show detailed engine info
    matrix                    Run full build/test matrix
    compare <e1> <e2>         Compare output of two engines
    watch <engine>            Watch and rebuild on changes

OPTIONS:
    -v, --verbose             Verbose output
    -q, --quiet               Suppress non-error output
    -p, --parallel            Run operations in parallel
    -j, --jobs N              Number of parallel jobs (default: 4)
    -o, --output DIR          Output directory (default: output)
    -s, --source DIR          Source directory (default: test-corpus/posts)
    -f, --format FMT          Output format: html, json, xml (default: html)
    -e, --env ENV             Environment: dev, prod, test (default: dev)
    --drafts                  Include draft posts
    --no-color                Disable colored output
    --dry-run                 Show what would be done without doing it
    -h, --help                Show this help message

EXAMPLES:
    # Run all tests for all engines
    ./test-all.sh test

    # Test specific engine with specific suite
    ./test-all.sh test forth-estate markdown

    # Build with options
    ./test-all.sh -o public -s content --drafts build forth-estate

    # Parallel matrix build
    ./test-all.sh -p -j 8 matrix

    # Compare engine outputs
    ./test-all.sh compare forth-estate webforge

    # Benchmark
    ./test-all.sh bench forth-estate

ENGINES:
EOF
    list_engines
}

# ============================================================================
# Engine Discovery
# ============================================================================

list_engines() {
    for dir in "$ENGINES_DIR"/*/; do
        [[ -d "$dir" ]] || continue
        local name=$(basename "$dir")
        if [[ -f "$dir/manifest.json" ]] || [[ -f "$dir/manifest.ncl" ]]; then
            local lang=""
            if [[ -f "$dir/manifest.json" ]]; then
                lang=$(jq -r '.language // "unknown"' "$dir/manifest.json" 2>/dev/null)
            elif [[ -f "$dir/manifest.ncl" ]]; then
                lang=$(nickel query "$dir/manifest.ncl" --field language 2>/dev/null || echo "unknown")
            fi
            echo -e "    ${CYAN}$name${RESET} ${DIM}($lang)${RESET}"
        fi
    done
}

get_engines() {
    for dir in "$ENGINES_DIR"/*/; do
        [[ -d "$dir" ]] || continue
        local name=$(basename "$dir")
        if [[ -f "$dir/manifest.json" ]] || [[ -f "$dir/manifest.ncl" ]]; then
            echo "$name"
        fi
    done
}

engine_exists() {
    [[ -d "$ENGINES_DIR/$1" ]] && \
    { [[ -f "$ENGINES_DIR/$1/manifest.json" ]] || [[ -f "$ENGINES_DIR/$1/manifest.ncl" ]]; }
}

# ============================================================================
# Engine Operations
# ============================================================================

test_forth_estate() {
    local suite="${1:-all}"
    local engine_dir="$ENGINES_DIR/forth-estate"
    local gforth_cmd="gforth"

    # Try direct gforth first, then toolbox
    if ! command -v gforth &>/dev/null; then
        if command -v toolbox &>/dev/null; then
            gforth_cmd="toolbox run gforth"
            log_debug "Using toolbox for gforth"
        else
            log_warn "Gforth not installed. Skipping."
            log_info "Install: sudo dnf install gforth"
            return 1
        fi
    fi

    cd "$engine_dir"

    case "$suite" in
        all)
            log "Running all Forth Estate tests..."
            $gforth_cmd forth-estate.fs -e "test-markdown cr test-frontmatter cr test-full bye"
            ;;
        markdown)
            log "Running markdown test..."
            $gforth_cmd forth-estate.fs -e "test-markdown bye"
            ;;
        frontmatter)
            log "Running frontmatter test..."
            $gforth_cmd forth-estate.fs -e "test-frontmatter bye"
            ;;
        full)
            log "Running full pipeline test..."
            $gforth_cmd forth-estate.fs -e "test-full bye"
            ;;
        *)
            die "Unknown test suite: $suite (use: all|markdown|frontmatter|full)"
            ;;
    esac
}

build_forth_estate() {
    local src="$1"
    local dest="$2"
    local engine_dir="$ENGINES_DIR/forth-estate"
    local gforth_cmd="gforth"

    # Try direct gforth first, then toolbox
    if ! command -v gforth &>/dev/null; then
        if command -v toolbox &>/dev/null; then
            gforth_cmd="toolbox run gforth"
        else
            die "Gforth not installed"
        fi
    fi

    [[ "$DRY_RUN" == "true" ]] && {
        log_info "[dry-run] Would build: $src → $dest"
        return 0
    }

    mkdir -p "$dest"
    cd "$engine_dir"

    log "Building: $src → $dest"
    $gforth_cmd forth-estate.fs -e "s\" $src\" s\" $dest\" build bye"
}

bench_forth_estate() {
    local iterations="${1:-10}"
    local gforth_cmd="gforth"

    # Try direct gforth first, then toolbox
    if ! command -v gforth &>/dev/null; then
        if command -v toolbox &>/dev/null; then
            gforth_cmd="toolbox run gforth"
        else
            die "Gforth not installed"
        fi
    fi

    if ! command -v hyperfine &>/dev/null; then
        log_warn "hyperfine not installed, using basic timing"
        time $gforth_cmd "$ENGINES_DIR/forth-estate/forth-estate.fs" -e "test-full bye"
        return
    fi

    hyperfine --warmup 2 --runs "$iterations" \
        "$gforth_cmd $ENGINES_DIR/forth-estate/forth-estate.fs -e 'test-full bye'"
}

# ============================================================================
# Commands
# ============================================================================

cmd_test() {
    local engine="${1:-all}"
    local suite="${2:-all}"

    if [[ "$engine" == "all" ]]; then
        log "Testing all engines..."
        local passed=0 failed=0 skipped=0

        for e in $(get_engines); do
            echo -e "\n${BOLD}=== $e ===${RESET}"
            if "test_${e//-/_}" "$suite" 2>/dev/null; then
                ((passed++))
            else
                ((failed++))
            fi
        done

        echo -e "\n${BOLD}Results:${RESET} ${GREEN}$passed passed${RESET}, ${RED}$failed failed${RESET}"
    else
        engine_exists "$engine" || die "Unknown engine: $engine"
        "test_${engine//-/_}" "$suite"
    fi
}

cmd_build() {
    local engine="$1"
    [[ -z "$engine" ]] && die "Usage: build <engine>"
    engine_exists "$engine" || die "Unknown engine: $engine"

    "build_${engine//-/_}" "$SOURCE_DIR" "$OUTPUT_DIR"
    log_success "Build complete: $OUTPUT_DIR"
}

cmd_bench() {
    local engine="$1"
    [[ -z "$engine" ]] && die "Usage: bench <engine>"
    engine_exists "$engine" || die "Unknown engine: $engine"

    log "Benchmarking $engine..."
    "bench_${engine//-/_}"
}

cmd_validate() {
    local engine="$1"
    [[ -z "$engine" ]] && die "Usage: validate <engine>"
    engine_exists "$engine" || die "Unknown engine: $engine"

    local manifest_json="$ENGINES_DIR/$engine/manifest.json"
    local manifest_ncl="$ENGINES_DIR/$engine/manifest.ncl"

    if [[ -f "$manifest_ncl" ]] && command -v nickel &>/dev/null; then
        log "Validating Nickel manifest..."
        nickel typecheck "$manifest_ncl" && log_success "Manifest valid"
    elif [[ -f "$manifest_json" ]]; then
        log "Validating JSON manifest..."
        jq . "$manifest_json" >/dev/null && log_success "Manifest valid"
    else
        die "No manifest found"
    fi
}

cmd_list() {
    echo -e "${BOLD}Available engines:${RESET}"
    list_engines
}

cmd_info() {
    local engine="$1"
    [[ -z "$engine" ]] && die "Usage: info <engine>"
    engine_exists "$engine" || die "Unknown engine: $engine"

    echo -e "${BOLD}=== $engine ===${RESET}\n"

    if [[ -f "$ENGINES_DIR/$engine/manifest.ncl" ]] && command -v nickel &>/dev/null; then
        nickel export "$ENGINES_DIR/$engine/manifest.ncl" 2>/dev/null | jq .
    elif [[ -f "$ENGINES_DIR/$engine/manifest.json" ]]; then
        jq . "$ENGINES_DIR/$engine/manifest.json"
    fi
}

cmd_matrix() {
    log "Running full build matrix..."

    local engines=($(get_engines))
    local formats=("html" "json" "xml")
    local envs=("dev" "prod")

    for engine in "${engines[@]}"; do
        for format in "${formats[@]}"; do
            for env in "${envs[@]}"; do
                log_info "Matrix: $engine × $format × $env"
                [[ "$DRY_RUN" != "true" ]] && {
                    OUTPUT_DIR="output/matrix/$engine/$format/$env"
                    FORMAT="$format"
                    ENV="$env"
                    cmd_build "$engine" 2>/dev/null || log_warn "Failed: $engine"
                }
            done
        done
    done

    log_success "Matrix complete"
}

cmd_compare() {
    local e1="$1" e2="$2"
    [[ -z "$e1" || -z "$e2" ]] && die "Usage: compare <engine1> <engine2>"

    log "Comparing $e1 vs $e2..."

    local tmp1=$(mktemp -d)
    local tmp2=$(mktemp -d)
    trap "rm -rf $tmp1 $tmp2" EXIT

    "build_${e1//-/_}" "$SOURCE_DIR" "$tmp1" 2>/dev/null
    "build_${e2//-/_}" "$SOURCE_DIR" "$tmp2" 2>/dev/null

    diff -r "$tmp1" "$tmp2" || log_warn "Outputs differ"
}

cmd_watch() {
    local engine="$1"
    [[ -z "$engine" ]] && die "Usage: watch <engine>"

    if ! command -v inotifywait &>/dev/null; then
        die "inotifywait not found. Install inotify-tools."
    fi

    log "Watching $SOURCE_DIR for changes..."

    while inotifywait -r -e modify,create,delete "$SOURCE_DIR" 2>/dev/null; do
        log "Change detected, rebuilding..."
        cmd_build "$engine"
    done
}

# ============================================================================
# Argument Parsing
# ============================================================================

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -v|--verbose)   VERBOSE=true; shift ;;
            -q|--quiet)     QUIET=true; shift ;;
            -p|--parallel)  PARALLEL=true; shift ;;
            -j|--jobs)      JOBS="$2"; shift 2 ;;
            -o|--output)    OUTPUT_DIR="$2"; shift 2 ;;
            -s|--source)    SOURCE_DIR="$2"; shift 2 ;;
            -f|--format)    FORMAT="$2"; shift 2 ;;
            -e|--env)       ENV="$2"; shift 2 ;;
            --drafts)       INCLUDE_DRAFTS=true; shift ;;
            --no-color)     NO_COLOR=true; shift ;;
            --dry-run)      DRY_RUN=true; shift ;;
            -h|--help)      show_help; exit 0 ;;
            -*)             die "Unknown option: $1" ;;
            *)              break ;;
        esac
    done

    COMMAND="${1:-test}"
    shift || true
    ARGS=("$@")
}

# ============================================================================
# Main
# ============================================================================

main() {
    parse_args "$@"
    setup_colors

    log_debug "Command: $COMMAND"
    log_debug "Args: ${ARGS[*]}"
    log_debug "Options: verbose=$VERBOSE parallel=$PARALLEL jobs=$JOBS"

    case "$COMMAND" in
        test)       cmd_test "${ARGS[@]}" ;;
        build)      cmd_build "${ARGS[@]}" ;;
        bench)      cmd_bench "${ARGS[@]}" ;;
        validate)   cmd_validate "${ARGS[@]}" ;;
        list)       cmd_list ;;
        info)       cmd_info "${ARGS[@]}" ;;
        matrix)     cmd_matrix ;;
        compare)    cmd_compare "${ARGS[@]}" ;;
        watch)      cmd_watch "${ARGS[@]}" ;;
        *)          die "Unknown command: $COMMAND (try --help)" ;;
    esac
}

main "$@"
