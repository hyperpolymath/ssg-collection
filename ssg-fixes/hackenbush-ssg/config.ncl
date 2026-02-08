# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# hackenbush-ssg - Nickel Configuration

# ═══════════════════════════════════════════════════════════════════════════════
# PROJECT CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

let Project = {
  name = "hackenbush-ssg",
  version = "0.1.0",
  language = "Conway's Game of Life",
  tier = "experimental",

  # RSR compliance settings
  rsr = {
    level = "gold",
    sha_pinned_actions = true,
    spdx_headers = true,
    well_known_dir = true,
  },
} in

# ═══════════════════════════════════════════════════════════════════════════════
# LIFE PATTERN CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

let LifeConfig = {
  # B3/S23 is standard Conway's Game of Life
  rule = "B3/S23",

  # Grid settings
  grid = {
    default_width = 1000,
    default_height = 1000,
    boundary = "finite",  # or "toroidal" for wrap-around
  },

  # Simulation settings
  simulation = {
    max_generations = 1000000,
    output_interval = 1000,
    checkpoint_interval = 10000,
  },

  # Output region configuration
  output = {
    start_x = 0,
    start_y = 0,
    width = 80,
    height = 25,
    encoding = "ascii",  # How to interpret cell states as bytes
  },
} in

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

let BuildConfig = {
  # Deno runtime settings
  deno = {
    permissions = ["--allow-read", "--allow-write"],
    check = true,
    fmt = true,
    lint = true,
  },

  # ReScript adapter settings
  rescript = {
    version = "11.1.0",
    output = "es6",
    suffix = ".mjs",
  },

  # Container settings
  container = {
    base_image = "cgr.dev/chainguard/wolfi-base:latest",
    registry = "ghcr.io/hyperpolymath",
    tag = "latest",
  },
} in

# ═══════════════════════════════════════════════════════════════════════════════
# SECURITY CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

let SecurityConfig = {
  # Dependency scanning
  scanning = {
    enabled = true,
    tools = ["dependabot", "codeql", "trufflehog"],
  },

  # Allowed file patterns
  allowed_patterns = {
    source = ["src/**/*.rle"],
    runtime = ["runtime/**/*.ts"],
    adapter = ["adapters/**/*.res"],
    tests = ["tests/**/*.ts"],
  },

  # Forbidden patterns (SSG logic in wrong places)
  forbidden = {
    runtime_ssg_logic = ["markdown", "frontmatter", "template", "render"],
    conventional_languages = ["*.py", "*.rb", "*.go", "*.java"],
  },
} in

# ═══════════════════════════════════════════════════════════════════════════════
# CI/CD CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

let CIConfig = {
  # Workflow triggers
  triggers = {
    push_branches = ["main", "develop"],
    pull_request_branches = ["main", "develop"],
  },

  # Required checks
  required_checks = [
    "pattern-validation",
    "type-check",
    "lint",
    "security-scan",
    "rsr-compliance",
  ],

  # Test matrix
  test_matrix = {
    os = ["ubuntu-latest"],
    deno_version = ["v1.x"],
  },
} in

# ═══════════════════════════════════════════════════════════════════════════════
# EXPORT CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

{
  project = Project,
  life = LifeConfig,
  build = BuildConfig,
  security = SecurityConfig,
  ci = CIConfig,
}
