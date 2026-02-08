# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# mustfile.ncl - Contract-driven deployment specification for qed-ssg
# Following the hyperpolymath/mustfile pattern

# ============================================================================
# MUST-SPEC: Contract of Physical State
# ============================================================================
# Every operation is a state transition where resources are consumed
# to produce outputs. This file defines the must-spec contracts.

let Language = std.enum.TagOrString in

# ============================================================================
# POLICY ENFORCEMENT CONTRACTS
# ============================================================================

let LanguagePolicy = {
  # ALLOWED languages with their use cases
  allowed = {
    rescript = {
      extensions = [".res", ".resi"],
      use_case = "Primary application code",
      notes = "Compiles to JS, type-safe",
    },
    deno = {
      runtime = true,
      use_case = "Runtime & package management",
      notes = "Replaces Node/npm/bun",
    },
    rust = {
      extensions = [".rs"],
      use_case = "Performance-critical, systems, WASM",
      notes = "Preferred for CLI tools",
    },
    gleam = {
      extensions = [".gleam"],
      use_case = "Backend services",
      notes = "Runs on BEAM or compiles to JS",
    },
    bash = {
      extensions = [".sh"],
      use_case = "Scripts, automation",
      notes = "Keep minimal",
    },
    javascript = {
      extensions = [".js"],
      use_case = "Only where ReScript cannot",
      notes = "MCP protocol glue, Deno APIs only",
      constraint = "Must be Deno-native, no Node/npm",
    },
    nickel = {
      extensions = [".ncl"],
      use_case = "Configuration language",
      notes = "For complex configs",
    },
    guile_scheme = {
      extensions = [".scm"],
      use_case = "State/meta files",
      notes = "STATE.scm, META.scm, ECOSYSTEM.scm",
    },
    julia = {
      extensions = [".jl"],
      use_case = "Batch scripts, data processing",
      notes = "Per RSR",
    },
  },

  # BANNED - enforcement required
  banned = {
    typescript = {
      extensions = [".ts", ".tsx", ".mts", ".cts"],
      replacement = "rescript",
      error = "TypeScript is banned. Use ReScript instead.",
    },
    nodejs = {
      files = ["package.json", "package-lock.json", ".npmrc"],
      replacement = "deno",
      error = "Node.js is banned. Use Deno instead.",
    },
    npm = {
      files = ["package.json", "package-lock.json", ".npmrc"],
      replacement = "deno",
      error = "npm is banned. Use Deno instead.",
    },
    bun = {
      files = ["bun.lockb", "bunfig.toml"],
      replacement = "deno",
      error = "Bun is banned. Use Deno instead.",
    },
    pnpm = {
      files = ["pnpm-lock.yaml", ".pnpmfile.cjs"],
      replacement = "deno",
      error = "pnpm is banned. Use Deno instead.",
    },
    yarn = {
      files = ["yarn.lock", ".yarnrc", ".yarnrc.yml"],
      replacement = "deno",
      error = "yarn is banned. Use Deno instead.",
    },
    go = {
      extensions = [".go"],
      files = ["go.mod", "go.sum"],
      replacement = "rust",
      error = "Go is banned. Use Rust instead.",
    },
    makefile = {
      files = ["Makefile", "makefile", "GNUmakefile"],
      patterns = ["*.mk"],
      replacement = "justfile",
      error = "Makefile is banned. Use Justfile instead.",
    },
    python_general = {
      extensions = [".py"],
      exception = "SaltStack only",
      replacement = "rescript/rust",
      error = "Python is banned except for SaltStack. Use ReScript or Rust.",
    },
    java = {
      extensions = [".java"],
      replacement = "rust/tauri/dioxus",
      error = "Java is banned. Use Rust, Tauri, or Dioxus instead.",
    },
    kotlin = {
      extensions = [".kt", ".kts"],
      replacement = "rust/tauri/dioxus",
      error = "Kotlin is banned. Use Rust, Tauri, or Dioxus instead.",
    },
    swift = {
      extensions = [".swift"],
      replacement = "rust/tauri/dioxus",
      error = "Swift is banned. Use Rust, Tauri, or Dioxus instead.",
    },
  },
} in

# ============================================================================
# BUILD CONTRACTS
# ============================================================================

let BuildContract = {
  # ReScript compilation must succeed
  rescript = {
    command = "rescript build",
    outputs = [".res.js"],
    must_pass = true,
  },

  # Deno type checking
  deno_check = {
    command = "deno check src/**/*.js adapters/**/*.js",
    must_pass = true,
  },

  # Linting
  lint = {
    command = "deno lint src/ adapters/",
    must_pass = true,
  },

  # Formatting
  fmt = {
    command = "deno fmt --check src/ adapters/ tests/",
    must_pass = true,
  },
} in

# ============================================================================
# TEST CONTRACTS
# ============================================================================

let TestContract = {
  unit = {
    command = "deno test --allow-read --allow-run tests/unit/",
    coverage_threshold = 70,
    must_pass = true,
  },

  integration = {
    command = "deno test --allow-read --allow-run tests/integration/",
    must_pass = true,
  },

  e2e = {
    command = "deno test --allow-all tests/e2e/",
    must_pass = true,
  },

  benchmark = {
    command = "deno test --allow-read --allow-run tests/benchmark/",
    must_pass = false, # Benchmarks are informational
  },
} in

# ============================================================================
# SECURITY CONTRACTS
# ============================================================================

let SecurityContract = {
  # No eval() allowed
  no_eval = {
    pattern = "eval\\(",
    files = ["*.js", "*.res.js"],
    must_not_match = true,
    error = "eval() is forbidden for security reasons",
  },

  # No deprecated Deno.run
  no_deno_run = {
    pattern = "Deno\\.run",
    files = ["*.js", "*.res.js"],
    must_not_match = true,
    error = "Deno.run is deprecated, use Deno.Command instead",
  },

  # SPDX headers required
  spdx_required = {
    pattern = "SPDX-License-Identifier",
    files = ["*.js", "*.res", "*.ncl", "*.sh"],
    must_match = true,
    location = "first 3 lines",
    error = "SPDX license header is required",
  },

  # No hardcoded secrets
  no_secrets = {
    patterns = [
      "password\\s*=\\s*['\"]",
      "api_key\\s*=\\s*['\"]",
      "secret\\s*=\\s*['\"]",
      "token\\s*=\\s*['\"]",
    ],
    files = ["*.js", "*.res", "*.ncl"],
    must_not_match = true,
    error = "Hardcoded secrets detected",
  },

  # HTTPS only
  https_only = {
    pattern = "http://(?!localhost|127\\.0\\.0\\.1)",
    files = ["*.js", "*.res", "*.ncl"],
    must_not_match = true,
    error = "HTTP URLs are forbidden, use HTTPS",
  },
} in

# ============================================================================
# DEPLOYMENT CONTRACTS
# ============================================================================

let DeployContract = {
  # Container build must succeed
  container = {
    command = "podman build -t qed-ssg:latest -f Containerfile --target production .",
    must_pass = true,
  },

  # Health check must pass
  health = {
    endpoint = "/health",
    expected_status = 200,
    timeout_ms = 5000,
  },
} in

# ============================================================================
# EXPORT MUST-SPEC
# ============================================================================

{
  name = "qed-ssg",
  version = "0.1.0",
  description = "Must-spec contracts for qed-ssg",

  # Core contracts
  policy = LanguagePolicy,
  build = BuildContract,
  test = TestContract,
  security = SecurityContract,
  deploy = DeployContract,

  # Toolchain requirements
  toolchain = {
    just = { required = true, min_version = "1.0.0" },
    deno = { required = true, min_version = "1.40.0" },
    rescript = { required = true, min_version = "11.0.0" },
    nickel = { required = true, min_version = "1.5.0" },
    podman = { required = false, notes = "For container builds" },
  },

  # Enforcement hooks
  hooks = {
    pre_commit = {
      run = ["just fmt-check", "just lint", "just enforce-policy"],
      block_on_failure = true,
    },
    pre_push = {
      run = ["just test", "just audit"],
      block_on_failure = true,
    },
  },
}
