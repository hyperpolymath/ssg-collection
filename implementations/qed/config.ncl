# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# config.ncl — qed-ssg Nickel configuration schema

# ============================================================================
# TYPE DEFINITIONS
# ============================================================================

let AdapterName = std.contract.from_predicate (fun x =>
  std.is_string x && std.string.length x > 0
) in

let Language = std.enum.TagOrString in

let Port = std.contract.from_predicate (fun x =>
  std.is_number x && x >= 1024 && x <= 65535
) in

let FilePath = std.contract.from_predicate (fun x =>
  std.is_string x && std.string.length x > 0
) in

# ============================================================================
# ADAPTER SCHEMA
# ============================================================================

let Adapter = {
  name | AdapterName,
  language | Language,
  description | String,
  binary_path | FilePath | optional,
  default_port | Port | default = 8080,
  tools | Array {
    name | String,
    description | String,
    required_args | Array String | default = [],
  },
} in

# ============================================================================
# PROJECT CONFIGURATION
# ============================================================================

let ProjectConfig = {
  name | String | default = "qed-ssg",
  version | String | default = "0.1.0",

  # Adapter settings
  adapters | {
    enabled | Array AdapterName | default = [],
    timeout_ms | Number | default = 30000,
    retry_count | Number | default = 3,
  },

  # Build settings
  build | {
    output_dir | FilePath | default = "dist",
    source_dir | FilePath | default = "content",
    clean_before_build | Bool | default = true,
  },

  # Development settings
  dev | {
    port | Port | default = 3000,
    watch | Bool | default = true,
    open_browser | Bool | default = false,
  },

  # Testing settings
  test | {
    coverage_threshold | Number | default = 70,
    timeout_ms | Number | default = 60000,
    parallel | Bool | default = true,
  },

  # Security settings
  security | {
    audit_on_install | Bool | default = true,
    reject_eval | Bool | default = true,
    require_spdx | Bool | default = true,
    allowed_commands | Array String | default = ["deno", "just", "git"],
  },
} in

# ============================================================================
# ADAPTER REGISTRY
# ============================================================================

let AdapterRegistry = {
  rust = {
    adapters = ["cobalt", "mdbook", "zola"],
    runtime = "native",
  },
  haskell = {
    adapters = ["hakyll", "ema"],
    runtime = "stack",
  },
  elixir = {
    adapters = ["serum", "tableau", "nimble-publisher"],
    runtime = "mix",
  },
  clojure = {
    adapters = ["babashka", "cryogen", "perun"],
    runtime = "clojure",
  },
  julia = {
    adapters = ["franklin", "documenter", "staticwebpages"],
    runtime = "julia",
  },
  scala = {
    adapters = ["laika", "scalatex"],
    runtime = "sbt",
  },
  racket = {
    adapters = ["frog", "pollen"],
    runtime = "racket",
  },
  other = {
    adapters = [
      { name = "fornax", lang = "f#" },
      { name = "orchid", lang = "kotlin" },
      { name = "marmot", lang = "crystal" },
      { name = "coleslaw", lang = "common-lisp" },
      { name = "nimrod", lang = "nim" },
      { name = "reggae", lang = "d" },
      { name = "publish", lang = "swift" },
      { name = "wub", lang = "tcl" },
      { name = "yocaml", lang = "ocaml" },
      { name = "zotonic", lang = "erlang" },
    ],
  },
} in

# ============================================================================
# CI/CD CONFIGURATION
# ============================================================================

let CIConfig = {
  workflows | {
    ci | {
      triggers = ["push", "pull_request"],
      jobs = ["lint", "test", "audit", "build"],
    },
    codeql | {
      triggers = ["push", "pull_request", "schedule"],
      languages = ["javascript-typescript"],
    },
    release | {
      triggers = ["tag"],
      jobs = ["test-all", "build", "publish"],
    },
  },

  dependencies | {
    ecosystems = ["github-actions", "npm"],
    update_frequency = "weekly",
    auto_merge = false,
  },
} in

# ============================================================================
# HOOKS CONFIGURATION
# ============================================================================

let HooksConfig = {
  pre_commit | {
    enabled | Bool | default = true,
    commands = ["just fmt-check", "just lint", "just test"],
  },

  pre_push | {
    enabled | Bool | default = true,
    commands = ["just test-all", "just audit"],
  },

  commit_msg | {
    enabled | Bool | default = true,
    conventional_commits | Bool | default = true,
    max_length | Number | default = 72,
  },
} in

# ============================================================================
# EXPORT DEFAULT CONFIGURATION
# ============================================================================

{
  project = {
    name = "qed-ssg",
    version = "0.1.0",
    description = "Satellite SSG with MCP adapters for 28 static site generators",
    license = "MIT OR AGPL-3.0-or-later",
    repository = "https://github.com/hyperpolymath/qed-ssg",
  },

  adapters = {
    count = 28,
    registry = AdapterRegistry,
    timeout_ms = 30000,
  },

  build = {
    output_dir = "dist",
    source_dir = "content",
  },

  dev = {
    port = 3000,
  },

  test = {
    coverage_threshold = 70,
  },

  security = {
    audit_on_install = true,
    require_spdx = true,
  },

  ci = CIConfig,
  hooks = HooksConfig,
}
