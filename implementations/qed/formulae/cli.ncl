# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# formulae/cli.ncl — CLI command combinatorics and permutations

# ============================================================================
# CLI COMMAND TYPES
# ============================================================================

let Command = {
  name | String,
  description | String,
  args | Array String | default = [],
  flags | Array { name | String, short | String | optional, description | String } | default = [],
  subcommands | Array String | default = [],
} in

# ============================================================================
# BASE COMMANDS
# ============================================================================

let base_commands = {
  build = {
    name = "build",
    description = "Build the project",
    flags = [
      { name = "output", short = "o", description = "Output directory" },
      { name = "clean", short = "c", description = "Clean before build" },
      { name = "verbose", short = "v", description = "Verbose output" },
    ],
  },

  test = {
    name = "test",
    description = "Run tests",
    flags = [
      { name = "coverage", description = "Generate coverage report" },
      { name = "watch", short = "w", description = "Watch mode" },
      { name = "filter", short = "f", description = "Filter tests" },
    ],
    subcommands = ["unit", "e2e", "all"],
  },

  lint = {
    name = "lint",
    description = "Run linters",
    flags = [
      { name = "fix", description = "Auto-fix issues" },
      { name = "strict", description = "Strict mode" },
    ],
    subcommands = ["js", "scm", "all"],
  },

  adapter = {
    name = "adapter",
    description = "Manage SSG adapters",
    subcommands = ["list", "test", "sync", "info"],
    args = ["adapter_name"],
  },

  audit = {
    name = "audit",
    description = "Security audit",
    flags = [
      { name = "fix", description = "Auto-fix vulnerabilities" },
      { name = "json", description = "JSON output" },
    ],
  },

  dev = {
    name = "dev",
    description = "Development server",
    flags = [
      { name = "port", short = "p", description = "Port number" },
      { name = "open", description = "Open browser" },
    ],
  },
} in

# ============================================================================
# COMMAND COMBINATORS
# ============================================================================

# Combine two commands in sequence
let sequence = fun cmd1 cmd2 => {
  name = "${cmd1.name}-then-${cmd2.name}",
  description = "${cmd1.description}, then ${cmd2.description}",
  composed = [cmd1.name, cmd2.name],
  shell = "just ${cmd1.name} && just ${cmd2.name}",
} in

# Combine commands in parallel
let parallel = fun cmds => {
  name = std.array.fold_left (fun acc c => "${acc}-and-${c.name}") (std.array.first cmds).name (std.array.drop_first cmds),
  description = "Run in parallel: " ++ std.string.join ", " (std.array.map (fun c => c.name) cmds),
  composed = std.array.map (fun c => c.name) cmds,
  shell = std.string.join " & " (std.array.map (fun c => "just ${c.name}") cmds) ++ " && wait",
} in

# Create command with all flag permutations
let with_all_flags = fun cmd =>
  let flag_names = std.array.map (fun f => f.name) cmd.flags in
  let permutations = [
    [],  # no flags
  ] @ std.array.map (fun f => [f]) flag_names  # single flags
  in
  std.array.map
    (fun flags => {
      base = cmd.name,
      flags = flags,
      shell = "just ${cmd.name}" ++ (
        if std.array.length flags > 0
        then " " ++ std.string.join " " (std.array.map (fun f => "--${f}") flags)
        else ""
      ),
    })
    permutations
in

# ============================================================================
# WORKFLOW COMPOSITIONS
# ============================================================================

let workflows = {
  # Quick check
  quick = sequence base_commands.lint base_commands.test,

  # Full CI
  ci = {
    name = "ci",
    description = "Full CI pipeline",
    composed = ["lint", "test", "test-e2e", "audit", "build"],
    shell = "just lint && just test && just test-e2e && just audit && just build",
  },

  # Pre-commit
  pre_commit = {
    name = "pre-commit",
    description = "Pre-commit checks",
    composed = ["fmt-check", "lint", "test"],
    shell = "just fmt-check && just lint && just test",
  },

  # Release
  release = {
    name = "release",
    description = "Release preparation",
    composed = ["test-all", "audit", "build", "docs"],
    shell = "just test-all && just audit && just build && just docs",
  },

  # Adapter validation
  adapter_check = {
    name = "adapter-check",
    description = "Validate all adapters",
    composed = ["test-adapters", "lint-adapters", "docs-adapters"],
    shell = "just test-adapters && just lint && just docs-adapters",
  },
} in

# ============================================================================
# COMMAND MATRIX
# ============================================================================

let command_matrix = {
  # All possible command + flag combinations
  all_permutations =
    std.record.map
      (fun _name cmd => with_all_flags cmd)
      base_commands,

  # Useful command chains
  chains = [
    "just lint && just test",
    "just fmt && just lint && just test",
    "just clean && just build",
    "just test-adapters && just test",
    "just audit && just build",
  ],

  # Command aliases
  aliases = {
    t = "test",
    ta = "test-all",
    te = "test-e2e",
    l = "lint",
    f = "fmt",
    b = "build",
    c = "clean",
    d = "dev",
    a = "audit",
  },
} in

# ============================================================================
# EXPORT
# ============================================================================

{
  commands = base_commands,
  combinators = { sequence = sequence, parallel = parallel, with_all_flags = with_all_flags },
  workflows = workflows,
  matrix = command_matrix,

  # Generate shell completions
  completions = {
    bash = m%"
_qed_ssg_completions() {
  local commands="build test lint adapter audit dev clean docs"
  COMPREPLY=($(compgen -W "$commands" -- "${COMP_WORDS[COMP_CWORD]}"))
}
complete -F _qed_ssg_completions just
"%,
    zsh = m%"
_qed_ssg() {
  local commands=(build test lint adapter audit dev clean docs)
  _describe 'command' commands
}
compdef _qed_ssg just
"%,
    fish = m%"
complete -c just -a "build test lint adapter audit dev clean docs"
"%,
  },
}
