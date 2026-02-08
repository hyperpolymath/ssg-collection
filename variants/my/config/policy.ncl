# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Hyperpolymath Language Policy Configuration
# This Nickel file defines and enforces the language policy for this repository

{
  # Policy version
  version = "1.0.0",

  # Allowed languages and tools
  allowed = {
    languages = [
      "rescript",
      "rust",
      "gleam",
      "bash",
      "posix-shell",
      "javascript", # Only for MCP protocol glue and Deno APIs
      "nickel",
      "guile-scheme",
    ],
    runtimes = [
      "deno",
    ],
    package_managers = [
      "guix",
      "nix",
      "deno", # deno.json imports
    ],
    build_tools = [
      "just",
      "must",
    ],
    mobile_frameworks = [
      "tauri", # >= 2.0
      "dioxus",
    ],
  },

  # Banned technologies - these MUST NOT be used
  banned = {
    languages = [
      "typescript",
      "go",
      "python", # except SaltStack
      "java",
      "kotlin",
      "swift",
      "dart",
    ],
    runtimes = [
      "node",
      "bun",
    ],
    package_managers = [
      "npm",
      "pnpm",
      "yarn",
    ],
    build_tools = [
      "make",
      "cmake",
      "meson",
      "bazel",
    ],
    mobile_frameworks = [
      "react-native",
      "flutter",
    ],
    file_patterns = [
      "*.ts",
      "*.tsx",
      "*.go",
      "*.py", # except salt/
      "package.json",
      "package-lock.json",
      "yarn.lock",
      "bun.lock",
      "bun.lockb",
      "pnpm-lock.yaml",
      "Makefile",
      "makefile",
      "GNUmakefile",
    ],
  },

  # Security requirements
  security = {
    banned_patterns = [
      "shell: true",
      "shell:true",
      "eval(",
      "Function(",
      "MD5",
      "SHA1", # for security purposes
      "http://", # plain HTTP URLs
    ],
    required_patterns = [
      "SPDX-License-Identifier:",
      "SPDX-FileCopyrightText:",
    ],
  },

  # Validation rules
  validate = {
    no_typescript = fun files =>
      std.array.filter (fun f => std.string.is_suffix ".ts" f || std.string.is_suffix ".tsx" f) files == [],
    no_package_json = fun files =>
      std.array.filter (fun f => std.string.is_suffix "package.json" f) files == [],
    no_makefile = fun files =>
      std.array.filter (fun f =>
        f == "Makefile" || f == "makefile" || f == "GNUmakefile"
      ) files == [],
    has_justfile = fun files =>
      std.array.any (fun f => f == "justfile" || f == "Justfile") files,
    has_deno_json = fun files =>
      std.array.any (fun f => f == "deno.json" || f == "deno.jsonc") files,
  },
}
