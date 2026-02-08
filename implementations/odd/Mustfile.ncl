# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Mustfile.ncl - Contract of Physical State for odd-ssg
# Authority-First Deployment System using Nickel

let Project = {
  name = "odd-ssg",
  version = "0.1.0",
  description = "Probabilistic static site generator with Mill-Based Synthesis engine",
  license = "AGPL-3.0-or-later",
  repository = "https://github.com/hyperpolymath/odd-ssg",
} in

let LanguagePolicy = {
  # Allowed languages and their use cases
  allowed = {
    rescript = {
      use_case = "Primary application code",
      extensions = [".res", ".resi"],
      enforced = true,
    },
    deno = {
      use_case = "Runtime & package management",
      config = "deno.json",
      enforced = true,
    },
    javascript = {
      use_case = "FFI glue only (minimal)",
      extensions = [".js"],
      locations = ["src/ssg/deno_fs.js", "src/adapter_loader.js", "src/noteg-lang/lsp/main.js", "src/noteg-mcp/main.js", "src/tests/test_utils.js"],
      enforced = true,
    },
    nickel = {
      use_case = "Configuration language",
      extensions = [".ncl"],
      enforced = true,
    },
    bash = {
      use_case = "Scripts, automation",
      extensions = [".sh"],
      minimal = true,
    },
  },

  # Banned languages - must not exist in codebase
  banned = {
    typescript = {
      extensions = [".ts", ".tsx", ".mts", ".cts"],
      replacement = "ReScript",
    },
    nodejs = {
      indicators = ["package.json", "package-lock.json", "node_modules"],
      replacement = "Deno",
    },
    bun = {
      indicators = ["bun.lockb", "bunfig.toml"],
      replacement = "Deno",
    },
    npm = {
      indicators = ["npm-shrinkwrap.json", ".npmrc"],
      replacement = "Deno",
    },
    makefile = {
      indicators = ["Makefile", "makefile", "GNUmakefile", "*.mk"],
      replacement = "justfile",
    },
    go = {
      extensions = [".go"],
      replacement = "Rust",
    },
  },
} in

let BuildTargets = {
  rescript = {
    command = "npx rescript build",
    watch = "npx rescript build -w",
    clean = "npx rescript clean",
    outputs = ["src/**/*.res.js"],
  },

  deno = {
    check = "deno check src/**/*.js",
    lint = "deno lint",
    fmt = "deno fmt",
    test = "deno test --allow-read --allow-write src/tests/",
  },

  docker = {
    build = "podman build -t odd-ssg:latest .",
    run = "podman run -it --rm -v $(pwd):/app:Z odd-ssg:latest",
  },
} in

let Deployment = {
  # Container-first deployment
  container = {
    runtime = "podman",
    image = "odd-ssg",
    registry = "ghcr.io/hyperpolymath",
  },

  # State transitions
  transitions = {
    dev = {
      from = "clean",
      to = "development",
      commands = ["deno task rescript:build"],
    },
    test = {
      from = "development",
      to = "tested",
      commands = ["deno task test"],
    },
    build = {
      from = "tested",
      to = "built",
      commands = ["deno task build"],
    },
    deploy = {
      from = "built",
      to = "deployed",
      commands = ["podman push"],
    },
  },
} in

let PolicyEnforcement = {
  pre_commit = {
    check_banned_files = true,
    check_spdx_headers = true,
    check_no_secrets = true,
    run_lint = true,
    run_fmt_check = true,
  },

  ci = {
    check_policy = "deno task policy:check",
    build = "deno task rescript:build",
    lint = "deno task lint",
    test = "deno task test",
  },
} in

{
  project = Project,
  language_policy = LanguagePolicy,
  build = BuildTargets,
  deployment = Deployment,
  enforcement = PolicyEnforcement,

  # Metadata
  mustfile_version = "0.1.0",
  nickel_version = "1.0",
}
