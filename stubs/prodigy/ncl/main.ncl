# SPDX-License-Identifier: AGPL-3.0-or-later
# prodigy-ssg Nickel Configuration
# RSR Gold compliant configuration schema

let Project = {
  name = "prodigy-ssg",
  version = "1.1.0",
  language = "Prolog",
  interpreter = "swipl",
  license = "AGPL-3.0-or-later",
}

let Paths = {
  source = "src",
  engine = "engine/src",
  content = "content",
  templates = "templates",
  output = "_site",
  tests = "tests",
  adapters = "adapters",
}

let Commands = {
  build = "just build",
  test = "just test",
  test_unit = "just test-unit",
  test_e2e = "just test-e2e",
  check = "just check",
  lint = "just lint",
  clean = "just clean",
  dev = "just dev",
  repl = "just repl",
  lsp = "just lsp",
}

let Deployment = {
  container = {
    engine = "podman",
    image_name = Project.name,
    tag = Project.version,
    build_cmd = "podman build -t ${Project.name}:${Project.version} .",
  },

  system = {
    install_prefix = "/usr/local",
    config_dir = "~/.config/prodigy",
    data_dir = "~/.local/share/prodigy",
  },
}

let CI = {
  platforms = ["ubuntu-latest"],
  prolog_version = "9.0",

  steps = {
    checkout = "actions/checkout@v4",
    setup_prolog = "sudo apt-get install -y swi-prolog",
    syntax_check = "just check",
    run_tests = "just test",
    security_audit = "just audit",
  },
}

let Security = {
  sast_tools = ["CodeQL", "Semgrep"],
  dependency_scanning = true,
  secrets_scanning = true,

  forbidden_patterns = [
    "password\\s*=",
    "secret\\s*=",
    "api_key\\s*=",
    "token\\s*=",
  ],
}

let RSRCompliance = {
  level = "gold",

  requirements = {
    spdx_headers = true,
    sha_pinned_actions = true,
    codeql_enabled = true,
    dependabot_enabled = true,
    security_policy = true,
    contributing_guide = true,
    code_of_conduct = true,
  },
}

# Export configuration
{
  project = Project,
  paths = Paths,
  commands = Commands,
  deployment = Deployment,
  ci = CI,
  security = Security,
  rsr = RSRCompliance,
}
