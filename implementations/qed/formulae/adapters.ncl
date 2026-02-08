# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# formulae/adapters.ncl — Nickel formulae for SSG adapter generation

# ============================================================================
# ADAPTER TEMPLATE FORMULA
# ============================================================================

let mkAdapter = fun name language description binary =>
  let tools_prefix = std.string.lowercase name in
  {
    # Metadata
    meta = {
      name = name,
      language = language,
      description = description,
      spdx = "MIT",
    },

    # Runtime configuration
    runtime = {
      binary = binary,
      timeout_ms = 30000,
      retry_count = 3,
    },

    # Standard tools every adapter must have
    tools = [
      {
        name = "${tools_prefix}_init",
        description = "Initialize a new ${name} site",
        args = { path = "string" },
      },
      {
        name = "${tools_prefix}_build",
        description = "Build the ${name} site",
        args = { path = "string?", output = "string?" },
      },
      {
        name = "${tools_prefix}_serve",
        description = "Start ${name} development server",
        args = { path = "string?", port = "number?" },
      },
      {
        name = "${tools_prefix}_clean",
        description = "Clean build artifacts",
        args = { path = "string?" },
      },
      {
        name = "${tools_prefix}_version",
        description = "Get ${name} version",
        args = {},
      },
    ],

    # Generate JavaScript adapter code
    code = m%"
// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * ${name} adapter - ${description}
 */

export const name = "${name}";
export const language = "${language}";
export const description = "${description}";

let connected = false;
let binaryPath = "${binary}";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(binaryPath, {
    args,
    cwd: cwd || Deno.cwd(),
    stdout: "piped",
    stderr: "piped",
  });
  const output = await cmd.output();
  const decoder = new TextDecoder();
  return {
    success: output.success,
    stdout: decoder.decode(output.stdout),
    stderr: decoder.decode(output.stderr),
    code: output.code,
  };
}

export async function connect() {
  try {
    const result = await runCommand(["--version"]);
    connected = result.success;
    return connected;
  } catch {
    connected = false;
    return false;
  }
}

export async function disconnect() {
  connected = false;
}

export function isConnected() {
  return connected;
}

export const tools = [
  // Generated tools will be added here
];
"%,
  }
in

# ============================================================================
# ADAPTER INSTANCES
# ============================================================================

let adapters = {
  # Rust SSGs
  zola = mkAdapter "Zola" "Rust" "Fast static site generator with built-in Sass compilation" "zola",
  cobalt = mkAdapter "Cobalt" "Rust" "Straightforward static site generator" "cobalt",
  mdbook = mkAdapter "mdBook" "Rust" "Create books from Markdown" "mdbook",

  # Haskell SSGs
  hakyll = mkAdapter "Hakyll" "Haskell" "Haskell library for generating static sites" "stack",
  ema = mkAdapter "Ema" "Haskell" "Static site generator with hot reload" "stack",

  # Elixir SSGs
  serum = mkAdapter "Serum" "Elixir" "Simple static website generator" "mix",
  tableau = mkAdapter "Tableau" "Elixir" "Static site generator focused on data" "mix",

  # Clojure SSGs
  cryogen = mkAdapter "Cryogen" "Clojure" "Simple static site generator" "lein",
  perun = mkAdapter "Perun" "Clojure" "Composable static site generator" "boot",
  babashka = mkAdapter "Babashka" "Clojure" "Native Clojure interpreter for scripting" "bb",

  # Julia SSGs
  franklin = mkAdapter "Franklin.jl" "Julia" "Static site generator for technical blogging" "julia",
  documenter = mkAdapter "Documenter.jl" "Julia" "Documentation generator for Julia" "julia",

  # Scala SSGs
  laika = mkAdapter "Laika" "Scala" "Site and e-book generator" "sbt",

  # Racket SSGs
  frog = mkAdapter "Frog" "Racket" "Static blog generator" "raco",
  pollen = mkAdapter "Pollen" "Racket" "Book-publishing system" "raco",
} in

# ============================================================================
# FORMULA COMBINATORS
# ============================================================================

let combineAdapters = fun adapters =>
  let names = std.record.fields adapters in
  {
    count = std.array.length names,
    list = names,
    by_language = std.array.fold_left
      (fun acc name =>
        let adapter = adapters."%{name}" in
        let lang = adapter.meta.language in
        std.record.update lang
          (fun existing =>
            if existing == null then [name]
            else existing @ [name])
          acc)
      {}
      names,
  }
in

# ============================================================================
# EXPORT
# ============================================================================

{
  mkAdapter = mkAdapter,
  adapters = adapters,
  registry = combineAdapters adapters,

  # Generate all adapter files
  generate_all = std.record.map
    (fun _name adapter => adapter.code)
    adapters,
}
