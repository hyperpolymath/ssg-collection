# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# build.ncl — eclipse-ssg build configuration

{
  project = {
    name = "eclipse-ssg",
    version = "0.2.0",
    description = "Satellite SSG implementation with 28 adapter support",
    license = "AGPL-3.0-or-later",
    repository = "https://github.com/hyperpolymath/eclipse-ssg",
  },

  build = {
    output_dir = "dist",
    source_dirs = ["ssg/src", "noteg-lang/src", "adapters"],
    minify = true,
    source_maps = false,
    target = "deno",
  },

  adapters = {
    enabled = [
      "zola", "hakyll", "cobalt", "ema", "serum",
      "cryogen", "franklin", "mdbook", "yocaml"
    ],
    directory = "adapters",
    parallel = 4,
    timeout_seconds = 300,
  },

  test = {
    coverage_threshold = 70,
    timeout_seconds = 300,
    directories = ["tests", "tests/e2e", "tests/verify"],
    parallel = true,
  },

  security = {
    codeql_enabled = true,
    secret_scanning = true,
    dependency_audit = true,
    spdx_required = true,
  },

  ci = {
    platforms = ["ubuntu-latest", "macos-latest"],
    deno_versions = ["v1.x", "canary"],
    cache_deps = true,
  },
}
