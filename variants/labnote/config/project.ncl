# Nickel configuration for labnote-ssg
# See: https://nickel-lang.org/

let AdapterConfig = {
  name
    | String
    | doc "Name of the SSG adapter",
  language
    | String
    | doc "Programming language of the SSG",
  binary
    | String
    | doc "Binary executable name",
  version
    | String
    | default = "latest"
    | doc "Required version",
}

let ProjectConfig = {
  name
    | String
    | default = "labnote-ssg"
    | doc "Project name",
  version
    | String
    | doc "Project version",
  adapters
    | Array AdapterConfig
    | doc "List of adapter configurations",

  settings = {
    deno_version
      | String
      | default = "1.40.0"
      | doc "Deno runtime version",

    coverage_minimum
      | Number
      | default = 70
      | doc "Minimum test coverage percentage",

    ci = {
      lint
        | Bool
        | default = true,
      fmt
        | Bool
        | default = true,
      test
        | Bool
        | default = true,
      security
        | Bool
        | default = true,
    },
  },
}

# Default project configuration
let config : ProjectConfig = {
  name = "labnote-ssg",
  version = "0.3.0",
  adapters = [
    { name = "Zola", language = "Rust", binary = "zola" },
    { name = "mdBook", language = "Rust", binary = "mdbook" },
    { name = "Cobalt", language = "Rust", binary = "cobalt" },
    { name = "Serum", language = "Elixir", binary = "mix" },
    { name = "Hakyll", language = "Haskell", binary = "hakyll-init" },
    { name = "Cryogen", language = "Clojure", binary = "lein" },
    { name = "Franklin", language = "Julia", binary = "julia" },
    { name = "Laika", language = "Scala", binary = "sbt" },
  ],
}

in config
