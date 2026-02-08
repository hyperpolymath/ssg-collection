# Forth Estate Engine Manifest (Nickel)
#
# Nickel provides:
# - Type contracts for validation
# - Merging for configuration inheritance
# - Functions for computed values

let SemVer = fun label value =>
  if std.string.is_match "^\\d+\\.\\d+\\.\\d+(-[a-zA-Z0-9]+)?$" value then
    value
  else
    std.contract.blame_with_message "Invalid semver: ${value}" label

let NonEmpty = fun label value =>
  if std.string.length value > 0 then
    value
  else
    std.contract.blame_with_message "String cannot be empty" label

let Feature = std.contract.from_predicate (fun x =>
  std.array.elem x ["markdown", "frontmatter", "templates", "syntax-highlighting",
                     "toc", "footnotes", "math", "diagrams", "search"])

{
  # Core metadata
  name
    | NonEmpty
    | doc "Engine identifier"
    = "forth-estate",

  language
    | NonEmpty
    | doc "Implementation language"
    = "Forth",

  implementation
    | NonEmpty
    | doc "Specific compiler/runtime"
    = "Gforth",

  version
    | SemVer
    | doc "Engine version (semver)"
    = "0.1.0",

  # Build configuration
  build_command
    | Array String
    | doc "Command to invoke the engine"
    = ["gforth", "forth-estate.fs", "-e"],

  build_args
    | Array String
    | doc "Default arguments"
    = ["build"],

  # Capabilities
  features
    | Array Feature
    | doc "Supported features"
    = ["markdown", "frontmatter", "templates"],

  # Dependencies
  dependencies = {
    runtime
      | Array String
      | doc "Required runtime dependencies"
      = ["gforth >= 0.7"],

    optional
      | Array String
      | doc "Optional dependencies for extra features"
      = [],
  },

  # Forth-specific configuration
  forth = {
    stack_size
      | std.number.Nat
      | doc "Data stack size in cells"
      | default
      = 65536,

    return_stack_size
      | std.number.Nat
      | doc "Return stack size in cells"
      | default
      = 32768,

    dictionary_size
      | std.number.Nat
      | doc "Dictionary size in bytes"
      | default
      = 1048576,

    # Gforth-specific options
    gforth_options = {
      debugging | Bool | default = false,
      profiling | Bool | default = false,
      image_file | String | optional,
    },
  },

  # Markdown processing options
  markdown = {
    extensions = {
      tables | Bool | default = true,
      strikethrough | Bool | default = true,
      autolinks | Bool | default = true,
      task_lists | Bool | default = true,
      footnotes | Bool | default = false,
      definition_lists | Bool | default = false,
      abbreviations | Bool | default = false,
    },

    code_blocks = {
      syntax_highlighting | Bool | default = false,
      line_numbers | Bool | default = false,
      language_labels | Bool | default = false,
    },

    html = {
      escape | Bool | default = true,
      allow_raw | Bool | default = false,
    },
  },

  # Template system options
  templates = {
    syntax = "mustache",  # or "handlebars", "jinja2", "custom"
    extension | String | default = ".html",

    delimiters = {
      open | String | default = "{{",
      close | String | default = "}}",
    },

    helpers | { .. } | default = {},
    partials_dir | String | optional,
  },

  # Frontmatter options
  frontmatter = {
    format | [| 'yaml, 'toml, 'json |] | default = 'yaml,
    delimiter | String | default = "---",
    required_fields | Array String | default = ["title"],
    date_format | String | default = "ISO-8601",
  },

  # Performance tuning
  performance = {
    parallel | Bool | default = false,
    incremental | Bool | default = false,
    cache | Bool | default = false,
    cache_dir | String | default = ".cache/forth-estate",
  },

  # Computed fields
  full_command =
    std.array.concat build_command build_args,

  display_name =
    "${name} (${language}/${implementation})",

  # Repository info
  repository = "https://github.com/hyperpolymath/forth-estate-ssg",
  documentation = "https://github.com/hyperpolymath/poly-ssg/tree/main/engines/forth-estate",
  author = "hyperpolymath",
  license = "MIT",
}
