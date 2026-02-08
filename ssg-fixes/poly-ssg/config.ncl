# poly-ssg configuration in Nickel
#
# Ultracombinatoric configuration for polyglot SSG
#
# Usage:
#   nickel export config.ncl
#   nickel export config.ncl --field site
#   nickel query config.ncl --field engines.forth-estate

let EngineManifest = {
  name | String,
  language | String,
  implementation | String | optional,
  version | String | default = "0.1.0",
  build_command | Array String,
  build_args | Array String | default = [],
  features | Array String | default = ["markdown", "frontmatter"],
  dependencies | {
    runtime | Array String | default = [],
    optional | Array String | default = [],
  } | default = {},
}

let FrontmatterSchema = {
  title | String,
  date | String | optional,
  template | String | default = "default",
  tags | Array String | default = [],
  draft | Bool | default = false,
  # Extensible - any additional fields allowed
  ..
}

let SiteConfig = {
  name | String,
  url | String | optional,
  description | String | optional,
  author | String | optional,
  language | String | default = "en",
}

let BuildConfig = {
  source | String | default = "content",
  output | String | default = "output",
  templates | String | default = "templates",
  assets | String | default = "assets",
  clean_before_build | Bool | default = true,
  parallel | Bool | default = true,
  drafts | Bool | default = false,
}

let TemplateConfig = {
  engine | String | default = "mustache",
  extension | String | default = ".html",
  partials_dir | String | default = "partials",
  helpers | { .. } | default = {},
}

let MarkdownConfig = {
  smart_punctuation | Bool | default = true,
  syntax_highlighting | Bool | default = true,
  heading_anchors | Bool | default = true,
  table_of_contents | Bool | default = false,
  footnotes | Bool | default = true,
  strikethrough | Bool | default = true,
  tasklists | Bool | default = true,
  extensions | Array String | default = [],
}

let OutputFormats = {
  html | {
    enabled | Bool | default = true,
    minify | Bool | default = false,
    pretty | Bool | default = true,
  } | default = {},
  json | {
    enabled | Bool | default = false,
    feed | Bool | default = false,
  } | default = {},
  xml | {
    enabled | Bool | default = false,
    rss | Bool | default = false,
    atom | Bool | default = false,
    sitemap | Bool | default = true,
  } | default = {},
}

# Engine definitions with full combinatoric options
let engines = {
  forth-estate = {
    name = "forth-estate",
    language = "Forth",
    implementation = "Gforth",
    version = "0.1.0",
    build_command = ["gforth", "forth-estate.fs", "-e"],
    build_args = ["build"],
    features = ["markdown", "frontmatter", "templates"],
    dependencies = {
      runtime = ["gforth >= 0.7"],
      optional = [],
    },
    # Forth-specific options
    stack_size = 65536,
    return_stack_size = 32768,
    dictionary_size = 1048576,
  },

  # Future engines (stubs for planning)
  webforge = {
    name = "webforge",
    language = "Ada",
    implementation = "GNAT",
    version = "0.0.1",
    build_command = ["./webforge"],
    features = ["markdown", "frontmatter", "templates", "spark-proofs"],
    dependencies = {
      runtime = ["gnat >= 12"],
      optional = ["spark2014"],
    },
    # Ada-specific options
    contracts = true,
    spark_mode = "on",
  },

  parallelpress = {
    name = "parallelpress",
    language = "Chapel",
    implementation = "chpl",
    version = "0.0.1",
    build_command = ["./parallelpress"],
    features = ["markdown", "frontmatter", "templates", "parallel"],
    dependencies = {
      runtime = ["chapel >= 1.30"],
      optional = [],
    },
    # Chapel-specific options
    num_locales = 1,
    tasks_per_locale = 0,  # 0 = auto
  },
}

# Combinatoric build matrix
let build_matrix = {
  engines = ["forth-estate"],  # Add more as implemented
  formats = ["html", "json", "xml"],
  environments = ["development", "production", "testing"],

  # Generate all combinations
  combinations =
    std.array.flat_map
      (fun engine =>
        std.array.flat_map
          (fun format =>
            std.array.map
              (fun env => { engine = engine, format = format, environment = env })
              environments)
          formats)
      engines,
}

# Main configuration export
{
  # Site metadata
  site | SiteConfig = {
    name = "My Poly-SSG Site",
    url = "https://example.com",
    description = "Built with poly-ssg",
    author = "hyperpolymath",
    language = "en",
  },

  # Build settings
  build | BuildConfig = {
    source = "content",
    output = "output",
    templates = "templates",
    assets = "assets",
    clean_before_build = true,
    parallel = true,
    drafts = false,
  },

  # Template settings
  templates | TemplateConfig = {
    engine = "mustache",
    extension = ".html",
    partials_dir = "partials",
  },

  # Markdown processing
  markdown | MarkdownConfig = {
    smart_punctuation = true,
    syntax_highlighting = true,
    heading_anchors = true,
    table_of_contents = false,
    footnotes = true,
  },

  # Output formats
  formats | OutputFormats = {
    html = { enabled = true, minify = false, pretty = true },
    json = { enabled = false },
    xml = { enabled = false, sitemap = true },
  },

  # Engine configurations
  engines = engines,

  # Build matrix for CI/testing
  matrix = build_matrix,

  # Frontmatter schema (for validation)
  frontmatter_schema = FrontmatterSchema,

  # Environment-specific overrides
  environments = {
    development = {
      build = { drafts = true, clean_before_build = false },
      formats = { html = { minify = false, pretty = true } },
    },
    production = {
      build = { drafts = false, clean_before_build = true },
      formats = { html = { minify = true, pretty = false } },
    },
    testing = {
      build = { drafts = true, parallel = false },
    },
  },

  # Hooks for extensibility
  hooks = {
    pre_build = [],
    post_build = [],
    pre_render = [],
    post_render = [],
    on_error = [],
  },

  # Plugin system (future)
  plugins = {
    enabled = [],
    config = {},
  },
}
