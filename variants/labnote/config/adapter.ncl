# Nickel adapter schema
# Type-safe configuration for SSG adapters

let Tool = {
  name
    | String
    | doc "Tool name in format {ssg}_{action}",
  description
    | String
    | doc "Human-readable description",
  inputSchema
    | { type | String, properties | { _ : _ } | optional, required | Array String | optional }
    | doc "JSON Schema for input validation",
}

let Adapter = {
  name
    | String
    | doc "SSG display name",
  language
    | String
    | doc "Implementation language",
  description
    | String
    | doc "Brief SSG description",
  binary_path
    | String
    | doc "Path to SSG binary",
  tools
    | Array Tool
    | doc "Available MCP tools",

  # Computed fields
  tool_count
    | Number
    | default = 0
    | doc "Number of tools",
}

# Example Zola adapter config
let zola_adapter : Adapter = {
  name = "Zola",
  language = "Rust",
  description = "Fast static site generator with Sass and syntax highlighting",
  binary_path = "zola",
  tools = [
    {
      name = "zola_init",
      description = "Initialize a new Zola site",
      inputSchema = {
        type = "object",
        properties = {
          path = { type = "string", description = "Path for new site" },
          force = { type = "boolean", description = "Overwrite existing" },
        },
        required = ["path"],
      },
    },
    {
      name = "zola_build",
      description = "Build the Zola site",
      inputSchema = {
        type = "object",
        properties = {
          path = { type = "string" },
          outputDir = { type = "string" },
          drafts = { type = "boolean" },
        },
      },
    },
    {
      name = "zola_serve",
      description = "Start development server",
      inputSchema = {
        type = "object",
        properties = {
          port = { type = "number" },
          open = { type = "boolean" },
        },
      },
    },
    {
      name = "zola_version",
      description = "Get Zola version",
      inputSchema = { type = "object" },
    },
  ],
  tool_count = 4,
}

in {
  Adapter = Adapter,
  Tool = Tool,
  examples = {
    zola = zola_adapter,
  },
}
