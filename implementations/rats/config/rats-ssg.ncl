# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# rats-ssg main configuration (Nickel)

let ServerConfig = {
  port | Number | default = 3000,
  host | String | default = "127.0.0.1",
  logLevel | [| 'debug, 'info, 'warn, 'error |] | default = 'info,
} in

let AdapterConfig = {
  enabled | Array String | default = ["all"],
  timeout | Number | default = 30000,
  retries | Number | default = 3,
  concurrency | Number | default = 10,
} in

let SecurityConfig = {
  tlsVerify | Bool | default = true,
  maxRequestSize | Number | default = 10485760,
  rateLimit | {
    enabled | Bool | default = true,
    requests | Number | default = 100,
    window | String | default = "1m",
  },
} in

let MCPConfig = {
  version | String | default = "1.0",
  capabilities | Array String | default = ["tools", "resources"],
  toolPrefix | String | default = "",
} in

{
  # Main configuration
  server = ServerConfig & {
    port = 3000,
    host = "127.0.0.1",
    logLevel = 'info,
  },

  adapters = AdapterConfig & {
    enabled = [
      "zola",
      "hakyll",
      "mdbook",
      "cobalt",
      "cryogen",
      "serum",
      "franklin",
      "publish",
      "laika",
      "fornax",
      "perun",
      "frog",
      "pollen",
      "ema",
      "orchid",
      "scalatex",
      "yocaml",
      "marmot",
      "babashka",
      "coleslaw",
      "nimrod",
      "nimble-publisher",
      "reggae",
      "wub",
      "tableau",
      "staticwebpages",
      "documenter",
      "zotonic",
    ],
    timeout = 30000,
    retries = 3,
  },

  security = SecurityConfig & {
    tlsVerify = true,
    maxRequestSize = 10485760,
  },

  mcp = MCPConfig & {
    version = "1.0",
    capabilities = ["tools"],
  },
}
