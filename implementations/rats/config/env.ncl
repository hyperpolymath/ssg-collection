# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# Environment-specific configuration (Nickel)

let Env = fun env =>
  let base = {
    server = {
      port = 3000,
      host = "127.0.0.1",
    },
    logging = {
      level = 'info,
      format = 'json,
    },
    security = {
      tlsVerify = true,
    },
  } in

  if env == "development" then
    base & {
      server.host = "0.0.0.0",
      logging.level = 'debug,
      logging.format = 'pretty,
      security.tlsVerify = false,
      features = {
        hotReload = true,
        debugEndpoints = true,
      },
    }
  else if env == "test" then
    base & {
      server.port = 3001,
      logging.level = 'warn,
      features = {
        mockAdapters = true,
      },
    }
  else if env == "production" then
    base & {
      server.port = 8080,
      logging.level = 'warn,
      logging.format = 'json,
      security = {
        tlsVerify = true,
        rateLimit = {
          enabled = true,
          requests = 100,
          window = "1m",
        },
      },
      features = {
        metrics = true,
        healthCheck = true,
      },
    }
  else
    base
in

{
  development = Env "development",
  test = Env "test",
  production = Env "production",

  # Helper to get config by NODE_ENV or RATS_ENV
  current = Env "development",
}
