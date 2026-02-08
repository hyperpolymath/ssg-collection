// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// env.res — Environment variable handling

// ============================================================
// ENVIRONMENT CONFIGURATION
// ============================================================

// Get environment variable with fallback
let get = (key: string, fallback: string): string => {
  switch Js.Dict.get(Node.Process.process["env"], key) {
  | Some(value) => value
  | None => fallback
  }
}

// Get required environment variable
let getRequired = (key: string): result<string, string> => {
  switch Js.Dict.get(Node.Process.process["env"], key) {
  | Some(value) => Ok(value)
  | None => Error(`Required environment variable ${key} not set`)
  }
}

// Check if environment variable is set
let has = (key: string): bool => {
  Js.Dict.get(Node.Process.process["env"], key)->Belt.Option.isSome
}

// Environment modes
type envMode =
  | Development
  | Production
  | Test

// Get current environment mode
let getMode = (): envMode => {
  switch get("NODE_ENV", "development") {
  | "production" => Production
  | "test" => Test
  | _ => Development
  }
}

// Is production mode
let isProduction = (): bool => {
  getMode() == Production
}

// Is development mode
let isDevelopment = (): bool => {
  getMode() == Development
}

// Configuration from environment
type envConfig = {
  mode: envMode,
  port: int,
  host: string,
  debug: bool,
}

// Load configuration from environment
let loadConfig = (): envConfig => {
  {
    mode: getMode(),
    port: get("PORT", "8080")->Belt.Int.fromString->Belt.Option.getWithDefault(8080),
    host: get("HOST", "localhost"),
    debug: get("DEBUG", "false") == "true",
  }
}
