// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// NoteG SSG - Environment Configuration

@module("process") @scope("env")
external nodeEnv: option<string> = "NODE_ENV"

@module("process") @scope("env")
external notegConfigPath: option<string> = "NOTEG_CONFIG"

@module("process") @scope("env")
external notegOutputDir: option<string> = "NOTEG_OUTPUT_DIR"

@module("process") @scope("env")
external notegContentDir: option<string> = "NOTEG_CONTENT_DIR"

@module("process") @scope("env")
external notegDebug: option<string> = "NOTEG_DEBUG"

type environment = {
  isDevelopment: bool,
  isProduction: bool,
  isTest: bool,
  configPath: string,
  outputDir: string,
  contentDir: string,
  debug: bool,
}

let getEnv = (): environment => {
  let env = switch nodeEnv {
  | Some(e) => e
  | None => "development"
  }

  {
    isDevelopment: env == "development",
    isProduction: env == "production",
    isTest: env == "test",
    configPath: switch notegConfigPath {
    | Some(p) => p
    | None => "noteg.config.json"
    },
    outputDir: switch notegOutputDir {
    | Some(d) => d
    | None => "public"
    },
    contentDir: switch notegContentDir {
    | Some(d) => d
    | None => "content"
    },
    debug: switch notegDebug {
    | Some("true") | Some("1") => true
    | _ => false
    },
  }
}

let validateEnv = (env: environment): result<unit, string> => {
  if env.configPath == "" {
    Error("NOTEG_CONFIG cannot be empty")
  } else if env.outputDir == "" {
    Error("NOTEG_OUTPUT_DIR cannot be empty")
  } else if env.contentDir == "" {
    Error("NOTEG_CONTENT_DIR cannot be empty")
  } else {
    Ok()
  }
}
