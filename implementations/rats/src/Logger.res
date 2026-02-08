// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Logger.res - Logging utilities

open Types

let currentLevel = ref(Info)

let setLevel = (level: logLevel) => {
  currentLevel := level
}

let setLevelFromString = (s: string) => {
  currentLevel := logLevelFromString(s)
}

let shouldLog = (level: logLevel): bool => {
  logLevelToInt(level) >= logLevelToInt(currentLevel.contents)
}

let debug = (msg: string) => {
  if shouldLog(Debug) {
    Js.Console.log(`[DEBUG] ${msg}`)
  }
}

let info = (msg: string) => {
  if shouldLog(Info) {
    Js.Console.log(`[INFO] ${msg}`)
  }
}

let warn = (msg: string) => {
  if shouldLog(Warn) {
    Js.Console.warn(`[WARN] ${msg}`)
  }
}

let error = (msg: string) => {
  Js.Console.error(`[ERROR] ${msg}`)
}
