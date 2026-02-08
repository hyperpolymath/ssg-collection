// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// server.js - Entry point for rats-ssg MCP Server
// Loads Deno runtime glue and compiled ReScript modules

// Load runtime glue (provides Deno API bridges)
import "./runtime/deno_glue.js";

// Load compiled ReScript main module
import "./lib/es6/src/Main.res.js";
