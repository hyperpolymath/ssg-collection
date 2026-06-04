// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Minimal Deno testing glue - required for ReScript interop

import { assertEquals as denoAssertEquals, assertExists as denoAssertExists } from "@std/assert";
import { describe as denoDescribe, it as denoIt } from "@std/testing/bdd";

export const assertEquals = denoAssertEquals;
export const assertExists = denoAssertExists;
export const describe = denoDescribe;
export const it = denoIt;
