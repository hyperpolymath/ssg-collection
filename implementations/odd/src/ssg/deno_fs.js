// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Minimal Deno FFI glue - required for ReScript interop

export async function readDir(path) {
  const entries = [];
  for await (const entry of Deno.readDir(path)) {
    entries.push({ name: entry.name, isFile: entry.isFile });
  }
  return entries;
}

export async function readTextFile(path) {
  return await Deno.readTextFile(path);
}

export async function writeTextFile(path, content) {
  await Deno.writeTextFile(path, content);
}

export async function mkdir(path, options) {
  await Deno.mkdir(path, options);
}

export function now() {
  return Date.now();
}
