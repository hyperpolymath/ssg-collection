// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Minimal Deno FFI glue for adapter loading - required for ReScript interop

const ADAPTERS = [
  "babashka", "cobalt", "coleslaw", "cryogen", "documenter",
  "ema", "fornax", "franklin", "frog", "hakyll",
  "laika", "marmot", "mdbook", "nimble-publisher", "nimrod",
  "orchid", "perun", "pollen", "publish", "reggae",
  "scalatex", "serum", "staticwebpages", "tableau", "wub",
  "yocaml", "zola", "zotonic"
];

export async function loadAdapter(name) {
  return await import(`../adapters/${name}.js`);
}

export async function loadAllAdapters() {
  const adapters = {};
  for (const name of ADAPTERS) {
    try {
      adapters[name] = await loadAdapter(name);
    } catch {
      // Adapter not available
    }
  }
  return adapters;
}
