export function join(...parts) {
  return parts
    .filter(Boolean)
    .join("/")
    .replaceAll("//", "/");
}

export function toPosixPath(p) {
  return p.replaceAll("\\", "/");
}

export async function ensureDir(path) {
  await Deno.mkdir(path, { recursive: true });
}

export async function readJson(path) {
  const txt = await Deno.readTextFile(path);
  return JSON.parse(txt);
}

export async function listFiles(dir) {
  const out = [];
  for await (const entry of Deno.readDir(dir)) {
    const p = join(dir, entry.name);
    if (entry.isFile) out.push(p);
    else if (entry.isDirectory) out.push(...(await listFiles(p)));
  }
  return out;
}

export async function copyDir(src, dst) {
  try {
    const stat = await Deno.stat(src);
    if (!stat.isDirectory) return;
  } catch {
    // no assets dir; that's fine
    return;
  }

  await ensureDir(dst);

  for await (const entry of Deno.readDir(src)) {
    const from = join(src, entry.name);
    const to = join(dst, entry.name);
    if (entry.isDirectory) {
      await copyDir(from, to);
    } else if (entry.isFile) {
      await ensureDir(dst);
      await Deno.copyFile(from, to);
    }
  }
}

export async function writeText(path, text) {
  await ensureDir(path.split("/").slice(0, -1).join("/") || ".");
  await Deno.writeTextFile(path, text);
}
