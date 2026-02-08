import { join } from "./util.mjs";

const DEFAULT_PORT = 8000;

export async function serveDir(distDir = "dist", port = DEFAULT_PORT) {
  const addr = `http://127.0.0.1:${port}`;
  console.log(`Serving ${distDir}/ at ${addr}`);

  Deno.serve({ port, hostname: "127.0.0.1" }, async (req) => {
    const url = new URL(req.url);
    let path = decodeURIComponent(url.pathname);

    if (path.endsWith("/")) path += "index.html";
    if (path === "/") path = "/index.html";

    // prevent traversal
    if (path.includes("..")) return new Response("Bad path", { status: 400 });

    const fsPath = join(distDir, path.replace(/^\//, ""));
    try {
      const data = await Deno.readFile(fsPath);
      return new Response(data, {
        status: 200,
        headers: { "content-type": contentType(fsPath) },
      });
    } catch {
      return new Response("Not found", { status: 404 });
    }
  });
}

function contentType(path) {
  const p = path.toLowerCase();
  if (p.endsWith(".html")) return "text/html; charset=utf-8";
  if (p.endsWith(".css")) return "text/css; charset=utf-8";
  if (p.endsWith(".js") || p.endsWith(".mjs")) return "text/javascript; charset=utf-8";
  if (p.endsWith(".json")) return "application/json; charset=utf-8";
  if (p.endsWith(".svg")) return "image/svg+xml";
  if (p.endsWith(".png")) return "image/png";
  return "application/octet-stream";
}
