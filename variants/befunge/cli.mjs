import { buildSite } from "./src/ssg.mjs";
import { serveDir } from "./src/serve.mjs";
import { dev } from "./src/dev.mjs";

const cmd = Deno.args[0] ?? "help";

switch (cmd) {
  case "build":
    await buildSite();
    break;
  case "serve":
    await serveDir();
    break;
  case "dev":
    await dev();
    break;
  default:
    console.log(`befunge-ssg
Usage:
  deno task build   # build site into ./dist
  deno task serve   # serve ./dist on http://127.0.0.1:8000
  deno task dev     # watch + rebuild + serve
`);
}
