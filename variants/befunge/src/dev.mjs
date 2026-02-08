import { buildSite } from "./ssg.mjs";
import { serveDir } from "./serve.mjs";

export async function dev() {
  await buildSite();
  // fire-and-forget server
  serveDir();

  console.log("Watching for changes in ./site, ./assets, ./befunge-ssg.json …");

  const watcher = Deno.watchFs(["site", "assets", "befunge-ssg.json"], { recursive: true });
  let pending = false;

  for await (const _event of watcher) {
    if (pending) continue;
    pending = true;

    // debounce a little
    setTimeout(async () => {
      try {
        await buildSite();
      } catch (e) {
        console.error("Build failed:", e?.message ?? e);
      } finally {
        pending = false;
      }
    }, 150);
  }
}
