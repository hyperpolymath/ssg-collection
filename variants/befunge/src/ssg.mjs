import { join, toPosixPath, ensureDir, readJson, listFiles, copyDir, writeText } from "./util.mjs";
import * as RS from "../rs/lib/es6/src/Adapter.js"; // built by ReScript
import * as RRender from "../rs/lib/es6/src/Render.js";

const DEFAULT_CONFIG = "befunge-ssg.json";

export async function buildSite(configPath = DEFAULT_CONFIG) {
  const cfg = await readJson(configPath);

  const inputDir = cfg.inputDir ?? "site";
  const outputDir = cfg.outputDir ?? "dist";
  const programsDir = cfg.programsDir ?? "programs";
  const assetsDir = cfg.assetsDir ?? "assets";
  const siteTitle = cfg.siteTitle ?? "Befunge Gallery";
  const baseUrl = cfg.baseUrl ?? "/";

  await ensureDir(outputDir);
  await ensureDir(join(outputDir, "programs"));

  // Copy assets (site-wide)
  await copyDir(assetsDir, join(outputDir, "assets"));

  const programFiles = (await listFiles(join(inputDir, programsDir)))
    .filter((p) => p.toLowerCase().endsWith(".bf"))
    .sort();

  const programs = [];
  for (const path of programFiles) {
    const content = await Deno.readTextFile(path);
    const filename = path.split("/").pop() ?? "unknown.bf";
    const program = RS.parseProgram(content, filename, cfg.maxGridWidth ?? 160, cfg.maxGridHeight ?? 80);
    programs.push(program);

    const programOutDir = join(outputDir, "programs", program.slug);
    await ensureDir(programOutDir);

    const html = RRender.renderProgramPage(siteTitle, baseUrl, program);
    await writeText(join(programOutDir, "index.html"), html);
  }

  const indexHtml = RRender.renderIndexPage(siteTitle, baseUrl, programs);
  await writeText(join(outputDir, "index.html"), indexHtml);

  const robots = `User-agent: *\nAllow: /\n`;
  await writeText(join(outputDir, "robots.txt"), robots);

  console.log(`Built ${programs.length} program page(s) into ${outputDir}/`);
}

// ---- tiny internal util layer (no TS) ----
/**
 * NOTE: util.mjs is created below in this same response.
 * Kept separate so ssg.mjs stays readable.
 */
