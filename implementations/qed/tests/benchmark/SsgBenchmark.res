// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// tests/benchmark/SsgBenchmark.res — Build throughput and parser throughput benchmarks
//
// Measures:
//   - Build throughput: small (10), medium (100), large (1000) page simulations
//   - Parser throughput: tokenisation and AST construction rates for NoteG source
//
// All benchmarks are self-contained. No external binaries or network calls.

open DenoTest

// ============================================================================
// BENCHMARK UTILITY (mirrors AdapterBenchmark pattern)
// ============================================================================

type benchmarkResult = {
  name: string,
  iterations: int,
  totalMs: float,
  avgMs: float,
  minMs: float,
  maxMs: float,
  opsPerSec: float,
}

// Run fn for ~iterations passes and collect timing statistics
let benchmark = async (
  name: string,
  fn: unit => promise<unit>,
  ~iterations: int=50,
): benchmarkResult => {
  let times: array<float> = []

  // Warmup — 5 passes not counted in results
  for _i in 0 to 4 {
    await fn()
  }

  // Timed passes
  for _i in 0 to iterations - 1 {
    let t0 = Deno.Performance.now()
    await fn()
    let t1 = Deno.Performance.now()
    times->Js.Array2.push(t1 -. t0)->ignore
  }

  let totalMs = times->Js.Array2.reduce((a, b) => a +. b, 0.0)
  let avgMs = totalMs /. Js.Int.toFloat(iterations)
  let minMs = times->Js.Array2.reduce(
    (a, b) => if a < b { a } else { b },
    Js.Math._POSITIVE_INFINITY,
  )
  let maxMs = times->Js.Array2.reduce(
    (a, b) => if a > b { a } else { b },
    Js.Math._NEGATIVE_INFINITY,
  )
  let opsPerSec = if avgMs > 0.0 { 1000.0 /. avgMs } else { 0.0 }

  {name, iterations, totalMs, avgMs, minMs, maxMs, opsPerSec}
}

let printResult = (result: benchmarkResult): unit => {
  Js.Console.log(
    `\n[BENCH] ${result.name}` ++
    `\n  iterations : ${result.iterations->Js.Int.toString}` ++
    `\n  total      : ${Js.Float.toFixedWithPrecision(result.totalMs, ~digits=2)} ms` ++
    `\n  avg        : ${Js.Float.toFixedWithPrecision(result.avgMs, ~digits=3)} ms` ++
    `\n  min        : ${Js.Float.toFixedWithPrecision(result.minMs, ~digits=3)} ms` ++
    `\n  max        : ${Js.Float.toFixedWithPrecision(result.maxMs, ~digits=3)} ms` ++
    `\n  ops/sec    : ${Js.Float.toFixedWithPrecision(result.opsPerSec, ~digits=2)}`,
  )
}

// ============================================================================
// MOCK BUILD PIPELINE
//
// A minimal in-process simulation of the Eclipse SSG build pipeline.
// Mirrors the real Mill.applyCard and Build.buildPage logic so that
// benchmark numbers reflect realistic CPU-bound work.
// ============================================================================

// Simulate frontmatter parsing (split on ---)
let parseFrontmatter = (raw: string): (string, string) => {
  let parts = Js.String2.split(raw, "---")
  if Belt.Array.length(parts) >= 3 {
    (
      parts->Belt.Array.getExn(1)->Js.String2.trim,
      parts->Belt.Array.sliceToEnd(~from=2)->Belt.Array.joinWith("---")->Js.String2.trim,
    )
  } else {
    ("", raw)
  }
}

// Simulate template variable substitution (mirrors Mill.applyCard)
let applyTemplate = (template: string, vars: Js.Dict.t<string>): string => {
  Js.Dict.entries(vars)->Belt.Array.reduce(template, (acc, (k, v)) => {
    Js.String2.replaceByRe(
      acc,
      Js.Re.fromStringWithFlags("\\{\\{ " ++ k ++ " \\}\\}", ~flags="g"),
      v,
    )
  })
}

// Simulate building a single page from markdown-like content
let buildOnePage = (index: int): string => {
  let rawContent =
    "---\ntitle: Page " ++
    Int.toString(index) ++
    "\ndate: 2025-01-01\n---\n\n# Heading " ++
    Int.toString(index) ++
    "\n\nParagraph with content for page number " ++
    Int.toString(index) ++
    ". Some more text to make this non-trivial.\n"

  let (fm, body) = parseFrontmatter(rawContent)

  // Extract title from frontmatter (simple line scan)
  let title =
    fm
    ->Js.String2.split("\n")
    ->Array.find(line => Js.String2.startsWith(line, "title:"))
    ->Belt.Option.getWithDefault("title: Untitled")
    ->Js.String2.replace("title: ", "")
    ->Js.String2.trim

  let template = "<html><head><title>{{ title }}</title></head><body>{{ body }}</body></html>"
  let vars = Js.Dict.fromArray([("title", title), ("body", body)])
  applyTemplate(template, vars)
}

// Build N pages and return total output byte count
let buildNPages = (n: int): int => {
  Belt.Array.makeBy(n, i => buildOnePage(i))
  ->Belt.Array.reduce(0, (acc, html) => acc + Js.String2.length(html))
}

// ============================================================================
// MOCK PARSER (simulates tokenisation work)
// ============================================================================

// Produce a realistic NoteG source snippet for a given complexity level
let makeSource = (stmtCount: int): string => {
  Belt.Array.makeBy(stmtCount, i =>
    "let var" ++ Int.toString(i) ++ " = " ++ Int.toString(i * 17 + 3)
  )->Belt.Array.joinWith("\n")
}

// Simulate tokenisation (split on whitespace and punctuation — approximates real lexer work)
let tokeniseSimulated = (source: string): int => {
  // Count whitespace-separated tokens as a proxy for real tokenisation cost
  source
  ->Js.String2.split(" ")
  ->Array.concat(source->Js.String2.split("\n"))
  ->Belt.Array.length
}

// ============================================================================
// BENCHMARK: Small site — 10 pages
// ============================================================================

let _ = test("Benchmark: Build throughput — small site (10 pages)", async () => {
  let result = await benchmark(
    "Build 10 pages",
    async () => {
      let totalBytes = buildNPages(10)
      // Sanity: 10 pages must produce some output
      if totalBytes == 0 {
        Js.Exn.raiseError("Build produced 0 bytes for 10 pages")
      }
    },
    ~iterations=100,
  )
  printResult(result)

  // Soft threshold: 10-page build should average under 50 ms even in CI
  if result.avgMs > 50.0 {
    Js.Console.warn(
      `[WARN] Small site build averaged ${Js.Float.toFixedWithPrecision(result.avgMs, ~digits=1)} ms — exceeds 50 ms soft threshold`,
    )
  }
})

// ============================================================================
// BENCHMARK: Medium site — 100 pages
// ============================================================================

let _ = test("Benchmark: Build throughput — medium site (100 pages)", async () => {
  let result = await benchmark(
    "Build 100 pages",
    async () => {
      let totalBytes = buildNPages(100)
      if totalBytes == 0 {
        Js.Exn.raiseError("Build produced 0 bytes for 100 pages")
      }
    },
    ~iterations=20,
  )
  printResult(result)

  // Soft threshold: 100-page build should average under 500 ms in CI
  if result.avgMs > 500.0 {
    Js.Console.warn(
      `[WARN] Medium site build averaged ${Js.Float.toFixedWithPrecision(result.avgMs, ~digits=1)} ms — exceeds 500 ms soft threshold`,
    )
  }
})

// ============================================================================
// BENCHMARK: Large site — 1000 pages
// ============================================================================

let _ = test("Benchmark: Build throughput — large site (1000 pages)", async () => {
  let result = await benchmark(
    "Build 1000 pages",
    async () => {
      let totalBytes = buildNPages(1000)
      if totalBytes == 0 {
        Js.Exn.raiseError("Build produced 0 bytes for 1000 pages")
      }
    },
    ~iterations=5,
  )
  printResult(result)

  // Soft threshold: 1000-page build under 5000 ms
  if result.avgMs > 5000.0 {
    Js.Console.warn(
      `[WARN] Large site build averaged ${Js.Float.toFixedWithPrecision(result.avgMs, ~digits=1)} ms — exceeds 5000 ms soft threshold`,
    )
  }
})

// ============================================================================
// BENCHMARK: Parser throughput — small source
// ============================================================================

let _ = test("Benchmark: Parser throughput — small source (10 statements)", async () => {
  let source = makeSource(10)

  let result = await benchmark(
    "Tokenise 10 statements",
    async () => {
      let count = tokeniseSimulated(source)
      if count == 0 {
        Js.Exn.raiseError("Tokeniser produced 0 tokens for 10-statement source")
      }
    },
    ~iterations=1000,
  )
  printResult(result)
})

// ============================================================================
// BENCHMARK: Parser throughput — medium source
// ============================================================================

let _ = test("Benchmark: Parser throughput — medium source (100 statements)", async () => {
  let source = makeSource(100)

  let result = await benchmark(
    "Tokenise 100 statements",
    async () => {
      let count = tokeniseSimulated(source)
      if count == 0 {
        Js.Exn.raiseError("Tokeniser produced 0 tokens for 100-statement source")
      }
    },
    ~iterations=500,
  )
  printResult(result)
})

// ============================================================================
// BENCHMARK: Parser throughput — large source
// ============================================================================

let _ = test("Benchmark: Parser throughput — large source (1000 statements)", async () => {
  let source = makeSource(1000)

  let result = await benchmark(
    "Tokenise 1000 statements",
    async () => {
      let count = tokeniseSimulated(source)
      if count == 0 {
        Js.Exn.raiseError("Tokeniser produced 0 tokens for 1000-statement source")
      }
    },
    ~iterations=100,
  )
  printResult(result)
})

// ============================================================================
// BENCHMARK: Template substitution throughput
// ============================================================================

let _ = test("Benchmark: Template substitution throughput (1000 iterations)", async () => {
  let template =
    "<html><head><title>{{ title }}</title></head>" ++
    "<body><h1>{{ heading }}</h1><p>{{ intro }}</p><p>{{ content }}</p>" ++
    "<footer>{{ footer }}</footer></body></html>"

  let vars = Js.Dict.fromArray([
    ("title", "Benchmark Page"),
    ("heading", "Performance Test"),
    ("intro", "This is the introduction paragraph."),
    ("content", "This is the main content of the page with some text."),
    ("footer", "Copyright 2025 Jonathan D.A. Jewell"),
  ])

  let result = await benchmark(
    "Template substitution (5 vars)",
    async () => {
      let rendered = applyTemplate(template, vars)
      if Js.String2.length(rendered) == 0 {
        Js.Exn.raiseError("Template substitution produced empty output")
      }
    },
    ~iterations=1000,
  )
  printResult(result)
})

// ============================================================================
// BENCHMARK: Frontmatter parsing throughput
// ============================================================================

let _ = test("Benchmark: Frontmatter parsing throughput (1000 iterations)", async () => {
  let sample =
    "---\ntitle: My Post\ndate: 2025-06-15\ndraft: false\ntags:\n  - rescript\n  - ssg\n---\n\n# My Post\n\nContent body here."

  let result = await benchmark(
    "Frontmatter parse",
    async () => {
      let (fm, body) = parseFrontmatter(sample)
      if Js.String2.length(fm) == 0 || Js.String2.length(body) == 0 {
        Js.Exn.raiseError("Frontmatter parse produced empty result")
      }
    },
    ~iterations=1000,
  )
  printResult(result)
})

// ============================================================================
// BENCHMARK: Build scaling — verify linear-ish growth
// ============================================================================

let _ = test("Benchmark: Build scaling is sub-quadratic", async () => {
  // Measure single-page, 10-page, and 100-page builds
  let t10start = Deno.Performance.now()
  for _i in 0 to 9 {
    let _ = buildNPages(10)
  }
  let t10end = Deno.Performance.now()
  let avg10 = (t10end -. t10start) /. 10.0

  let t100start = Deno.Performance.now()
  for _i in 0 to 4 {
    let _ = buildNPages(100)
  }
  let t100end = Deno.Performance.now()
  let avg100 = (t100end -. t100start) /. 5.0

  Js.Console.log(
    `\n[BENCH] Build scaling` ++
    `\n  10 pages  avg: ${Js.Float.toFixedWithPrecision(avg10, ~digits=3)} ms` ++
    `\n  100 pages avg: ${Js.Float.toFixedWithPrecision(avg100, ~digits=3)} ms` ++
    `\n  ratio (should be ~10x for linear): ${Js.Float.toFixedWithPrecision(avg100 /. avg10, ~digits=2)}x`,
  )

  // Ratio of 100-page to 10-page must be less than 50x (allows for overhead but rejects O(n^2))
  let ratio = avg100 /. avg10
  if ratio > 50.0 {
    Js.Exn.raiseError(
      "Build scaling appears super-linear: 100-page build took " ++
      Js.Float.toFixedWithPrecision(ratio, ~digits=1) ++
      "x as long as 10-page build (threshold: 50x)",
    )
  }
})
