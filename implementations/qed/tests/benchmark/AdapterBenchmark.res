// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// tests/benchmark/AdapterBenchmark.res - Performance benchmarks for SSG adapters

open DenoTest

// ============================================================================
// BENCHMARK UTILITIES
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

let benchmark = async (name: string, fn: unit => promise<unit>, ~iterations: int=100): benchmarkResult => {
  let times = []

  // Warmup
  for _i in 0 to 4 {
    await fn()
  }

  // Actual benchmark
  for _i in 0 to iterations - 1 {
    let start = Deno.Performance.now()
    await fn()
    let endTime = Deno.Performance.now()
    times->Js.Array2.push(endTime -. start)->ignore
  }

  let totalMs = times->Js.Array2.reduce((a, b) => a +. b, 0.0)
  let avgMs = totalMs /. Js.Int.toFloat(iterations)
  let minMs =
    times->Js.Array2.reduce(
      (a, b) =>
        if a < b {
          a
        } else {
          b
        },
      Js.Math._POSITIVE_INFINITY,
    )
  let maxMs =
    times->Js.Array2.reduce(
      (a, b) =>
        if a > b {
          a
        } else {
          b
        },
      Js.Math._NEGATIVE_INFINITY,
    )
  let opsPerSec = 1000.0 /. avgMs

  {name, iterations, totalMs, avgMs, minMs, maxMs, opsPerSec}
}

let formatResult = (result: benchmarkResult): string => {
  `
${result.name}
  Iterations: ${result.iterations->Js.Int.toString}
  Total:      ${Js.Float.toFixedWithPrecision(result.totalMs, ~digits=2)}ms
  Average:    ${Js.Float.toFixedWithPrecision(result.avgMs, ~digits=3)}ms
  Min:        ${Js.Float.toFixedWithPrecision(result.minMs, ~digits=3)}ms
  Max:        ${Js.Float.toFixedWithPrecision(result.maxMs, ~digits=3)}ms
  Ops/sec:    ${Js.Float.toFixedWithPrecision(result.opsPerSec, ~digits=2)}
`
}

// ============================================================================
// ADAPTER LOADING BENCHMARKS
// ============================================================================

let _ = test("Benchmark: Adapter module loading", async () => {
  let results = []

  // Benchmark loading individual adapters
  let adaptersToTest = ["zola", "hakyll", "serum", "cobalt", "franklin"]

  for adapterName in adaptersToTest {
    let result = await benchmark(
      `Load ${adapterName} adapter`,
      async () => {
        // Note: In real benchmark, we'd need to clear cache
        try {
          let _ = await Adapter.importAdapter(`../../adapters/${adapterName}.js`)
        } catch {
        | _ => ()
        }
      },
      ~iterations=50,
    )
    results->Js.Array2.push(result)->ignore
  }

  // Print results
  Js.Console.log("\n=== Adapter Loading Benchmarks ===")
  results->Js.Array2.forEach(r => Js.Console.log(formatResult(r)))
})

let _ = test("Benchmark: Load all 28 adapters", async () => {
  let entries = await Deno.readDirAsArray("./adapters")
  let adapterFiles =
    entries
    ->Js.Array2.filter(entry => entry.isFile && Js.String2.endsWith(entry.name, ".js"))
    ->Js.Array2.map(entry => entry.name)

  let result = await benchmark(
    "Load all 28 adapters",
    async () => {
      let _ = await Js.Promise.all(
        adapterFiles->Js.Array2.map(file => Adapter.importAdapter(`../../adapters/${file}`)),
      )
    },
    ~iterations=20,
  )

  Js.Console.log("\n=== Bulk Loading Benchmark ===")
  Js.Console.log(formatResult(result))
})

// ============================================================================
// ADAPTER CONNECTION BENCHMARKS
// ============================================================================

let _ = test("Benchmark: Adapter connect/disconnect cycle", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  let result = await benchmark(
    "Zola connect/disconnect cycle",
    async () => {
      let _ = await adapter.connect()
      await adapter.disconnect()
    },
    ~iterations=50,
  )

  Js.Console.log("\n=== Connection Cycle Benchmark ===")
  Js.Console.log(formatResult(result))
})

// ============================================================================
// TOOL EXECUTION BENCHMARKS
// ============================================================================

let _ = test("Benchmark: Tool lookup", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  let result = await benchmark(
    "Tool lookup by name",
    async () => {
      let _ = adapter.tools->Js.Array2.find(t => t.name === "zola_version")
      let _ = adapter.tools->Js.Array2.find(t => t.name === "zola_build")
      let _ = adapter.tools->Js.Array2.find(t => t.name === "zola_init")
    },
    ~iterations=1000,
  )

  Js.Console.log("\n=== Tool Lookup Benchmark ===")
  Js.Console.log(formatResult(result))
})

let _ = test("Benchmark: isConnected check", async () => {
  let adapter = await Adapter.importAdapter("../../adapters/zola.js")

  let result = await benchmark(
    "isConnected() call",
    async () => {
      let _ = adapter.isConnected()
    },
    ~iterations=10000,
  )

  Js.Console.log("\n=== Connection Check Benchmark ===")
  Js.Console.log(formatResult(result))
})

// ============================================================================
// CONCURRENT OPERATIONS BENCHMARKS
// ============================================================================

let _ = test("Benchmark: Concurrent adapter loading", async () => {
  let adapters = [
    "zola",
    "cobalt",
    "mdbook",
    "hakyll",
    "ema",
    "serum",
    "tableau",
    "franklin",
    "frog",
    "pollen",
  ]

  // Sequential loading
  let sequentialResult = await benchmark(
    "Sequential load (10 adapters)",
    async () => {
      for adapterName in adapters {
        let _ = await Adapter.importAdapter(`../../adapters/${adapterName}.js`)
      }
    },
    ~iterations=20,
  )

  // Parallel loading
  let parallelResult = await benchmark(
    "Parallel load (10 adapters)",
    async () => {
      let _ = await Js.Promise.all(
        adapters->Js.Array2.map(adapterName =>
          Adapter.importAdapter(`../../adapters/${adapterName}.js`)
        ),
      )
    },
    ~iterations=20,
  )

  Js.Console.log("\n=== Concurrency Benchmarks ===")
  Js.Console.log(formatResult(sequentialResult))
  Js.Console.log(formatResult(parallelResult))
  Js.Console.log(
    `Speedup: ${Js.Float.toFixedWithPrecision(sequentialResult.avgMs /. parallelResult.avgMs, ~digits=2)}x`,
  )
})

// ============================================================================
// MEMORY BENCHMARKS
// ============================================================================

let _ = test("Benchmark: Memory usage per adapter", async () => {
  // Note: Deno doesn't expose detailed memory stats easily
  // This is a simplified approximation

  let beforeHeap = Deno.memoryUsage().heapUsed

  let adapters = []
  let entries = await Deno.readDirAsArray("./adapters")

  for entry in entries {
    if entry.isFile && Js.String2.endsWith(entry.name, ".js") {
      let adapter = await Adapter.importAdapter(`../../adapters/${entry.name}`)
      adapters->Js.Array2.push(adapter)->ignore
    }
  }

  let afterHeap = Deno.memoryUsage().heapUsed
  let totalMemory = afterHeap -. beforeHeap
  let perAdapter = totalMemory /. Js.Int.toFloat(adapters->Js.Array2.length)

  Js.Console.log("\n=== Memory Usage ===")
  Js.Console.log(`Total adapters loaded: ${adapters->Js.Array2.length->Js.Int.toString}`)
  Js.Console.log(`Total heap increase: ${Js.Float.toFixedWithPrecision(totalMemory /. 1024.0, ~digits=2)} KB`)
  Js.Console.log(`Per adapter: ${Js.Float.toFixedWithPrecision(perAdapter /. 1024.0, ~digits=2)} KB`)
})

// ============================================================================
// SUMMARY REPORT
// ============================================================================

let _ = test("Benchmark: Generate summary report", async () => {
  let entries = await Deno.readDirAsArray("./adapters")

  // Quick metrics
  let loadStart = Deno.Performance.now()
  for entry in entries {
    if entry.isFile && Js.String2.endsWith(entry.name, ".js") {
      let _ = await Adapter.importAdapter(`../../adapters/${entry.name}`)
    }
  }
  let loadEnd = Deno.Performance.now()

  let adapterCount =
    entries->Js.Array2.filter(e => e.isFile && Js.String2.endsWith(e.name, ".js"))->Js.Array2.length

  Js.Console.log("\n=== BENCHMARK SUMMARY ===")
  Js.Console.log(`Timestamp: ${Js.Date.toISOString(Js.Date.make())}`)
  Js.Console.log(`Platform: ${Deno.Build.build.os}`)
  Js.Console.log(`Deno Version: ${Deno.Version.version.deno}`)
  Js.Console.log(`V8 Version: ${Deno.Version.version.v8}`)
  Js.Console.log(`Adapter Count: ${adapterCount->Js.Int.toString}`)
  Js.Console.log(`Total Load Time: ${Js.Float.toFixedWithPrecision(loadEnd -. loadStart, ~digits=2)}ms`)
  Js.Console.log(
    `Avg Load Time Per Adapter: ${Js.Float.toFixedWithPrecision(
        (loadEnd -. loadStart) /. Js.Int.toFloat(adapterCount),
        ~digits=2,
      )}ms`,
  )
})
