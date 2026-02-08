# poly-ssg

A polyglot static site generator framework with MCP integration. Each engine implements the same SSG contract in a different programming language, bringing unique paradigmatic strengths.

## Philosophy

Polyglot by design: Each language brings its paradigm. Forth's stack-thinking, Ada's contracts, Haskell's purity—these aren't obstacles, they're features.

No lowest common denominator: Engines aren't constrained to match each other. Forth Estate embraces stack manipulation. Webforge could use SPARK proofs.

MCP as unifier: The protocol layer provides uniformity; the implementations provide diversity.

Educational value: Seeing the same problem solved across paradigms teaches more than any single implementation.

## Future Engines Roadmap

| Language | Name          | Status        | Notes                          |
|----------|---------------|---------------|--------------------------------|
| Forth    | Forth Estate  | In Progress   | Stack-based paradigm           |
| Ada      | Webforge      | Planned       | SPARK subset for verified templates |
| Chapel   | ParallelPress | Planned       | Data-parallel builds           |
| Haskell  | HakyllLite    | Planned       | Simplified Hakyll              |
| Racket   | PollenLite    | Planned       | Programmable publishing        |
| Gleam    | GleamSite     | Planned       | BEAM + types                   |
| OCaml    | YoCamlLite    | Planned       | Applicative pipelines          |
| Zig      | ZigPress      | Considered    | Comptime template validation   |
| Idris    | ProvenPages   | Considered    | Dependent types for templates  |

## Engine Interface Contract

Each engine must:

1. Accept standard invocation:
   ```
   cd engines/<engine-name>
   <build-command> <source-path> <output-path>
   ```

2. Support common frontmatter:
   ```yaml
   ---
   title: string (required)
   date: ISO-8601 date (optional)
   template: string (optional, default: "default")
   tags: list of strings (optional)
   draft: boolean (optional, default: false)
   ---
   ```

3. Produce valid HTML5 output

4. Exit with code 0 on success, non-zero on failure

5. Write to stdout for progress, stderr for errors

## Directory Structure

```
poly-ssg/
├── engines/
│   └── forth-estate/     # Gforth-based SSG
├── docs/
│   ├── PHILOSOPHY.md
│   └── ADDING-ENGINE.md
├── test-corpus/
│   ├── posts/            # Sample markdown posts
│   └── templates/        # Shared templates
└── scripts/
    └── test-all.sh       # Run tests across engines
```

## Quick Start

```bash
# Test Forth Estate (requires Gforth >= 0.7)
cd engines/forth-estate
gforth forth-estate.fs -e "s\" ../../test-corpus\" s\" output\" build bye"
```

## Adding a New Engine

See [docs/ADDING-ENGINE.md](docs/ADDING-ENGINE.md) for the full guide.

## License

MIT
