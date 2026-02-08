# Philosophy

## Polyglot by Design

Each language in poly-ssg brings its own paradigm. These aren't obstacles to work around—they're features to embrace:

- **Forth's stack-thinking**: Data flows through transformations naturally. No variable names cluttering the mental model.
- **Ada's contracts**: Pre/post conditions could verify template correctness at compile time.
- **Haskell's purity**: Side effects isolated, transformations composable.
- **Chapel's parallelism**: Build thousands of pages in parallel without threading headaches.

## No Lowest Common Denominator

Engines aren't constrained to match each other feature-for-feature. Each implementation should:

1. **Embrace the language's strengths**: Fourth Estate uses stack manipulation naturally. Webforge could use SPARK proofs for template safety.

2. **Avoid fighting the language**: Don't force OOP into Forth or mutation into Haskell.

3. **Demonstrate the paradigm**: Each engine teaches something about its host language.

## MCP as Unifier

The Model Context Protocol provides:

- **Uniform interface**: All engines speak the same protocol
- **Tool discovery**: AI assistants can work with any engine
- **Implementation freedom**: Protocol uniformity, implementation diversity

## Educational Value

Seeing the same problem (static site generation) solved across paradigms:

- Shows trade-offs between approaches
- Teaches language idioms through real examples
- Demonstrates that there's no "one true way"

## The Stack Paradigm (Forth Estate)

Forth Estate embraces Forth's core insight: computation as stack transformation.

```forth
\ Traditional: result = process(parse(read(file)))
\ Forth: file read parse process -> result on stack

s" post.md" read-file parse-frontmatter render-markdown
```

The data flows naturally. Each word transforms the stack. No intermediate variables needed (though they're available when clarity demands).

## Why "Forth Estate"?

1. A play on "Forth" - the language name is right there
2. The Fourth Estate traditionally refers to the press/media
3. An SSG creates content for publishing—a modern form of the press
4. It's just a good name
