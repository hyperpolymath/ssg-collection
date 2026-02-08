# parallax-ssg Roadmap

> The DEFINITIVE Chapel static site generator

**Current Version:** 2.0.0
**Last Updated:** 2025-12-22
**Status:** 44/44 Components Complete

---

## Final Project Status

### 44/44 Components Complete

| Category | Count | Status |
|----------|-------|--------|
| Core Engine | 4/4 | Complete |
| Build System | 4/4 | Complete |
| Site Generation | 4/4 | Complete |
| Adapters | 3/3 | Complete |
| Testing | 4/4 | Complete |
| Documentation | 8/8 | Complete |
| Configuration | 3/3 | Complete |
| SCM Metadata | 6/6 | Complete |
| CI/CD | 4/4 | Complete |
| Security | 4/4 | Complete |
| **TOTAL** | **44/44** | **100%** |

---

## Component Breakdown

### 1. Core Engine (4/4)

| Component | Location |
|-----------|----------|
| Chapel Engine | `src/parallel-press.chpl` |
| Markdown Synthesis | Frontmatter + body parsing |
| Template Engine | `{{variable}}` substitution |
| Variable Store | Config constants + frontmatter |

### 2. Build System (4/4)

| Component | Location |
|-----------|----------|
| Justfile | `justfile` - build, test, test-e2e, lsp, compile |
| Mustfile | `Mustfile` - required validations |
| Containerfile | `Containerfile` - Podman/Docker |
| .tool-versions | `.tool-versions` - asdf version management |

### 3. Site Generation (4/4)

| Component | Location |
|-----------|----------|
| Content Processing | YAML frontmatter + Markdown |
| Template Engine | `{{variable}}` substitution |
| Output Generation | HTML files to `_site/` |
| Content Schema | Post record type |

### 4. Adapters (3/3)

| Component | Location |
|-----------|----------|
| MCP Server | poly-ssg-mcp hub connection |
| ReScript Adapter | `adapters/src/ParallaxAdapter.res` |
| Hub Interface | parallax_build, parallax_compile, parallax_version |

### 5. Testing (4/4)

| Component | Location |
|-----------|----------|
| Unit Tests | `test-full` command |
| Markdown Tests | `test-markdown` command |
| Frontmatter Tests | `test-frontmatter` command |
| E2E Tests | `.github/workflows/ci.yml` |

### 6. Documentation (8/8)

| Component | Location |
|-----------|----------|
| README | `README.adoc` |
| ROADMAP | `ROADMAP.md` |
| SECURITY | `SECURITY.md` |
| CODE_OF_CONDUCT | `CODE_OF_CONDUCT.md` |
| CONTRIBUTING | `CONTRIBUTING.md` |
| Cookbook | `cookbook.adoc` |
| Adapter README | `adapters/README.md` |
| Copilot Instructions | `copilot-instructions.md` |

### 7. Configuration (3/3)

| Component | Location |
|-----------|----------|
| Site Config | Config vars in Chapel |
| Example Config | Default settings |
| Environment | CLI arguments |

### 8. SCM Metadata (6/6)

| Component | Location |
|-----------|----------|
| META.scm | Architecture decisions |
| STATE.scm | Project state |
| ECOSYSTEM.scm | Ecosystem position |
| PLAYBOOK.scm | Operations |
| AGENTIC.scm | AI agent config |
| NEUROSYM.scm | Symbolic patterns |

### 9. CI/CD (4/4)

| Component | Location |
|-----------|----------|
| CI Workflow | `.github/workflows/ci.yml` |
| CodeQL | `.github/workflows/codeql.yml` |
| Dependabot | `.github/dependabot.yml` |
| Language Enforcement | Blocks forbidden languages |

### 10. Security (4/4)

| Component | Location |
|-----------|----------|
| SECURITY.md | Security policy |
| security.txt | `.well-known/security.txt` |
| AIBDP | `.well-known/aibdp.json` |
| Consent Framework | `.well-known/ai.txt` |

---

## Available Commands

### Just Commands

```bash
just build          # Build the SSG engine
just build-debug    # Build with debug symbols
just build-release  # Build optimized release
just clean          # Clean build artifacts

just test           # Run all tests
just test-unit      # Run unit tests
just test-markdown  # Run markdown parser tests
just test-e2e       # Run end-to-end tests
just test-all       # Run all tests including E2E

just dev            # Start development mode
just generate       # Generate site from content
just serve          # Serve generated site locally

just adapter-build  # Build ReScript MCP adapter
just adapter-clean  # Clean adapter build
just adapter-watch  # Watch adapter for changes

just lsp            # Start language server
just compile FILE   # Compile a Chapel file
just check          # Check syntax without building
just fmt            # Format Chapel code

just ci             # Run CI pipeline locally
just security       # Run security checks
just lint           # Lint Chapel code

just docs           # Generate documentation
just docs-serve     # Serve documentation locally

just container-build  # Build container image
just container-run    # Run in container

just release VERSION  # Create release build
just tag VERSION      # Tag release in git
```

---

## Completed Milestones

### v1.0.0 - Chapel Implementation (Dec 2025)
- [x] Core Chapel SSG engine (`src/parallel-press.chpl`)
- [x] Frontmatter parsing (YAML-like key:value)
- [x] Markdown to HTML conversion
- [x] Template engine with placeholder support
- [x] Parallel build system using Chapel's `forall` loops
- [x] Atomic counters for thread-safe operations
- [x] Test functions (markdown, frontmatter, full)
- [x] RSR-compliant repository structure
- [x] Security policy and security.txt
- [x] CI/CD with language enforcement

### v1.0.1 - MCP Adapter (Dec 2025)
- [x] ReScript MCP adapter implementation
- [x] Tool definitions (build, compile, version)
- [x] Connection state management
- [x] Command execution interface
- [x] Fixed filename references in CI and adapter

### v2.0.0 - 44-Component Structure (Dec 2025)
- [x] Complete Justfile with all commands
- [x] Mustfile for required validations
- [x] Containerfile for Podman/Docker
- [x] Full SCM metadata (META, STATE, ECOSYSTEM, PLAYBOOK, AGENTIC, NEUROSYM)
- [x] Comprehensive cookbook.adoc documentation
- [x] Enhanced CI/CD with caching and full test suite
- [x] Security hardening and compliance checks

---

## Future Enhancements (Post-2.0)

While 44/44 components are complete, future enhancements could include:

- Watch mode for development
- Incremental builds
- Plugin architecture (Chapel-only)
- Extended Markdown support (tables, footnotes)
- Sitemap generation
- RSS/Atom feed generation
- i18n/l10n support
- Performance benchmarking suite

---

## Language Policy

**This project is written entirely in Chapel. NO EXCEPTIONS.**

| Allowed | Forbidden |
|---------|-----------|
| Chapel (SSG engine) | Python |
| ReScript (MCP adapter only) | JavaScript/TypeScript |
| | Ruby, Go, Java, Rust |
| | C, C++ |
| | Any other language |

The MCP adapter in `adapters/` is the **only** place where non-Chapel code is permitted.

---

## Related Projects

- [poly-ssg-mcp](https://github.com/hyperpolymath/poly-ssg-mcp) - Unified MCP hub for 28+ SSGs
- [rhodium-standard-repositories](https://github.com/hyperpolymath/rhodium-standard-repositories) - Repository standards

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for contribution guidelines.
See [cookbook.adoc](cookbook.adoc) for complete command reference.
For security issues, see [SECURITY.md](SECURITY.md).
