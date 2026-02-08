# Contributing to JTV Playground

Thank you for your interest in contributing! This project uses the **Tri-Perimeter Contribution Framework (TPCF)** to create a graduated trust model that balances openness with security.

## 🎯 Quick Start (Perimeter 3 Contributors)

1. **Fork the repository**
2. **Create a feature branch**: `git checkout -b feature/amazing-feature`
3. **Make your changes** (see guidelines below)
4. **Run tests**: `just test` (or `cargo test`, `julia test`, etc.)
5. **Commit**: `git commit -m "feat: add amazing feature"`
6. **Push**: `git push origin feature/amazing-feature`
7. **Open a Pull Request**

That's it! You're in **Perimeter 3** (Community Sandbox).

## 🔐 Tri-Perimeter Contribution Framework (TPCF)

This project uses a graduated trust model with three security perimeters:

### Perimeter 3: Community Sandbox (You Are Here!)

**Access Level**: Open contribution, public forks

**What You Can Do**:
- ✅ Fork the repository
- ✅ Create pull requests
- ✅ Participate in discussions
- ✅ Report issues
- ✅ Suggest improvements

**Restrictions**:
- ❌ No direct push to `main` branch
- ❌ PRs require review from Perimeter 1/2
- ❌ No access to secrets or deployment keys
- ❌ Limited CI/CD permissions

**How to Graduate**: Submit 3-5 quality PRs, engage constructively, demonstrate understanding of project goals.

### Perimeter 2: Regular Contributors (Earned Trust)

**Access Level**: Triage access, limited write permissions

**What You Get**:
- ✅ Merge small PRs without full review
- ✅ Triage and label issues
- ✅ Approve Perimeter 3 PRs (with Perimeter 1 oversight)
- ✅ Edit wiki and documentation
- ✅ Participate in project decisions

**Restrictions**:
- ❌ No deployment access
- ❌ Major changes require Perimeter 1 review
- ❌ No security-critical code changes alone

**How to Graduate**: Consistent high-quality contributions (20+ merged PRs), deep project knowledge, trusted by community.

### Perimeter 1: Core Maintainers (Full Trust)

**Access Level**: Full repository access, deployment rights

**Responsibilities**:
- ✅ Approve all security-critical changes
- ✅ Deploy releases
- ✅ Manage secrets and keys
- ✅ Set project direction
- ✅ Mentor Perimeter 2/3 contributors
- ✅ Handle security incidents

**Current Members**: See [MAINTAINERS.md](MAINTAINERS.md)

## 📋 Contribution Guidelines

### Code Style

**Rust**:
```bash
cargo fmt --all  # Format code
cargo clippy --all-targets --all-features  # Lint
```

**Julia**:
```julia
using JuliaFormatter
format(".")  # Format all Julia files
```

**JavaScript/ReScript**:
```bash
deno fmt  # Format with Deno
npm run lint  # Or use project-specific linter
```

### Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `style`: Formatting (no code change)
- `refactor`: Code restructuring
- `perf`: Performance improvement
- `test`: Adding/updating tests
- `chore`: Build process, dependencies

**Examples**:
```
feat(arangodb): add graph traversal queries
fix(julia): resolve memory leak in pipeline
docs(rsr): update compliance checklist
test(algorithms): add sorting benchmark tests
```

### Testing Requirements

**All code must have tests**:

```bash
# Run all tests
just test

# Or language-specific:
cargo test          # Rust
julia test/runtests.jl  # Julia
deno test           # Deno/ReScript
npm test            # Node.js
```

**Target**: 100% code coverage for RSR Bronze-level compliance.

### Documentation

**Update documentation** when you:
- Add a new feature
- Change behavior
- Fix a bug
- Add a new dependency
- Modify APIs

**Required docs**:
- Inline code comments for complex logic
- Function/method docstrings
- README.md updates
- CHANGELOG.md entry

### Pull Request Process

1. **Create descriptive PR title**: Use conventional commit format
2. **Fill out PR template**: Explain what, why, how
3. **Link related issues**: Use "Fixes #123" or "Relates to #456"
4. **Request review**: Tag relevant maintainers
5. **Respond to feedback**: Address review comments promptly
6. **Squash commits** (if needed): Keep history clean
7. **Wait for CI**: All checks must pass

### Code Review Checklist

Reviewers will check for:

- ✅ **Correctness**: Does it work as intended?
- ✅ **Tests**: Are there tests? Do they pass?
- ✅ **Performance**: Any performance regressions?
- ✅ **Security**: Any security implications?
- ✅ **Documentation**: Is it documented?
- ✅ **Style**: Does it follow project conventions?
- ✅ **RSR Compliance**: Does it maintain compliance?
- ✅ **Offline-First**: Does it avoid unnecessary network calls?

## 🎨 What to Work On

### Good First Issues

Look for issues labeled:
- `good first issue`: Perfect for newcomers
- `help wanted`: We'd love your help
- `documentation`: Improve docs
- `bug`: Fix something broken

### Current Priorities

See [ROADMAP.md](docs/ROADMAP.md) (if exists) or GitHub Project boards.

### Suggesting New Features

**Before creating an issue**:
1. Check if it already exists
2. Search closed issues (might have been rejected)
3. Discuss in GitHub Discussions first (for major features)

**When creating an issue**:
- Use issue templates
- Provide clear use case
- Explain why it's needed
- Consider implementation approach

## 🛠️ Development Setup

### Prerequisites

- **Rust**: 1.70+ (`rustup`, `cargo`)
- **Julia**: 1.9+ (juliaup recommended)
- **Deno**: 1.37+ (deno.land)
- **Node.js**: 18+ (for some experiments)
- **just**: Build automation (`cargo install just`)
- **Nix** (optional): Reproducible builds

### Clone and Build

```bash
# Clone repository
git clone https://github.com/Hyperpolymath/jtv-playground.git
cd jtv-playground

# Build all projects
just build

# Or build specific components
cd experiments/algorithms
cargo build --release
```

### Run Tests

```bash
# All tests
just test

# Specific language
just test-rust
just test-julia
just test-deno
```

### Local Development

```bash
# Watch mode (auto-rebuild on changes)
just watch

# Format all code
just fmt

# Lint all code
just lint

# Check RSR compliance
just rsr-check
```

## 🌍 Community Guidelines

### Code of Conduct

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).

**In short**:
- Be respectful and inclusive
- Welcome newcomers
- Provide constructive feedback
- Focus on what's best for the project
- Show empathy towards others

### Communication Channels

- **GitHub Issues**: Bug reports, feature requests
- **GitHub Discussions**: General questions, ideas
- **Pull Requests**: Code contributions
- **Email**: Security issues only (see SECURITY.md)

### Emotional Safety

This project prioritizes **emotional safety** per Palimpsest License:

- **Reversibility**: Mistakes can be undone
- **Experimentation**: Try things without fear
- **Learning**: Questions are encouraged
- **Respect**: Everyone's contributions matter

**If you make a mistake**: It's okay! We all do. Just fix it and learn.

## 📜 Legal

By contributing, you agree that:

1. **License**: Your contributions are dual-licensed under MIT + Palimpsest v0.8
2. **Ownership**: You own your contributions
3. **DCO**: You certify the [Developer Certificate of Origin](https://developercertificate.org/)
4. **No CLA**: We don't require a Contributor License Agreement

### Developer Certificate of Origin (DCO)

```
Developer Certificate of Origin
Version 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

**Sign your commits** with `-s`:

```bash
git commit -s -m "feat: add new feature"
```

## 🎁 Recognition

Contributors are recognized in:

- **CHANGELOG.md**: Your contributions are documented
- **humans.txt**: Listed in attribution
- **GitHub**: Contributor graph and stats
- **Hall of Fame**: Top contributors (future)

## 🚀 Release Process

(For Perimeter 1 maintainers)

1. Update version numbers
2. Update CHANGELOG.md
3. Create git tag: `v1.0.0`
4. Build release artifacts
5. Generate release notes
6. Publish to package registries
7. Announce release

## 📚 Additional Resources

- [RSR Framework](docs/rsr-compliance.md): Rhodium Standard Repository
- [TPCF Details](docs/tpcf.md): Tri-Perimeter Contribution Framework
- [Architecture](docs/ARCHITECTURE.md): System design
- [API Documentation](docs/API.md): API reference

## ❓ Questions?

- **General questions**: GitHub Discussions
- **Bug reports**: GitHub Issues
- **Security concerns**: SECURITY.md
- **Everything else**: Check existing documentation first

---

**Thank you for contributing!** 🎉

Every contribution, no matter how small, makes this project better.

_Last updated: 2025-11-22_
