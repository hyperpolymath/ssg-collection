# Contributing to iota-ssg

Thank you for your interest in contributing to iota-ssg! This document provides guidelines and information for contributors.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
- [Development Workflow](#development-workflow)
- [Style Guidelines](#style-guidelines)
- [Security](#security)

---

## Code of Conduct

This project follows our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

---

## Getting Started

### Prerequisites

- Node.js (for adapter development)
- Git with signed commits enabled
- Familiarity with the SSG you want to work with

### Setup

```bash
# Clone the repository
git clone https://github.com/hyperpolymath/iota-ssg.git
cd iota-ssg

# Verify setup
ls adapters/  # View available adapters
```

### Repository Structure

```
iota-ssg/
├── adapters/              # 28 SSG adapter implementations
│   ├── babashka.js       # Clojure SSG
│   ├── cobalt.js         # Rust SSG
│   ├── hakyll.js         # Haskell SSG
│   ├── zola.js           # Rust SSG
│   └── ...               # More adapters
├── .github/
│   ├── ISSUE_TEMPLATE/   # Issue templates
│   └── workflows/        # CI/CD workflows
├── CODE_OF_CONDUCT.md
├── CONTRIBUTING.md       # This file
├── LICENSE.txt
├── README.adoc
├── SECURITY.md
├── META.scm              # Architecture decisions
├── ECOSYSTEM.scm         # Ecosystem positioning
└── STATE.scm             # Project state tracking
```

---

## How to Contribute

### Reporting Bugs

**Before reporting**:
1. Search existing issues
2. Check if it's already fixed in `main`
3. Identify which adapter is affected

**When reporting**:

Use the [bug report template](.github/ISSUE_TEMPLATE/bug_report.md) and include:

- Clear, descriptive title
- Environment details (OS, Node version, SSG version)
- Steps to reproduce
- Expected vs actual behaviour
- Logs or error messages

### Suggesting Features

**Before suggesting**:
1. Search existing issues and discussions
2. Consider if it fits the project's scope

**When suggesting**:

Use the [feature request template](.github/ISSUE_TEMPLATE/feature_request.md) and include:

- Problem statement (what pain point does this solve?)
- Proposed solution
- Alternatives considered
- Which adapter(s) this affects

### Your First Contribution

Look for issues labelled:

- [`good first issue`](https://github.com/hyperpolymath/iota-ssg/labels/good%20first%20issue) — Simple tasks
- [`help wanted`](https://github.com/hyperpolymath/iota-ssg/labels/help%20wanted) — Community help needed
- [`documentation`](https://github.com/hyperpolymath/iota-ssg/labels/documentation) — Docs improvements

---

## Development Workflow

### Branch Naming

```
docs/short-description       # Documentation
feat/short-description       # New features/adapters
fix/issue-number-description # Bug fixes
refactor/what-changed        # Code improvements
security/what-fixed          # Security fixes
```

### Commit Messages

We follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types**: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`, `security`

**Examples**:
```
feat(adapters): add support for new SSG framework
fix(zola): correct build path detection
docs: update installation instructions
security(deps): update vulnerable dependency
```

### Pull Request Process

1. **Fork** the repository
2. **Create** a feature branch from `main`
3. **Make** your changes with clear commits
4. **Test** your changes locally
5. **Push** to your fork
6. **Open** a pull request

**PR Requirements**:
- Clear description of changes
- Link to related issues
- All CI checks passing
- Signed commits

---

## Style Guidelines

### JavaScript/Adapters

- Use consistent formatting (Prettier)
- Include SPDX license headers
- Document public functions
- Keep adapters focused and minimal

### Documentation

- Use clear, concise language
- Include code examples where helpful
- Keep README sections up to date

---

## Security

- **Never** commit secrets, credentials, or API keys
- Report security issues via [GitHub Security Advisories](https://github.com/hyperpolymath/iota-ssg/security/advisories/new)
- See [SECURITY.md](SECURITY.md) for our security policy

---

## Questions?

- Open a [Discussion](https://github.com/hyperpolymath/iota-ssg/discussions) for questions
- Check existing issues before opening new ones

---

## Recognition

Contributors are recognized in:
- Release notes
- GitHub contributors list
- Security acknowledgments (for security reports)

---

*Thank you for helping improve iota-ssg!*
