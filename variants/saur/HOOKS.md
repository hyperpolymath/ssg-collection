# Git Hooks

This document describes the git hooks available for saur-ssg development.

## Quick Install

```bash
# Install all hooks at once
just hooks-install

# Or manually
cp hooks/pre-commit .git/hooks/
cp hooks/pre-push .git/hooks/
cp hooks/commit-msg .git/hooks/
chmod +x .git/hooks/pre-commit .git/hooks/pre-push .git/hooks/commit-msg
```

## Available Hooks

### pre-commit

Runs before each commit to ensure code quality.

**Checks:**
- Code formatting (`deno fmt --check`)
- Linting (`deno lint`)
- No hardcoded secrets
- SPDX headers present
- No dangerous patterns (`eval()`, `Function()`)

**To skip (use sparingly):**
```bash
git commit --no-verify -m "message"
```

### pre-push

Runs before pushing to remote.

**Checks:**
- Tests pass (`deno test`)
- All adapters have valid MCP interface
- Required SCM files present (META.scm, STATE.scm, ECOSYSTEM.scm)

**To skip (use sparingly):**
```bash
git push --no-verify
```

### commit-msg

Validates commit message format.

**Required format:** Conventional Commits
```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**
- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation
- `style` - Formatting (no code change)
- `refactor` - Code restructuring
- `test` - Adding/fixing tests
- `chore` - Maintenance
- `perf` - Performance
- `ci` - CI/CD changes
- `build` - Build system
- `security` - Security fixes

**Examples:**
```bash
git commit -m "feat(adapters): add pandoc adapter"
git commit -m "fix(zola): correct serve port handling"
git commit -m "docs: update cookbook with new recipes"
git commit -m "security: sanitize CLI arguments"
```

## Removing Hooks

```bash
just hooks-remove

# Or manually
rm .git/hooks/pre-commit .git/hooks/pre-push .git/hooks/commit-msg
```

## Troubleshooting

### Hook not running
```bash
# Check if executable
ls -la .git/hooks/

# Make executable
chmod +x .git/hooks/pre-commit
```

### Deno not found
Hooks require Deno for full functionality. Install:
```bash
curl -fsSL https://deno.land/install.sh | sh
```

### Want to run hooks manually
```bash
# Run pre-commit checks
./hooks/pre-commit

# Run pre-push checks
./hooks/pre-push

# Or use Justfile
just ci  # Runs format, lint, test, audit
```

## CI/CD Integration

These hooks run the same checks as CI, allowing you to catch issues locally before pushing:

| Hook | CI Job |
|------|--------|
| pre-commit | lint, security |
| pre-push | test, validate-adapters |
| commit-msg | (not in CI) |

---

*Last updated: 2025-12-17*
