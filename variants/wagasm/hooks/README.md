# wagasm-ssg Git Hooks

Git hooks to enforce project standards.

## Installation

```bash
./hooks/install.sh
```

Or manually:

```bash
cp hooks/pre-commit .git/hooks/pre-commit
cp hooks/pre-push .git/hooks/pre-push
cp hooks/commit-msg .git/hooks/commit-msg
chmod +x .git/hooks/*
```

## Hooks

### pre-commit

Runs before each commit:

- **Language compliance** (FATAL): No TS/JS/RS/PY in src/
- **WAT compilation**: Syntax check
- **Host runtime**: Warning if SSG logic detected
- **SPDX headers**: Warning if missing

### pre-push

Runs before push:

- Full WAT compilation
- WASM validation
- Site generation test
- Output verification

### commit-msg

Enforces conventional commit format:

```
<type>(<scope>): <description>

Types: feat, fix, docs, style, refactor, test, chore, security, ci, build, perf
```

## Bypassing Hooks

Use `--no-verify` flag (not recommended):

```bash
git commit --no-verify -m "emergency fix"
git push --no-verify
```

## Uninstalling

```bash
rm .git/hooks/pre-commit .git/hooks/pre-push .git/hooks/commit-msg
```
