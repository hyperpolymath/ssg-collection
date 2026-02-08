# Git Hooks for rescribe-ssg

This directory contains git hook templates for rescribe-ssg development.

## Installation

```bash
# Install pre-commit hook
cp .hooks/pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit

# Install pre-push hook
cp .hooks/pre-push .git/hooks/pre-push
chmod +x .git/hooks/pre-push

# Or install all hooks at once
for hook in .hooks/*; do
  [ -f "$hook" ] && [ "$(basename $hook)" != "README.md" ] && \
    cp "$hook" .git/hooks/ && chmod +x ".git/hooks/$(basename $hook)"
done
```

## Available Hooks

### pre-commit

Runs before each commit:
- Language enforcement (no Python/TypeScript/JavaScript in src/)
- ReScript build verification
- Optional format check

### pre-push

Runs before pushing:
- Full language enforcement
- Complete build
- SCM metadata verification
- Security audit

## Customization

Edit the hooks in `.hooks/` directory, then reinstall to `.git/hooks/`.

## Bypassing Hooks

In emergencies, you can bypass hooks:

```bash
# Bypass pre-commit
git commit --no-verify -m "message"

# Bypass pre-push
git push --no-verify
```

**Warning**: Bypassing hooks may allow forbidden code to be pushed.
