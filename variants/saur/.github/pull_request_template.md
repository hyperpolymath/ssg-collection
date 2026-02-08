## Summary

<!-- Brief description of changes (1-3 bullet points) -->

-

## Type of Change

<!-- Mark the appropriate option with [x] -->

- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update
- [ ] Refactoring (no functional changes)
- [ ] Security fix

## Affected Components

<!-- Which categories are affected? See STATE.scm for full list -->

- [ ] Adapters (adapters/*.js)
- [ ] Build System (Justfile, Mustfile)
- [ ] Configuration (*.scm, *.ncl)
- [ ] Testing (tests/)
- [ ] Documentation (*.md, *.adoc)
- [ ] CI/CD (.github/workflows/)
- [ ] Security

## Checklist

### Code Quality

- [ ] Code follows project style guidelines (copilot-instructions.md)
- [ ] Code passes `deno fmt --check`
- [ ] Code passes `deno lint`
- [ ] SPDX headers present on new files

### Testing

- [ ] Tests added/updated for changes
- [ ] All tests pass (`just test`)
- [ ] E2E tests pass (`just test-e2e`)

### Security

- [ ] No hardcoded secrets or credentials
- [ ] No `eval()` or `Function()` usage
- [ ] Input validation for any user-provided data

### Documentation

- [ ] README updated (if applicable)
- [ ] API documentation updated (if applicable)
- [ ] CHANGELOG entry added (for user-facing changes)

### MCP Interface (for adapter changes)

- [ ] Exports: `name`, `language`, `tools`, `connect`, `disconnect`, `isConnected`
- [ ] Tool names follow pattern: `{ssg}_{operation}`
- [ ] Input schemas are valid JSON Schema

## Test Plan

<!-- How can reviewers verify this change works? -->

```bash
# Commands to test this PR
just test
just adapter-validate <name>  # if adapter changed
```

## Related Issues

<!-- Link any related issues: Fixes #123, Relates to #456 -->

## Additional Notes

<!-- Any additional context, screenshots, or information -->
