# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |
| < 0.1.0 | :x:                |

## Reporting a Vulnerability

We take security seriously. If you discover a security vulnerability in hackenbush-ssg, please report it responsibly.

### How to Report

1. **Email**: Send details to security@hyperpolymath.org
2. **Encryption**: Use our GPG key at https://hyperpolymath.org/gpg/security.asc
3. **GitHub**: For non-critical issues, open a security advisory at https://github.com/hyperpolymath/hackenbush-ssg/security/advisories

### What to Include

- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if any)

### Response Timeline

- **Initial Response**: Within 48 hours
- **Status Update**: Within 7 days
- **Resolution Target**: Within 30 days for critical issues

### What to Expect

- We will acknowledge receipt of your report
- We will investigate and keep you informed of progress
- We will credit you in the security advisory (unless you prefer anonymity)
- We follow coordinated disclosure practices

## Security Considerations

### Game of Life Patterns

The core SSG logic is encoded in Game of Life patterns (RLE files). These are data files that describe cell configurations and pose minimal security risk.

### Host Runtime

The Deno host runtime (`runtime/host.ts`) runs with limited permissions:
- `--allow-read`: Required to read pattern files and content
- `--allow-write`: Required to write generated HTML

### ReScript Adapter

The MCP adapter is written in ReScript, which compiles to safe JavaScript without runtime reflection or eval.

## Scope

This security policy covers:
- The hackenbush-ssg repository
- Official releases and packages
- The host runtime and adapter code

Out of scope:
- Third-party forks or modifications
- User-generated Life patterns (run at your own risk)
