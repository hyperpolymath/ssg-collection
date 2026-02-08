# MCP Adapter for hackenbush-ssg

This directory contains the **ReScript MCP adapter** that connects hackenbush-ssg to the poly-ssg-mcp hub.

## Structure

```
adapters/
├── src/
│   └── HackenbushAdapter.res  # ReScript adapter source
├── package.json               # Node dependencies
├── rescript.json              # ReScript configuration
└── README.md
```

## Building

```bash
npm install
npm run build
```

## Why ReScript?

The MCP adapter is the **ONLY** place non-native language code is allowed in this satellite.
ReScript was chosen because:

1. Type-safe JavaScript interop
2. Compiles to clean ES6 modules
3. Part of the RSR Tier 1 language set
4. Consistent across all poly-ssg satellites

## Connection to Hub

This adapter exports:
- `name` - Satellite identifier
- `language` - Primary implementation language
- `description` - Human-readable description
- `connect()` - Establish connection
- `disconnect()` - Close connection
- `isConnected()` - Check status
- `tools` - Array of MCP tools

## Language Enforcement

**IMPORTANT**: The adapter is for hub communication only.
ALL SSG logic must remain in the satellite's native language.
The adapter may ONLY:
- Execute commands in the native runtime
- Parse command output
- Report results to the hub

The adapter may NOT:
- Contain SSG logic
- Generate HTML/content
- Parse markdown
- Make routing decisions

---
*Part of the poly-ssg satellite network*
