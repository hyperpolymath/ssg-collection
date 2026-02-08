# Engine — Ada/SPARK Core

This directory contains the Ada/SPARK core engine for eclipse-ssg.

## Overview

The engine provides:

- **Formal verification** via SPARK 2014
- **Memory safety** guarantees
- **Deterministic builds** with proof obligations
- **Bernoulli verification** integration

## Files

| File | Purpose |
|------|---------|
| `engine.ads` | Package specification |
| `engine.adb` | Package body |
| `types.ads` | Type definitions |
| `templates.ads` | Operation-card templating |

## Building

```bash
# With Alire
alr build

# With GPRbuild
gprbuild -P engine.gpr
```

## Verification

```bash
# SPARK proof
gnatprove -P engine.gpr --level=2
```
