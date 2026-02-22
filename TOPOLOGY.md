<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# SSG Collection — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              STATIC CONTENT             │
                        │        (Markdown, AsciiDoc, Org)        │
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           SSG COLLECTION HUB            │
                        │                                         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ Implement-│  │  Variants         │  │
                        │  │ ations    │  │  (Experiments)    │  │
                        │  │ (Rats, QED)│ │  (Baremetal, etc) │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        │        │                 │              │
                        │  ┌─────▼─────┐  ┌────────▼──────────┐  │
                        │  │ Stubs     │  │  SSG Fixes        │  │
                        │  │ (Scaffolds)│ │  (Patch Layer)    │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │           DEPLOYMENT TARGETS            │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ GitHub Pgs│  │ IPFS      │  │ Local ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │          EXTERNAL FRAMEWORKS            │
                        │      (poly-ssg, casket-ssg)             │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile Automation  .machine_readable/  │
                        │  Consolidation Log    0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE COLLECTIONS
  Implementations (7 major)         ██████████ 100%    Rats/Eclipse/Wokelang stable
  Variants (13 minor)               ██████████ 100%    Experimental logic verified
  Stubs (32 templates)              ██████████ 100%    RSR skeletons active
  ssg-fixes (Absorbed)              ██████████ 100%    Legacy patches verified

REPO INFRASTRUCTURE
  Justfile Automation               ██████████ 100%    Standard build tasks
  .machine_readable/                ██████████ 100%    STATE tracking active
  Consolidation (2026-02-08)        ██████████ 100%    52 repos successfully merged

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██████████ 100%    Consolidated reference library
```

## Key Dependencies

```
Source Format ────► SSG Implementation ──► HTML Output ──────► Deploy
     │                    │                   │                 │
     ▼                    ▼                   ▼                 ▼
 SSG Variant ─────► Patch / Fix Layer ───► Static Assets ────► IPFS
```

## Update Protocol

This file is maintained primarily by AI agents for archival indexing. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
