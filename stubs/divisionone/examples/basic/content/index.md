---
title: Division One Example Site
date: 2025-01-18
---

# Welcome to Division One

This example site demonstrates the COBOL-powered static site generator.

## Features

- IDENTIFICATION DIVISION for metadata
- DATA DIVISION for structured content
- PROCEDURE DIVISION for build logic
- LINE SEQUENTIAL file processing

## COBOL Origins

COBOL (Common Business-Oriented Language) was developed in 1959 and remains in use for critical business systems.

### Why COBOL for SSG?

- Self-documenting syntax
- Record-oriented processing fits content models
- Division architecture maps to SSG phases
- Enterprise reliability

## Code Example

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
           DISPLAY "Hello from Division One!".
           STOP RUN.
```

## Build Instructions

```bash
# Compile the COBOL source
cobc -x -o divisionone src/divisionone.cob

# Initialize a new site
./divisionone init

# Build the site
./divisionone build
```

## More Content

- Division architecture ensures modularity
- GnuCOBOL provides modern compilation
- 80-character-wide code for traditional feel
