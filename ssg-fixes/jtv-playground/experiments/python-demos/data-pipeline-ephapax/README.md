# Data Processing Pipeline - Ephapax Implementation

A comprehensive ETL data pipeline demonstrating Ephapax's linear type system for safe data processing.

## Status

**Specification Only** - Waiting for Ephapax compiler completion.

## Features Demonstrated

### Linear Ownership
- Data flows through pipeline stages with ownership transfer
- `.take()` consumes previous stage data
- `.as_ref()` borrows for read-only access

### Region-Based Memory
- Large collections allocated in regions
- Bulk deallocation at region exit
- `@r` annotations track allocations

### Pattern Matching
- Age group classification
- Salary band assignment
- Department aggregation

## Pipeline Stages

1. **Load** - Read CSV or generate sample data
2. **Clean** - Handle duplicates, missing values, outliers
3. **Transform** - Feature engineering (age groups, salary bands, etc.)
4. **Analyze** - Compute statistics, correlations, insights
5. **Export** - Output CSV and JSON results

## Build (Future)

```bash
# When Ephapax compiler is ready:
ephapax build --target wasm32 pipeline.epx

# Run:
wasmtime pipeline.wasm
```

## License

SPDX-License-Identifier: MPL-2.0-or-later
