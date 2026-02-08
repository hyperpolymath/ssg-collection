# SQLite Database Demo - Ephapax Implementation

SQLite database operations demonstrating Ephapax's linear type system for safe resource management.

## Status

**Specification Only** - Waiting for Ephapax compiler completion.

## Features Demonstrated

### Linear Database Connections

```ephapax
type Connection = linear {
    handle: SQLiteHandle,
    path: String,
}

-- Connection must be closed exactly once
fn open@r(path: String) -> Result<Connection, String>
fn close(self) -> Result<(), String>  -- Consumes connection
```

### Linear Transactions

```ephapax
type Transaction = linear {
    conn: &mut Connection,
    committed: Bool,
}

-- Transaction must be committed OR rolled back (not both, not neither)
fn begin(conn: &mut Connection) -> Result<Transaction, String>
fn commit(mut self) -> Result<(), String>   -- Consumes transaction
fn rollback(mut self) -> Result<(), String> -- Consumes transaction
```

### Safety Guarantees

| Feature | Linear Types Benefit |
|---------|---------------------|
| Connections | Cannot be used after close |
| Transactions | Must be committed or rolled back |
| Query Results | Clear ownership of returned data |
| Resources | Auto-cleanup on scope exit |

## Components

### Migrations System
- Version-tracked schema migrations
- Transactional migration execution
- Idempotent migration checks

### Repositories
- **UserRepository** - CRUD operations, stats aggregation
- **PostRepository** - Posts with tags, view counting
- **TagRepository** - Tag management, popularity ranking

### Schema
```sql
users     - id, username, email, password_hash, full_name, is_active
posts     - id, user_id, title, content, status, view_count
tags      - id, name, slug
post_tags - post_id, tag_id (junction table)
comments  - id, post_id, user_id, content
```

## Build (Future)

```bash
ephapax build --target wasm32 database.epx
wasmtime database.wasm
```

## Linear Types Benefits

1. **No use-after-close** - Connection consumed on close
2. **No forgotten transactions** - Must explicitly commit/rollback
3. **No resource leaks** - Region-based deallocation
4. **Clear ownership** - Query results owned by caller

## License

SPDX-License-Identifier: MPL-2.0-or-later
