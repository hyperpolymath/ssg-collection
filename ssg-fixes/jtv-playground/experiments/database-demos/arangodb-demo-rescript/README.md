# ArangoDB Multi-Model Demo - ReScript

A comprehensive demonstration of ArangoDB's multi-model capabilities using ReScript and Deno.

## Features

- Document model operations (like MongoDB)
- Graph model (relationships and traversals)
- Key-value model
- AQL (ArangoDB Query Language)
- Complex joins and aggregations
- Graph traversal patterns

## Queries Demonstrated

### Basic Queries
- Get all users
- Users with follower count
- Popular posts by likes

### Graph Queries
- Posts with their tags
- Followers of followers (2-hop traversal)
- Shortest path between users

### Advanced Queries
- Complex aggregation (user engagement stats)
- Full-text search
- Similar posts recommendation

## Data Model

### Document Collections
- `users` - User profiles
- `posts` - Blog posts
- `comments` - Post comments
- `tags` - Content tags

### Edge Collections
- `follows` - User-to-user relationships
- `likes` - User-to-post relationships
- `tagged_with` - Post-to-tag relationships

### Graph
- `social` - Combined graph of all relationships

## Running

```bash
# Start ArangoDB (Docker)
docker run -p 8529:8529 -e ARANGO_ROOT_PASSWORD=rootpassword arangodb/arangodb

# Build ReScript
deno task build

# Run demo
deno task start
```

## Project Structure

```
arangodb-demo-rescript/
├── deno.json           # Deno configuration
├── rescript.json       # ReScript configuration
└── src/
    ├── bindings/
    │   └── ArangoDB.res  # ArangoDB bindings
    └── ArangoDemo.res    # Demo queries
```

## License

SPDX-License-Identifier: MPL-2.0-or-later
