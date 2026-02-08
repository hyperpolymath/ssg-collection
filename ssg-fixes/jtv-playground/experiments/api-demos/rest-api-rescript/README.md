# REST API - ReScript + Deno

A RESTful API implementation using ReScript compiled to JavaScript and run with Deno.

## Features

- CRUD operations for Users and Posts
- Type-safe routing with pattern matching
- JSON response helpers
- In-memory data store
- Health check endpoint

## Endpoints

### Users
- `GET /api/users` - List all users
- `GET /api/users/:id` - Get user by ID
- `POST /api/users` - Create new user
- `PUT /api/users/:id` - Update user
- `DELETE /api/users/:id` - Delete user
- `GET /api/users/:id/posts` - Get user's posts

### Posts
- `GET /api/posts` - List all posts
- `GET /api/posts/:id` - Get post by ID
- `POST /api/posts` - Create new post
- `PUT /api/posts/:id` - Update post
- `DELETE /api/posts/:id` - Delete post

### System
- `GET /health` - Health check
- `GET /` - API info

## Running

```bash
# Build ReScript
deno task build

# Start server
deno task start

# Development with watch
deno task dev
```

## Project Structure

```
rest-api-rescript/
├── deno.json           # Deno configuration
├── rescript.json       # ReScript configuration
└── src/
    ├── bindings/
    │   └── Deno.res    # Deno HTTP bindings
    ├── Router.res      # URL routing utilities
    └── Server.res      # Main server implementation
```

## License

SPDX-License-Identifier: MPL-2.0-or-later
