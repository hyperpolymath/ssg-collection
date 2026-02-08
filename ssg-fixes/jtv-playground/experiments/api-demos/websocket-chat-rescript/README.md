# WebSocket Chat - ReScript + Deno

A real-time chat server using WebSockets, implemented in ReScript and run with Deno.

## Features

- Real-time messaging
- Username-based authentication
- Private messaging
- User join/leave notifications
- User list broadcasting
- Message history

## Message Protocol

### Client to Server

```json
// Join chat
{"type": "join", "username": "alice"}

// Send chat message
{"type": "chat", "content": "Hello everyone!"}

// Send private message
{"type": "private", "target": "bob", "content": "Hey Bob!"}
```

### Server to Client

```json
// System message
{"type": "system", "content": "Welcome!", "timestamp": "..."}

// Chat message
{"type": "chat", "username": "alice", "content": "Hello!", "timestamp": "..."}

// User joined
{"type": "join", "username": "bob", "content": "bob has joined", "timestamp": "..."}

// User list
{"type": "userlist", "users": ["alice", "bob"], "timestamp": "..."}

// Private message
{"type": "private", "username": "alice", "content": "Secret!", "target": "bob", "timestamp": "..."}

// Error
{"type": "error", "content": "Username taken"}
```

## Running

```bash
# Build ReScript
deno task build

# Start server
deno task start

# Development with watch
deno task dev
```

## Endpoints

- `GET /` - Server info (JSON)
- `GET /ws` - WebSocket connection
- `GET /chat` - WebSocket connection (alias)

## Project Structure

```
websocket-chat-rescript/
├── deno.json           # Deno configuration
├── rescript.json       # ReScript configuration
└── src/
    ├── bindings/
    │   └── WebSocket.res  # WebSocket bindings
    └── Server.res         # Chat server implementation
```

## License

SPDX-License-Identifier: MPL-2.0-or-later
