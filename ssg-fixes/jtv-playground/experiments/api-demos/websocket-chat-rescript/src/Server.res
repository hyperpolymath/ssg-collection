// WebSocket Chat Server - ReScript + Deno
// SPDX-License-Identifier: AGPL-3.0-or-later

open WebSocket

// Message types
type messageType =
  | Join
  | Leave
  | Chat
  | System
  | UserList
  | PrivateMessage

type chatMessage = {
  type_: string,
  username: string,
  content: string,
  timestamp: string,
  target?: string,
}

// Client state
type client = {
  socket: WebSocket.t,
  username: string,
  joinedAt: string,
}

// Room management
module Room = {
  type t = {
    mutable clients: array<client>,
    mutable messageHistory: array<chatMessage>,
    maxHistory: int,
  }

  let make = (): t => {
    clients: [],
    messageHistory: [],
    maxHistory: 100,
  }

  let addClient = (room: t, client: client): unit => {
    room.clients = Array.concat(room.clients, [client])
  }

  let removeClient = (room: t, username: string): unit => {
    room.clients = room.clients->Array.filter(c => c.username != username)
  }

  let findClient = (room: t, username: string): option<client> => {
    room.clients->Array.find(c => c.username == username)
  }

  let hasUsername = (room: t, username: string): bool => {
    findClient(room, username)->Option.isSome
  }

  let getUsernames = (room: t): array<string> => {
    room.clients->Array.map(c => c.username)
  }

  let addMessage = (room: t, msg: chatMessage): unit => {
    room.messageHistory = Array.concat(room.messageHistory, [msg])
    if Array.length(room.messageHistory) > room.maxHistory {
      room.messageHistory = room.messageHistory->Array.sliceToEnd(~start=1)
    }
  }

  let broadcast = (room: t, message: chatMessage, ~exclude: option<string>=?): unit => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("type", Js.Json.string(message.type_)),
      ("username", Js.Json.string(message.username)),
      ("content", Js.Json.string(message.content)),
      ("timestamp", Js.Json.string(message.timestamp)),
    ]))

    room.clients->Array.forEach(client => {
      let shouldSend = switch exclude {
      | Some(name) => client.username != name
      | None => true
      }
      if shouldSend && isOpen(client.socket) {
        sendJson(client.socket, json)
      }
    })
  }

  let sendTo = (room: t, username: string, message: chatMessage): bool => {
    switch findClient(room, username) {
    | Some(client) if isOpen(client.socket) => {
        let json = Js.Json.object_(Js.Dict.fromArray([
          ("type", Js.Json.string(message.type_)),
          ("username", Js.Json.string(message.username)),
          ("content", Js.Json.string(message.content)),
          ("timestamp", Js.Json.string(message.timestamp)),
        ]))
        sendJson(client.socket, json)
        true
      }
    | _ => false
    }
  }

  let sendUserList = (room: t, client: client): unit => {
    let json = Js.Json.object_(Js.Dict.fromArray([
      ("type", Js.Json.string("userlist")),
      ("users", Js.Json.array(getUsernames(room)->Array.map(Js.Json.string))),
      ("timestamp", Js.Json.string(now())),
    ]))
    if isOpen(client.socket) {
      sendJson(client.socket, json)
    }
  }
}

// Global chat room
let chatRoom = Room.make()

// Timestamp helper
and now = (): string => {
  Js.Date.make()->Js.Date.toISOString
}

// Parse incoming message
let parseMessage = (data: string): option<{..}> => {
  try {
    Some(Js.Json.parseExn(data)->Js.Json.decodeObject->Option.getExn)
  } catch {
  | _ => None
  }
}

// Handle WebSocket connection
let handleConnection = (socket: WebSocket.t): unit => {
  let clientUsername = ref("")

  onopen(socket, () => {
    Js.Console.log("New connection")
  })

  onmessage(socket, (event: messageEvent) => {
    switch parseMessage(event.data) {
    | Some(obj) => {
        let type_ = Js.Dict.get(obj, "type")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")

        switch type_ {
        | "join" => {
            let username = Js.Dict.get(obj, "username")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")

            if username == "" {
              let errorMsg = Js.Json.object_(Js.Dict.fromArray([
                ("type", Js.Json.string("error")),
                ("content", Js.Json.string("Username is required")),
              ]))
              sendJson(socket, errorMsg)
            } else if Room.hasUsername(chatRoom, username) {
              let errorMsg = Js.Json.object_(Js.Dict.fromArray([
                ("type", Js.Json.string("error")),
                ("content", Js.Json.string("Username already taken")),
              ]))
              sendJson(socket, errorMsg)
            } else {
              clientUsername := username

              let client: client = {
                socket,
                username,
                joinedAt: now(),
              }
              Room.addClient(chatRoom, client)

              // Send welcome message to new user
              let welcomeMsg = Js.Json.object_(Js.Dict.fromArray([
                ("type", Js.Json.string("system")),
                ("content", Js.Json.string(`Welcome to the chat, ${username}!`)),
                ("timestamp", Js.Json.string(now())),
              ]))
              sendJson(socket, welcomeMsg)

              // Send user list
              Room.sendUserList(chatRoom, client)

              // Broadcast join to others
              let joinMsg: chatMessage = {
                type_: "join",
                username,
                content: `${username} has joined the chat`,
                timestamp: now(),
              }
              Room.broadcast(chatRoom, joinMsg, ~exclude=username)
              Room.addMessage(chatRoom, joinMsg)

              Js.Console.log(`${username} joined`)
            }
          }

        | "chat" => {
            let content = Js.Dict.get(obj, "content")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")

            if clientUsername.contents != "" && content != "" {
              let chatMsg: chatMessage = {
                type_: "chat",
                username: clientUsername.contents,
                content,
                timestamp: now(),
              }
              Room.broadcast(chatRoom, chatMsg)
              Room.addMessage(chatRoom, chatMsg)
            }
          }

        | "private" => {
            let target = Js.Dict.get(obj, "target")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")
            let content = Js.Dict.get(obj, "content")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")

            if clientUsername.contents != "" && target != "" && content != "" {
              let privateMsg: chatMessage = {
                type_: "private",
                username: clientUsername.contents,
                content,
                timestamp: now(),
                target,
              }

              // Send to target
              let sent = Room.sendTo(chatRoom, target, privateMsg)
              if !sent {
                let errorMsg = Js.Json.object_(Js.Dict.fromArray([
                  ("type", Js.Json.string("error")),
                  ("content", Js.Json.string(`User ${target} not found`)),
                ]))
                sendJson(socket, errorMsg)
              } else {
                // Echo back to sender
                sendJson(socket, Js.Json.object_(Js.Dict.fromArray([
                  ("type", Js.Json.string("private")),
                  ("username", Js.Json.string(clientUsername.contents)),
                  ("content", Js.Json.string(content)),
                  ("target", Js.Json.string(target)),
                  ("timestamp", Js.Json.string(now())),
                ])))
              }
            }
          }

        | _ => ()
        }
      }
    | None => ()
    }
  })

  onclose(socket, (_event: closeEvent) => {
    if clientUsername.contents != "" {
      Room.removeClient(chatRoom, clientUsername.contents)

      let leaveMsg: chatMessage = {
        type_: "leave",
        username: clientUsername.contents,
        content: `${clientUsername.contents} has left the chat`,
        timestamp: now(),
      }
      Room.broadcast(chatRoom, leaveMsg)
      Room.addMessage(chatRoom, leaveMsg)

      Js.Console.log(`${clientUsername.contents} left`)
    }
  })

  onerror(socket, error => {
    Js.Console.error2("WebSocket error:", error)
  })
}

// HTTP handler for upgrade
type request = {
  url: string,
}

type upgradeResult = {
  socket: WebSocket.t,
  response: 'response,
}

@scope("Deno") @val
external upgradeWebSocket: request => upgradeResult = "upgradeWebSocket"

let handleRequest = (req: request): 'response => {
  let url = %raw(`new URL(req.url)`)
  let pathname: string = %raw(`url.pathname`)

  if pathname == "/ws" || pathname == "/chat" {
    let result = upgradeWebSocket(req)
    handleConnection(result.socket)
    result.response
  } else if pathname == "/" {
    %raw(`new Response(JSON.stringify({
      message: "WebSocket Chat Server",
      version: "1.0.0",
      endpoints: {
        websocket: "/ws or /chat",
        info: "/"
      }
    }), { headers: { "Content-Type": "application/json" } })`)
  } else {
    %raw(`new Response("Not Found", { status: 404 })`)
  }
}

// Start server
let port = 3001

let _ = %raw(`Deno.serve({ port: port }, handleRequest)`)

Js.Console.log(`WebSocket Chat Server running on http://localhost:${Int.toString(port)}`)
Js.Console.log("Connect via WebSocket at /ws or /chat")
