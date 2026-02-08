// REST API Server - ReScript + Deno Implementation
// SPDX-License-Identifier: AGPL-3.0-or-later

open Deno

// Types for our data models
type user = {
  id: int,
  name: string,
  email: string,
  createdAt: string,
}

type post = {
  id: int,
  userId: int,
  title: string,
  content: string,
  createdAt: string,
}

// In-memory data stores
let nextUserId = ref(4)
let nextPostId = ref(4)

let users: ref<array<user>> = ref([
  {id: 1, name: "Alice Johnson", email: "alice@example.com", createdAt: "2024-01-15T10:30:00Z"},
  {id: 2, name: "Bob Smith", email: "bob@example.com", createdAt: "2024-01-16T11:45:00Z"},
  {id: 3, name: "Carol White", email: "carol@example.com", createdAt: "2024-01-17T09:15:00Z"},
])

let posts: ref<array<post>> = ref([
  {id: 1, userId: 1, title: "Getting Started with ReScript", content: "ReScript is a robustly typed language that compiles to efficient JavaScript.", createdAt: "2024-01-18T14:00:00Z"},
  {id: 2, userId: 1, title: "Deno vs Node.js", content: "Comparing the two JavaScript runtimes and their ecosystems.", createdAt: "2024-01-19T16:30:00Z"},
  {id: 3, userId: 2, title: "Functional Programming Basics", content: "An introduction to functional programming concepts.", createdAt: "2024-01-20T10:00:00Z"},
])

// Helper to convert user to JSON
let userToJson = (u: user): Js.Json.t => {
  Js.Json.object_(Js.Dict.fromArray([
    ("id", Js.Json.number(Int.toFloat(u.id))),
    ("name", Js.Json.string(u.name)),
    ("email", Js.Json.string(u.email)),
    ("createdAt", Js.Json.string(u.createdAt)),
  ]))
}

// Helper to convert post to JSON
let postToJson = (p: post): Js.Json.t => {
  Js.Json.object_(Js.Dict.fromArray([
    ("id", Js.Json.number(Int.toFloat(p.id))),
    ("userId", Js.Json.number(Int.toFloat(p.userId))),
    ("title", Js.Json.string(p.title)),
    ("content", Js.Json.string(p.content)),
    ("createdAt", Js.Json.string(p.createdAt)),
  ]))
}

// Get current timestamp
let now = (): string => {
  Js.Date.make()->Js.Date.toISOString
}

// Parse URL pathname and extract ID
let extractId = (pathname: string): option<int> => {
  let parts = Js.String2.split(pathname, "/")
  parts[2]->Option.flatMap(s => Int.fromString(s))
}

// Request handlers
let getAllUsers = async (_req: Request.t): Response.t => {
  let data = users.contents->Array.map(userToJson)->Js.Json.array
  Response.json(data)
}

let getUserById = async (_req: Request.t, id: int): Response.t => {
  switch users.contents->Array.find(u => u.id == id) {
  | Some(user) => Response.json(userToJson(user))
  | None => Response.notFound()
  }
}

let createUser = async (req: Request.t): Response.t => {
  let body = await Request.json(req)

  switch Js.Json.decodeObject(body) {
  | Some(obj) => {
      let name = Js.Dict.get(obj, "name")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")
      let email = Js.Dict.get(obj, "email")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")

      if name == "" || email == "" {
        Response.badRequest("Name and email are required")
      } else {
        let newUser = {
          id: nextUserId.contents,
          name,
          email,
          createdAt: now(),
        }
        nextUserId := nextUserId.contents + 1
        users := Array.concat(users.contents, [newUser])
        Response.json(~status=201, userToJson(newUser))
      }
    }
  | None => Response.badRequest("Invalid JSON body")
  }
}

let updateUser = async (req: Request.t, id: int): Response.t => {
  let body = await Request.json(req)

  switch users.contents->Array.findIndex(u => u.id == id) {
  | -1 => Response.notFound()
  | idx =>
    switch Js.Json.decodeObject(body) {
    | Some(obj) => {
        let currentUser = users.contents[idx]->Option.getExn
        let name = Js.Dict.get(obj, "name")->Option.flatMap(Js.Json.decodeString)->Option.getOr(currentUser.name)
        let email = Js.Dict.get(obj, "email")->Option.flatMap(Js.Json.decodeString)->Option.getOr(currentUser.email)

        let updatedUser = {
          ...currentUser,
          name,
          email,
        }
        users := users.contents->Array.mapWithIndex((u, i) => i == idx ? updatedUser : u)
        Response.json(userToJson(updatedUser))
      }
    | None => Response.badRequest("Invalid JSON body")
    }
  }
}

let deleteUser = async (_req: Request.t, id: int): Response.t => {
  let initialLen = Array.length(users.contents)
  users := users.contents->Array.filter(u => u.id != id)

  if Array.length(users.contents) < initialLen {
    Response.json(Js.Json.object_(Js.Dict.fromArray([
      ("message", Js.Json.string("User deleted successfully")),
    ])))
  } else {
    Response.notFound()
  }
}

// Posts handlers
let getAllPosts = async (_req: Request.t): Response.t => {
  let data = posts.contents->Array.map(postToJson)->Js.Json.array
  Response.json(data)
}

let getPostById = async (_req: Request.t, id: int): Response.t => {
  switch posts.contents->Array.find(p => p.id == id) {
  | Some(post) => Response.json(postToJson(post))
  | None => Response.notFound()
  }
}

let getPostsByUser = async (_req: Request.t, userId: int): Response.t => {
  let userPosts = posts.contents->Array.filter(p => p.userId == userId)
  let data = userPosts->Array.map(postToJson)->Js.Json.array
  Response.json(data)
}

let createPost = async (req: Request.t): Response.t => {
  let body = await Request.json(req)

  switch Js.Json.decodeObject(body) {
  | Some(obj) => {
      let userId = Js.Dict.get(obj, "userId")->Option.flatMap(Js.Json.decodeNumber)->Option.map(Float.toInt)->Option.getOr(0)
      let title = Js.Dict.get(obj, "title")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")
      let content = Js.Dict.get(obj, "content")->Option.flatMap(Js.Json.decodeString)->Option.getOr("")

      if userId == 0 || title == "" {
        Response.badRequest("userId and title are required")
      } else if users.contents->Array.find(u => u.id == userId)->Option.isNone {
        Response.badRequest("User not found")
      } else {
        let newPost = {
          id: nextPostId.contents,
          userId,
          title,
          content,
          createdAt: now(),
        }
        nextPostId := nextPostId.contents + 1
        posts := Array.concat(posts.contents, [newPost])
        Response.json(~status=201, postToJson(newPost))
      }
    }
  | None => Response.badRequest("Invalid JSON body")
  }
}

let updatePost = async (req: Request.t, id: int): Response.t => {
  let body = await Request.json(req)

  switch posts.contents->Array.findIndex(p => p.id == id) {
  | -1 => Response.notFound()
  | idx =>
    switch Js.Json.decodeObject(body) {
    | Some(obj) => {
        let currentPost = posts.contents[idx]->Option.getExn
        let title = Js.Dict.get(obj, "title")->Option.flatMap(Js.Json.decodeString)->Option.getOr(currentPost.title)
        let content = Js.Dict.get(obj, "content")->Option.flatMap(Js.Json.decodeString)->Option.getOr(currentPost.content)

        let updatedPost = {
          ...currentPost,
          title,
          content,
        }
        posts := posts.contents->Array.mapWithIndex((p, i) => i == idx ? updatedPost : p)
        Response.json(postToJson(updatedPost))
      }
    | None => Response.badRequest("Invalid JSON body")
    }
  }
}

let deletePost = async (_req: Request.t, id: int): Response.t => {
  let initialLen = Array.length(posts.contents)
  posts := posts.contents->Array.filter(p => p.id != id)

  if Array.length(posts.contents) < initialLen {
    Response.json(Js.Json.object_(Js.Dict.fromArray([
      ("message", Js.Json.string("Post deleted successfully")),
    ])))
  } else {
    Response.notFound()
  }
}

// CORS middleware
let addCorsHeaders = (response: Response.t): Response.t => {
  response
}

// Main router
let handleRequest = async (req: Request.t): Response.t => {
  let url = URL.make(Request.url(req))
  let pathname = URL.pathname(url)
  let method_ = Request.method_(req)

  // Log request
  Js.Console.log(`${method_} ${pathname}`)

  // Route matching
  let response = switch (method_, pathname) {
  // Users routes
  | ("GET", "/api/users") => await getAllUsers(req)
  | ("POST", "/api/users") => await createUser(req)
  | ("GET", path) if Js.String2.startsWith(path, "/api/users/") && !Js.String2.includes(path, "/posts") =>
    switch extractId(path) {
    | Some(id) => await getUserById(req, id)
    | None => Response.badRequest("Invalid user ID")
    }
  | ("PUT", path) if Js.String2.startsWith(path, "/api/users/") =>
    switch extractId(path) {
    | Some(id) => await updateUser(req, id)
    | None => Response.badRequest("Invalid user ID")
    }
  | ("DELETE", path) if Js.String2.startsWith(path, "/api/users/") =>
    switch extractId(path) {
    | Some(id) => await deleteUser(req, id)
    | None => Response.badRequest("Invalid user ID")
    }

  // Posts routes
  | ("GET", "/api/posts") => await getAllPosts(req)
  | ("POST", "/api/posts") => await createPost(req)
  | ("GET", path) if Js.String2.startsWith(path, "/api/posts/") =>
    switch extractId(path) {
    | Some(id) => await getPostById(req, id)
    | None => Response.badRequest("Invalid post ID")
    }
  | ("PUT", path) if Js.String2.startsWith(path, "/api/posts/") =>
    switch extractId(path) {
    | Some(id) => await updatePost(req, id)
    | None => Response.badRequest("Invalid post ID")
    }
  | ("DELETE", path) if Js.String2.startsWith(path, "/api/posts/") =>
    switch extractId(path) {
    | Some(id) => await deletePost(req, id)
    | None => Response.badRequest("Invalid post ID")
    }

  // User posts route
  | ("GET", path) if Js.Re.test_(%re("/\/api\/users\/\d+\/posts/"), path) => {
      let parts = Js.String2.split(path, "/")
      switch parts[3]->Option.flatMap(Int.fromString) {
      | Some(userId) => await getPostsByUser(req, userId)
      | None => Response.badRequest("Invalid user ID")
      }
    }

  // Health check
  | ("GET", "/health") =>
    Response.json(Js.Json.object_(Js.Dict.fromArray([
      ("status", Js.Json.string("ok")),
      ("timestamp", Js.Json.string(now())),
    ])))

  // Root
  | ("GET", "/") =>
    Response.json(Js.Json.object_(Js.Dict.fromArray([
      ("message", Js.Json.string("REST API - ReScript + Deno")),
      ("version", Js.Json.string("1.0.0")),
      ("endpoints", Js.Json.object_(Js.Dict.fromArray([
        ("users", Js.Json.string("/api/users")),
        ("posts", Js.Json.string("/api/posts")),
        ("health", Js.Json.string("/health")),
      ]))),
    ])))

  // Not found
  | _ => Response.notFound()
  }

  response
}

// Start server
let port = 3000

let _ = DenoServe.serve({"port": port}, handleRequest)

Js.Console.log(`Server running on http://localhost:${Int.toString(port)}`)
