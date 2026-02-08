// ArangoDB Multi-Model Database Demo - ReScript
// SPDX-License-Identifier: AGPL-3.0-or-later

open ArangoDB

// Type definitions for our data models
type user = {
  _key: string,
  name: string,
  email: string,
  age: int,
  city: string,
}

type tag = {
  _key: string,
  name: string,
  category: string,
}

type post = {
  _key: string,
  author: string,
  title: string,
  content: string,
  views: int,
  created_at: Js.Date.t,
}

type edge = {
  _from: string,
  _to: string,
}

type likeEdge = {
  ...edge,
  liked_at: Js.Date.t,
}

// Result types for queries
type userWithFollowers = {
  name: string,
  email: string,
  followers: int,
}

type popularPost = {
  title: string,
  author: string,
  likes: int,
  views: int,
}

type postWithTags = {
  title: string,
  author: string,
  tags: array<string>,
  views: int,
}

type userStats = {
  user: string,
  posts: int,
  totalViews: int,
  totalLikes: int,
  followers: int,
  avgViewsPerPost: float,
}

// ArangoDB Demo class
module ArangoDBDemo = {
  type t = {db: database}

  let make = (~url="http://localhost:8529"): t => {
    let db = createDatabase({
      url,
      databaseName: "playground",
      auth: {username: "root", password: "rootpassword"},
    })
    {db: db}
  }

  let initialize = async (demo: t): unit => {
    Js.Console.log("Initializing ArangoDB...")

    // Check if database exists
    let databases = await listDatabases(demo.db)
    if !Array.includes(databases, "playground") {
      let _ = await createDatabaseNamed(demo.db, "playground")
      useDatabase(demo.db, "playground")
    }

    // Create collections
    let collections = await listCollections(demo.db)
    let collectionNames = collections->Array.map(c => c["name"])

    let requiredCollections = ["users", "posts", "comments", "tags"]
    let edgeCollections = ["follows", "likes", "tagged_with"]

    for name in requiredCollections {
      if !Array.includes(collectionNames, name) {
        let _ = await createCollection(demo.db, name)
        Js.Console.log(`Created collection: ${name}`)
      }
    }

    for name in edgeCollections {
      if !Array.includes(collectionNames, name) {
        let _ = await createEdgeCollection(demo.db, name)
        Js.Console.log(`Created edge collection: ${name}`)
      }
    }

    // Create graph
    let graphs = await listGraphs(demo.db)
    let graphKeys = graphs->Array.map(g => g["_key"])

    if !Array.includes(graphKeys, "social") {
      let edgeDefs = [
        {"collection": "follows", "from": ["users"], "to": ["users"]},
        {"collection": "likes", "from": ["users"], "to": ["posts"]},
        {"collection": "tagged_with", "from": ["posts"], "to": ["tags"]},
      ]
      let _ = await createGraph(demo.db, "social", edgeDefs)
      Js.Console.log("Created social graph")
    }

    // Insert sample data
    await insertSampleData(demo)
    Js.Console.log("Database initialized")
  }

  and insertSampleData = async (demo: t): unit => {
    let users = collection(demo.db, "users")
    let posts = collection(demo.db, "posts")
    let tags = collection(demo.db, "tags")
    let follows = collection(demo.db, "follows")
    let likes = collection(demo.db, "likes")
    let taggedWith = collection(demo.db, "tagged_with")

    // Users
    let userData: array<user> = [
      {_key: "alice", name: "Alice Johnson", email: "alice@example.com", age: 28, city: "New York"},
      {_key: "bob", name: "Bob Smith", email: "bob@example.com", age: 35, city: "San Francisco"},
      {_key: "charlie", name: "Charlie Davis", email: "charlie@example.com", age: 42, city: "Austin"},
      {_key: "diana", name: "Diana Miller", email: "diana@example.com", age: 31, city: "Seattle"},
    ]

    for u in userData {
      let _ = await save(users, u, {overwriteMode: "ignore"})
    }

    // Tags
    let tagData: array<tag> = [
      {_key: "javascript", name: "JavaScript", category: "programming"},
      {_key: "arangodb", name: "ArangoDB", category: "database"},
      {_key: "webdev", name: "Web Development", category: "general"},
      {_key: "performance", name: "Performance", category: "optimization"},
    ]

    for t in tagData {
      let _ = await save(tags, t, {overwriteMode: "ignore"})
    }

    // Posts
    let postData: array<post> = [
      {
        _key: "post1",
        author: "users/alice",
        title: "Getting Started with ArangoDB",
        content: "ArangoDB is a multi-model database...",
        views: 150,
        created_at: Js.Date.fromString("2025-01-15"),
      },
      {
        _key: "post2",
        author: "users/bob",
        title: "JavaScript Best Practices",
        content: "Here are some best practices...",
        views: 320,
        created_at: Js.Date.fromString("2025-01-18"),
      },
      {
        _key: "post3",
        author: "users/alice",
        title: "Graph Databases Explained",
        content: "Graph databases model data...",
        views: 280,
        created_at: Js.Date.fromString("2025-01-20"),
      },
    ]

    for p in postData {
      let _ = await save(posts, p, {overwriteMode: "ignore"})
    }

    // Follows
    let followsData: array<edge> = [
      {_from: "users/bob", _to: "users/alice"},
      {_from: "users/charlie", _to: "users/alice"},
      {_from: "users/diana", _to: "users/alice"},
      {_from: "users/alice", _to: "users/bob"},
      {_from: "users/charlie", _to: "users/bob"},
    ]

    for f in followsData {
      let _ = await save(follows, f, {overwriteMode: "ignore"})
    }

    // Likes
    let likesData: array<edge> = [
      {_from: "users/bob", _to: "posts/post1"},
      {_from: "users/charlie", _to: "posts/post1"},
      {_from: "users/diana", _to: "posts/post1"},
      {_from: "users/alice", _to: "posts/post2"},
    ]

    for l in likesData {
      let _ = await save(likes, l, {overwriteMode: "ignore"})
    }

    // Tagged with
    let taggedData: array<edge> = [
      {_from: "posts/post1", _to: "tags/arangodb"},
      {_from: "posts/post1", _to: "tags/webdev"},
      {_from: "posts/post2", _to: "tags/javascript"},
      {_from: "posts/post2", _to: "tags/webdev"},
      {_from: "posts/post3", _to: "tags/arangodb"},
    ]

    for t in taggedData {
      let _ = await save(taggedWith, t, {overwriteMode: "ignore"})
    }

    Js.Console.log("Sample data inserted")
  }

  // AQL Queries

  let getAllUsers = async (demo: t): array<user> => {
    Js.Console.log("\n Query: Get all users")

    let aql = `
      FOR user IN users
        SORT user.name
        RETURN user
    `

    let cursor = await query(demo.db, aql)
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }

  let getUsersWithFollowerCount = async (demo: t): array<userWithFollowers> => {
    Js.Console.log("\n Query: Users with follower count")

    let aql = `
      FOR user IN users
        LET followerCount = LENGTH(
          FOR follow IN follows
            FILTER follow._to == user._id
            RETURN 1
        )
        RETURN {
          name: user.name,
          email: user.email,
          followers: followerCount
        }
    `

    let cursor = await query(demo.db, aql)
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }

  let getPopularPosts = async (demo: t, ~minLikes=2): array<popularPost> => {
    Js.Console.log(`\n Query: Popular posts (min ${Int.toString(minLikes)} likes)`)

    let aql = `
      FOR post IN posts
        LET likeCount = LENGTH(
          FOR like IN likes
            FILTER like._to == post._id
            RETURN 1
        )
        FILTER likeCount >= @minLikes
        SORT likeCount DESC
        RETURN {
          title: post.title,
          author: DOCUMENT(post.author).name,
          likes: likeCount,
          views: post.views
        }
    `

    let cursor = await queryWithBindVars(demo.db, aql, {"minLikes": minLikes})
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }

  let getPostsWithTags = async (demo: t): array<postWithTags> => {
    Js.Console.log("\n Query: Posts with their tags")

    let aql = `
      FOR post IN posts
        LET postTags = (
          FOR v IN 1..1 OUTBOUND post tagged_with
            RETURN v.name
        )
        RETURN {
          title: post.title,
          author: DOCUMENT(post.author).name,
          tags: postTags,
          views: post.views
        }
    `

    let cursor = await query(demo.db, aql)
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }

  let graphTraversal_FollowersOfFollowers = async (demo: t, username: string): array<{.."name": string, "distance": int}> => {
    Js.Console.log(`\n Graph Traversal: Followers of ${username}'s followers`)

    let aql = `
      FOR user IN users
        FILTER user._key == @username
        FOR v, e, p IN 2..2 INBOUND user follows
          FILTER v._key != @username
          RETURN DISTINCT {
            name: v.name,
            distance: LENGTH(p.edges)
          }
    `

    let cursor = await queryWithBindVars(demo.db, aql, {"username": username})
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }

  let graphTraversal_ShortestPath = async (demo: t, from: string, to_: string): array<string> => {
    Js.Console.log(`\n Shortest path: ${from} to ${to_}`)

    let aql = `
      FOR user1 IN users
        FILTER user1._key == @from
        FOR user2 IN users
          FILTER user2._key == @to
          LET path = (
            FOR v, e IN OUTBOUND SHORTEST_PATH user1 TO user2 follows
              RETURN v.name
          )
          RETURN path
    `

    let cursor = await queryWithBindVars(demo.db, aql, {"from": from, "to": to_})
    let results = await all(cursor)
    let path = results[0]->Option.getOr([])
    Js.Console.log2("Path:", path)
    path
  }

  let complexAggregation = async (demo: t): array<userStats> => {
    Js.Console.log("\n Complex Aggregation: User engagement stats")

    let aql = `
      FOR user IN users
        LET userPosts = (
          FOR post IN posts
            FILTER post.author == user._id
            RETURN post
        )
        LET totalViews = SUM(userPosts[*].views)
        LET totalLikes = SUM(
          FOR post IN userPosts
            LET likes = LENGTH(
              FOR like IN likes
                FILTER like._to == post._id
                RETURN 1
            )
            RETURN likes
        )
        LET followerCount = LENGTH(
          FOR follow IN follows
            FILTER follow._to == user._id
            RETURN 1
        )
        RETURN {
          user: user.name,
          posts: LENGTH(userPosts),
          totalViews: totalViews,
          totalLikes: totalLikes,
          followers: followerCount,
          avgViewsPerPost: totalViews / (LENGTH(userPosts) || 1)
        }
    `

    let cursor = await query(demo.db, aql)
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }

  let fullTextSearch = async (demo: t, searchTerm: string): array<{.."title": string, "author": string, "excerpt": string}> => {
    Js.Console.log(`\n Full-text search: "${searchTerm}"`)

    let aql = `
      FOR post IN posts
        FILTER CONTAINS(LOWER(post.title), LOWER(@searchTerm)) OR
               CONTAINS(LOWER(post.content), LOWER(@searchTerm))
        RETURN {
          title: post.title,
          author: DOCUMENT(post.author).name,
          excerpt: SUBSTRING(post.content, 0, 100)
        }
    `

    let cursor = await queryWithBindVars(demo.db, aql, {"searchTerm": searchTerm})
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }

  let recommendation_SimilarPosts = async (demo: t, postKey: string): array<{.."title": string, "commonTags": int, "views": int}> => {
    Js.Console.log(`\n Recommendation: Posts similar to ${postKey}`)

    let aql = `
      FOR post IN posts
        FILTER post._key == @postKey
        LET postTags = (
          FOR v IN 1..1 OUTBOUND post tagged_with
            RETURN v._id
        )
        FOR otherPost IN posts
          FILTER otherPost._key != @postKey
          LET otherTags = (
            FOR v IN 1..1 OUTBOUND otherPost tagged_with
              RETURN v._id
          )
          LET commonTags = LENGTH(INTERSECTION(postTags, otherTags))
          FILTER commonTags > 0
          SORT commonTags DESC
          LIMIT 3
          RETURN {
            title: otherPost.title,
            commonTags: commonTags,
            views: otherPost.views
          }
    `

    let cursor = await queryWithBindVars(demo.db, aql, {"postKey": postKey})
    let results = await all(cursor)
    Js.Console.log2("Results:", results)
    results
  }
}

// Demo execution
let demo = async () => {
  Js.Console.log("=".repeat(60))
  Js.Console.log("ArangoDB Multi-Model Database Demo - ReScript")
  Js.Console.log("=".repeat(60))

  let arango = ArangoDBDemo.make()

  try {
    await ArangoDBDemo.initialize(arango)

    let _ = await ArangoDBDemo.getAllUsers(arango)
    let _ = await ArangoDBDemo.getUsersWithFollowerCount(arango)
    let _ = await ArangoDBDemo.getPopularPosts(arango, ~minLikes=2)
    let _ = await ArangoDBDemo.getPostsWithTags(arango)
    let _ = await ArangoDBDemo.graphTraversal_FollowersOfFollowers(arango, "alice")
    let _ = await ArangoDBDemo.graphTraversal_ShortestPath(arango, "alice", "diana")
    let _ = await ArangoDBDemo.complexAggregation(arango)
    let _ = await ArangoDBDemo.fullTextSearch(arango, "database")
    let _ = await ArangoDBDemo.recommendation_SimilarPosts(arango, "post1")

    Js.Console.log("\nDemo completed!")
  } catch {
  | Js.Exn.Error(e) => Js.Console.error2("Demo error:", e)
  }
}

let _ = demo()
