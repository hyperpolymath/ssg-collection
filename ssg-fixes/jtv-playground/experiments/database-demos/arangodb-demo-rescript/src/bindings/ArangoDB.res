// ArangoDB Bindings for ReScript
// SPDX-License-Identifier: AGPL-3.0-or-later

type cursor<'a>
type collection
type edgeCollection
type graph

type authConfig = {
  username: string,
  password: string,
}

type databaseConfig = {
  url: string,
  databaseName: string,
  auth: authConfig,
}

type edgeDefinition = {
  collection: string,
  from: array<string>,
  to_: array<string>,
}

type database

@module("https://deno.land/x/arangojs@8.0.0/mod.ts") @new
external createDatabase: databaseConfig => database = "Database"

@send external listDatabases: database => promise<array<string>> = "listDatabases"
@send external createDatabaseNamed: (database, string) => promise<unit> = "createDatabase"
@send external useDatabase: (database, string) => unit = "useDatabase"
@send external listCollections: database => promise<array<{.."name": string}>> = "listCollections"
@send external createCollection: (database, string) => promise<collection> = "createCollection"
@send external createEdgeCollection: (database, string) => promise<edgeCollection> = "createEdgeCollection"
@send external collection: (database, string) => collection = "collection"

@send external listGraphs: database => promise<array<{.."_key": string}>> = "listGraphs"

// Graph creation requires proper edge definitions
@send external createGraph: (database, string, array<{..}>) => promise<graph> = "createGraph"

// Collection operations
type saveOptions = {overwriteMode?: string}

@send external save: (collection, 'a, saveOptions) => promise<unit> = "save"
@send external saveEdge: (edgeCollection, 'a, saveOptions) => promise<unit> = "save"

// Query operations
@send external query: (database, string) => promise<cursor<'a>> = "query"
@send external queryWithBindVars: (database, string, 'bindVars) => promise<cursor<'a>> = "query"

// Cursor operations
@send external all: cursor<'a> => promise<array<'a>> = "all"
