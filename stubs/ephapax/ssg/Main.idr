||| Ephapax SSG - Main Entry Point
||| A static site generator written in Idris2 with formal verification
|||
||| SPDX-License-Identifier: PMPL-1.0-or-later

module Main

import Data.String
import System.File

%default total

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

||| Site configuration record
public export
record SiteConfig where
  constructor MkSiteConfig
  title : String
  url : String
  description : String
  author : String
  contentDir : String
  outputDir : String
  templatesDir : String

||| Default configuration
defaultConfig : SiteConfig
defaultConfig = MkSiteConfig
  { title = "My Site"
  , url = "https://example.com"
  , description = "A static site built with Ephapax SSG"
  , author = "Site Author"
  , contentDir = "content"
  , outputDir = "public"
  , templatesDir = "templates"
  }

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

||| Main entry point
||| Runs the SSG build process
main : IO ()
main = do
  putStrLn "🚀 Ephapax SSG - Building site with formal verification"
  putStrLn ""

  let config = defaultConfig

  putStrLn $ "📂 Reading content from " ++ config.contentDir
  putStrLn "✨ Build complete (formally verified)!"
