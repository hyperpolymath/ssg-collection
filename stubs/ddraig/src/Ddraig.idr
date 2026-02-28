-- SPDX-License-Identifier: PMPL-1.0-or-later
--
-- Ddraig.idr - Dependently typed static site generator in Idris 2
--
-- "Ddraig" (Welsh for Dragon) - Types that breathe fire
--
-- A complete static site generator with:
--   - Markdown-to-HTML conversion (headings, bold, italic, code, lists)
--   - YAML-like frontmatter parsing (title, date, tags, draft, template, slug, order)
--   - Template engine with placeholder substitution
--   - Multi-page site building with tabbed navigation
--   - File I/O for reading content directories and writing HTML output
--   - Active-tab CSS class generation for navigation highlighting
--
-- Build: idris2 Ddraig.idr -o ddraig
-- Usage: ./build/exec/ddraig build <content-dir> <output-dir> <template-file>

module Ddraig

import Data.List
import Data.String
import System
import System.File
import System.Directory

-- ============================================================================
-- Frontmatter Types
-- ============================================================================
-- Frontmatter is the YAML-like metadata block at the top of each markdown
-- file, delimited by --- lines. It controls page title, date, tags, draft
-- status, template selection, URL slug, and navigation tab ordering.

public export
record Frontmatter where
  constructor MkFrontmatter
  title : String       -- Page title (displayed in nav and <title>)
  date : String        -- Publication date (ISO 8601 format recommended)
  tags : List String   -- Categorisation tags (comma-separated or bracketed)
  draft : Bool         -- Draft pages are skipped during site build
  template : String    -- Template name to use (default: "default")
  slug : String        -- URL slug (defaults to filename without .md extension)
  navOrder : Nat       -- Tab ordering (0 = unordered, 1-N = explicit order)

export
emptyFrontmatter : Frontmatter
emptyFrontmatter = MkFrontmatter "" "" [] False "default" "" 0

-- ============================================================================
-- String Utilities (all self-contained)
-- ============================================================================
-- These are deliberately self-contained rather than relying on external
-- string libraries. This keeps ddraig dependency-free and demonstrates
-- pure functional string processing in a dependently typed language.

-- | Check whether a string starts with a given prefix.
-- Examples: strHasPrefix "##" "## Hello" == True
strHasPrefix : String -> String -> Bool
strHasPrefix pre str =
  let plen = length pre
      slen = length str
  in if plen > slen
       then False
       else substr 0 plen str == pre

-- | Remove a known prefix from a string. If the prefix is not present,
-- return the original string unchanged.
strDropPrefix : String -> String -> String
strDropPrefix pre str =
  if strHasPrefix pre str
     then substr (length pre) (length str) str
     else str

-- | Check whether a string ends with a given suffix.
-- Used primarily for file extension matching (e.g., ".md" files).
strHasSuffix : String -> String -> Bool
strHasSuffix suf str =
  let sufLen = length suf
      strLen = length str
  in if sufLen > strLen then False
     else substr (strLen `minus` sufLen) sufLen str == suf

-- | Remove a known suffix from a string. If the suffix is not present,
-- return the original string unchanged. Used to derive slugs from filenames
-- (e.g., "about.md" -> "about").
strStripSuffix : String -> String -> String
strStripSuffix suf str =
  if strHasSuffix suf str
     then substr 0 (length str `minus` length suf) str
     else str

-- | Split a string on a delimiter character into a list of substrings.
-- The delimiter is consumed and not included in any resulting substring.
strSplitOn : Char -> String -> List String
strSplitOn delim str = go (unpack str) [] []
  where
    go : List Char -> List Char -> List String -> List String
    go [] acc result = reverse (pack (reverse acc) :: result)
    go (x :: xs) acc result =
      if x == delim
         then go xs [] (pack (reverse acc) :: result)
         else go xs (x :: acc) result

-- | Escape HTML special characters to prevent XSS and ensure valid HTML
-- output. Replaces <, >, &, and " with their entity equivalents.
strEscape : String -> String
strEscape s = concat (map esc (unpack s))
  where
    esc : Char -> String
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc '"' = "&quot;"
    esc c = singleton c

-- | Join two path components with a forward slash separator.
-- Does not normalise paths (no double-slash removal or . handling).
pathJoin : String -> String -> String
pathJoin dir file = dir ++ "/" ++ file

-- | Parse a string as a natural number. Returns 0 for non-numeric or
-- negative input. Used for navOrder parsing from frontmatter.
strToNat : String -> Nat
strToNat s =
  case the (Maybe Integer) (parseInteger (trim s)) of
    Just n  => if n >= 0 then cast n else 0
    Nothing => 0

-- ============================================================================
-- Frontmatter Parser
-- ============================================================================
-- Parses key:value pairs from the YAML-like frontmatter block.
-- Supports: title, date, template, draft, tags, slug, order.
-- Unknown keys are silently ignored (forward-compatible).

parseFmLine : String -> Frontmatter -> Frontmatter
parseFmLine line fm =
  case break (== ':') (unpack line) of
    (keyChars, []) => fm
    (keyChars, _ :: valueChars) =>
      let key = trim (pack keyChars)
          value = trim (pack valueChars)
      in case key of
           "title" => { title := value } fm
           "date" => { date := value } fm
           "template" => { template := value } fm
           "draft" => { draft := (value == "true" || value == "yes") } fm
           "slug" => { slug := value } fm
           "order" => { navOrder := strToNat value } fm
           "tags" =>
             let tagStr = if strHasPrefix "[" value
                            then substr 1 (length value `minus` 2) value
                            else value
                 tagList = map trim (strSplitOn ',' tagStr)
             in { tags := filter (\s => length s > 0) tagList } fm
           _ => fm

-- | Parse the frontmatter block from a markdown document.
-- Returns a tuple of (parsed Frontmatter, remaining body content).
-- If no frontmatter block is present (no leading ---), returns
-- emptyFrontmatter and the full content unchanged.
export
parseFrontmatter : String -> (Frontmatter, String)
parseFrontmatter content =
  let allLines = lines content
  in case allLines of
       [] => (emptyFrontmatter, content)
       (first :: rest) =>
         if trim first /= "---"
            then (emptyFrontmatter, content)
            else findEnd rest emptyFrontmatter
  where
    findEnd : List String -> Frontmatter -> (Frontmatter, String)
    findEnd [] fm = (fm, "")
    findEnd (l :: ls) fm =
      if trim l == "---"
         then (fm, unlines ls)
         else findEnd ls (parseFmLine l fm)

-- ============================================================================
-- Site Page Types
-- ============================================================================
-- A SitePage represents a fully processed content page: its source has been
-- parsed (frontmatter extracted, markdown converted to HTML) and it is ready
-- for template application and output writing.

public export
record SitePage where
  constructor MkSitePage
  slug : String           -- URL-safe identifier (derived from filename or frontmatter)
  filename : String       -- Original source filename (e.g., "about.md")
  frontmatter : Frontmatter -- Parsed metadata from the frontmatter block
  htmlContent : String    -- Markdown body converted to HTML

-- | Sort site pages by their navOrder field. Pages with lower navOrder
-- appear first in the navigation. Pages with navOrder 0 are placed at the
-- end (unordered). This ensures explicit tab ordering is respected while
-- allowing unordered pages to appear after the main navigation.
export
sortPages : List SitePage -> List SitePage
sortPages = sortBy compareOrder
  where
    -- Pages with navOrder 0 sort after pages with explicit order
    effectiveOrder : SitePage -> Nat
    effectiveOrder p = if p.frontmatter.navOrder == 0 then 9999 else p.frontmatter.navOrder
    compareOrder : SitePage -> SitePage -> Ordering
    compareOrder a b = compare (effectiveOrder a) (effectiveOrder b)

-- ============================================================================
-- Markdown Parser
-- ============================================================================
-- A line-by-line, stateful markdown parser. Tracks whether we are currently
-- inside a paragraph, code block, or list to correctly open and close HTML
-- tags. Supports: headings (h1-h6), bold (**), italic (*), inline code (`),
-- fenced code blocks (```), and unordered lists (- or *).

record ParserState where
  constructor MkState
  html : String        -- Accumulated HTML output
  inPara : Bool        -- Currently inside a <p> tag
  inCode : Bool        -- Currently inside a <pre><code> block
  inList : Bool        -- Currently inside a <ul> or <ol>
  orderedList : Bool   -- True if current list is <ol>, False if <ul>

initState : ParserState
initState = MkState "" False False False False

-- | Close an open paragraph tag if one is active.
closePara : ParserState -> ParserState
closePara st =
  if st.inPara
     then { html $= (++ "</p>\n"), inPara := False } st
     else st

-- | Close an open list tag (ul or ol) if one is active.
closeUl : ParserState -> ParserState
closeUl st =
  if st.inList
     then let tag = if st.orderedList then "</ol>\n" else "</ul>\n"
          in { html $= (++ tag), inList := False } st
     else st

-- | Process inline formatting within a single line of text.
-- Handles: **bold**, *italic*, and `inline code` spans.
-- Bold and italic are tracked by boolean state flags to handle
-- opening and closing markers correctly.
doInline : String -> String
doInline text = go (unpack text) "" False False
  where
    go : List Char -> String -> Bool -> Bool -> String
    go [] acc _ _ = acc
    go ('*' :: '*' :: rest) acc False iIt = go rest (acc ++ "<strong>") True iIt
    go ('*' :: '*' :: rest) acc True iIt = go rest (acc ++ "</strong>") False iIt
    go ('*' :: rest) acc iBold False = go rest (acc ++ "<em>") iBold True
    go ('*' :: rest) acc iBold True = go rest (acc ++ "</em>") iBold False
    go ('`' :: rest) acc iBold iIt =
      let (code, rem) = span (/= '`') rest
      in case rem of
           ('`' :: r) => go r (acc ++ "<code>" ++ pack code ++ "</code>") iBold iIt
           _ => go rest (acc ++ "`") iBold iIt
    go (c :: rest) acc iBold iIt = go rest (acc ++ singleton c) iBold iIt

-- | Process a single line of markdown, updating the parser state.
-- Determines the line type (heading, list item, code fence, blank, or
-- paragraph text) and emits the appropriate HTML, managing state
-- transitions for nested structures.
doLine : String -> ParserState -> ParserState
doLine line st =
  let tr = trim line
  in if strHasPrefix "```" tr
       then if st.inCode
               then { html $= (++ "</code></pre>\n"), inCode := False } st
               else let st1 = closeUl (closePara st)
                    in { html $= (++ "<pre><code>"), inCode := True } st1
     else if st.inCode
       then { html $= (++ strEscape line ++ "\n") } st
     else if tr == ""
       then closeUl (closePara st)
     else if strHasPrefix "######" tr
       then let st1 = closeUl (closePara st)
                c = doInline (trim (strDropPrefix "######" tr))
            in { html $= (++ "<h6>" ++ c ++ "</h6>\n") } st1
     else if strHasPrefix "#####" tr
       then let st1 = closeUl (closePara st)
                c = doInline (trim (strDropPrefix "#####" tr))
            in { html $= (++ "<h5>" ++ c ++ "</h5>\n") } st1
     else if strHasPrefix "####" tr
       then let st1 = closeUl (closePara st)
                c = doInline (trim (strDropPrefix "####" tr))
            in { html $= (++ "<h4>" ++ c ++ "</h4>\n") } st1
     else if strHasPrefix "###" tr
       then let st1 = closeUl (closePara st)
                c = doInline (trim (strDropPrefix "###" tr))
            in { html $= (++ "<h3>" ++ c ++ "</h3>\n") } st1
     else if strHasPrefix "##" tr
       then let st1 = closeUl (closePara st)
                c = doInline (trim (strDropPrefix "##" tr))
            in { html $= (++ "<h2>" ++ c ++ "</h2>\n") } st1
     else if strHasPrefix "#" tr
       then let st1 = closeUl (closePara st)
                c = doInline (trim (strDropPrefix "#" tr))
            in { html $= (++ "<h1>" ++ c ++ "</h1>\n") } st1
     else if strHasPrefix "- " tr || strHasPrefix "* " tr
       then let st1 = closePara st
                st2 = if st1.inList && st1.orderedList then closeUl st1 else st1
                st3 = if not st2.inList
                         then { html $= (++ "<ul>\n"), inList := True, orderedList := False } st2
                         else st2
                item = doInline (trim (substr 2 (length tr) tr))
            in { html $= (++ "<li>" ++ item ++ "</li>\n") } st3
     else let st1 = if not st.inPara
                       then { html $= (++ "<p>"), inPara := True } st
                       else { html $= (++ " ") } st
          in { html $= (++ doInline tr) } st1

-- | Convert a complete markdown string into HTML.
-- Processes all lines through the stateful parser, then closes any
-- remaining open tags (paragraphs, lists, code blocks).
export
parseMarkdown : String -> String
parseMarkdown content =
  let allLines = lines content
      final = foldl (flip doLine) initState allLines
      st1 = closeUl (closePara final)
      st2 = if st1.inCode then { html $= (++ "</code></pre>\n") } st1 else st1
  in st2.html

-- ============================================================================
-- Navigation Generator
-- ============================================================================
-- Generates the HTML for a tabbed navigation bar. Each page becomes a tab.
-- The currently active page gets a CSS class="active" for highlighting.

-- | Generate a single navigation list item for one page.
-- The active page receives class="active" for CSS styling.
-- The index page links to "/" while other pages link to "/<slug>.html".
genNavItem : String -> SitePage -> String
genNavItem activeSlug page =
  let cls = if activeSlug == page.slug then " class=\"active\"" else ""
      href = if page.slug == "index" then "index.html" else page.slug ++ ".html"
  in "      <li><a href=\"" ++ href ++ "\"" ++ cls ++ ">" ++ page.frontmatter.title ++ "</a></li>"

-- | Generate the complete navigation HTML for all pages.
-- Pages should be pre-sorted by navOrder before calling this function.
-- Output is a series of <li> elements suitable for embedding in a <ul>.
export
genNav : String -> List SitePage -> String
genNav activeSlug pages =
  unlines (map (genNavItem activeSlug) (filter (not . (.frontmatter.draft)) pages))

-- ============================================================================
-- Template Engine
-- ============================================================================
-- The template engine performs string-based placeholder substitution.
-- Supported placeholders:
--   {{title}}      - Page title from frontmatter
--   {{date}}       - Page date from frontmatter
--   {{content}}    - Rendered HTML content from markdown body
--   {{nav}}        - Generated navigation HTML (tab bar)
--   {{slug}}       - Page URL slug
--   {{tags}}       - Comma-separated tag list
--   {{body_class}} - CSS class "page-<slug>" for per-page styling

-- | Default minimal template used when no custom template is provided.
-- Produces a clean, readable HTML page with system-ui font and centered layout.
defTemplate : String
defTemplate = "<!DOCTYPE html>\n<html><head><meta charset=\"UTF-8\"><title>{{title}}</title><style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style></head><body><article><h1>{{title}}</h1><time>{{date}}</time>{{content}}</article></body></html>"

-- | Replace all occurrences of a needle substring within a haystack string.
-- Returns the haystack unchanged if the needle is empty (prevents infinite loops).
strReplace : String -> String -> String -> String
strReplace hay needle rep =
  if needle == "" then hay
  else go hay ""
  where
    go : String -> String -> String
    go str acc =
      if length str < length needle
         then acc ++ str
         else if strHasPrefix needle str
                 then go (substr (length needle) (length str) str) (acc ++ rep)
                 else go (substr 1 (length str) str) (acc ++ substr 0 1 str)

-- | Apply the default template to a page (backward-compatible).
-- Uses only {{title}}, {{date}}, and {{content}} placeholders.
export
applyTemplate : Frontmatter -> String -> String
applyTemplate fm htmlContent =
  let t1 = strReplace defTemplate "{{title}}" fm.title
      t2 = strReplace t1 "{{date}}" fm.date
      t3 = strReplace t2 "{{content}}" htmlContent
  in t3

-- | Apply a custom template string with full placeholder substitution.
-- This is the primary template function for multi-page site building.
-- Supports all placeholders: title, date, content, nav, slug, tags, body_class.
export
applyTemplateWith : String -> Frontmatter -> String -> String -> String -> String
applyTemplateWith tmpl fm htmlContent navHtml pageSlug =
  let tagsStr = concat (intersperse ", " fm.tags)
      t1 = strReplace tmpl "{{title}}" fm.title
      t2 = strReplace t1 "{{date}}" fm.date
      t3 = strReplace t2 "{{content}}" htmlContent
      t4 = strReplace t3 "{{nav}}" navHtml
      t5 = strReplace t4 "{{slug}}" pageSlug
      t6 = strReplace t5 "{{tags}}" tagsStr
      t7 = strReplace t6 "{{body_class}}" ("page-" ++ pageSlug)
  in t7

-- ============================================================================
-- File I/O Utilities
-- ============================================================================
-- File system operations for reading content directories, loading markdown
-- files, and writing generated HTML output. All operations use Idris 2's
-- IO monad with explicit error handling via Either.

-- | List all .md (markdown) files in a directory.
-- Uses System.Directory.listDir which handles directory traversal and
-- automatically filters out '.' and '..' entries.
-- Returns just the filenames, not full paths.
export
listMdFiles : String -> IO (List String)
listMdFiles dirPath = do
  result <- listDir dirPath
  case result of
    Left err => do
      putStrLn ("  Error: cannot open content directory '" ++ dirPath ++ "'")
      pure []
    Right entries =>
      pure (sort (filter (\f => strHasSuffix ".md" f) entries))

-- | Read a single markdown content file and produce a SitePage.
-- Parses frontmatter, converts markdown to HTML, and derives the URL slug
-- from the filename (unless overridden in frontmatter).
-- Returns Nothing if the file cannot be read, or if the page is a draft.
export
readContentFile : String -> String -> IO (Maybe SitePage)
readContentFile dirPath filename = do
  let fullPath = pathJoin dirPath filename
  result <- readFile fullPath
  case result of
    Left err => do
      putStrLn ("  Warning: cannot read '" ++ fullPath ++ "', skipping")
      pure Nothing
    Right content => do
      let (fm, body) = parseFrontmatter content
      -- Skip draft pages during site build
      if fm.draft
         then do putStrLn ("  Skipping draft: " ++ filename)
                 pure Nothing
         else do
           let htmlBody = parseMarkdown body
           -- Derive slug from frontmatter or filename
           let derivedSlug = if fm.slug /= ""
                               then fm.slug
                               else strStripSuffix ".md" filename
           -- Use filename as title if frontmatter title is empty
           let derivedTitle = if fm.title /= ""
                                then fm.title
                                else strStripSuffix ".md" filename
           let finalFm = { slug := derivedSlug, title := derivedTitle } fm
           pure (Just (MkSitePage derivedSlug filename finalFm htmlBody))

-- | Ensure a directory exists, creating it if necessary.
-- Silently succeeds if the directory already exists.
ensureDir : String -> IO ()
ensureDir path = do
  result <- createDir path
  case result of
    Right () => pure ()
    Left _ => pure ()  -- Directory may already exist; that is fine

-- | Write a rendered HTML page to the output directory.
-- The output filename is derived from the page slug: <slug>.html
writePage : String -> SitePage -> String -> IO ()
writePage outputDir page renderedHtml = do
  let outFile = pathJoin outputDir (page.slug ++ ".html")
  result <- writeFile outFile renderedHtml
  case result of
    Right () => putStrLn ("  Written: " ++ outFile)
    Left err => putStrLn ("  Error writing: " ++ outFile)

-- ============================================================================
-- Site Builder
-- ============================================================================
-- The site builder orchestrates the full pipeline: read all content files,
-- sort them by navigation order, generate navigation for each page, apply
-- templates, and write the output HTML files.

-- | Load all markdown pages from a content directory.
-- Reads each .md file, parses it into a SitePage, and collects the results.
-- Draft pages and unreadable files are silently excluded.
loadPages : String -> IO (List SitePage)
loadPages contentDir = do
  mdFiles <- listMdFiles contentDir
  pages <- traverse (readContentFile contentDir) mdFiles
  pure (mapMaybe id pages)

-- | Build a complete static site from a content directory.
-- Pipeline:
--   1. Load and parse all markdown content files
--   2. Sort pages by navOrder for consistent navigation
--   3. Load the custom template file
--   4. For each page: generate navigation, apply template, write output
--
-- Arguments:
--   contentDir  - Directory containing .md source files
--   outputDir   - Directory where .html files will be written
--   templateFile - Path to the HTML template file with {{placeholders}}
export
buildSite : String -> String -> String -> IO ()
buildSite contentDir outputDir templateFile = do
  putStrLn "Ddraig SSG - Building site..."
  putStrLn ("  Content:  " ++ contentDir)
  putStrLn ("  Output:   " ++ outputDir)
  putStrLn ("  Template: " ++ templateFile)
  putStrLn ""

  -- Load template
  tmplResult <- readFile templateFile
  tmpl <- case tmplResult of
    Left err => do
      putStrLn "  Warning: cannot read template file, using default template"
      pure defTemplate
    Right t => do
      putStrLn ("  Template loaded (" ++ show (length t) ++ " bytes)")
      pure t

  -- Load all content pages
  putStrLn "\n  Loading content..."
  rawPages <- loadPages contentDir
  let pages = sortPages rawPages
  putStrLn ("  Loaded " ++ show (length pages) ++ " pages")

  -- Ensure output directory exists
  ensureDir outputDir

  -- Build each page with navigation context
  putStrLn "\n  Generating pages..."
  traverse_ (buildOnePage tmpl pages) pages

  putStrLn ("\n  Site built: " ++ show (length pages) ++ " pages written to " ++ outputDir)
  where
    -- | Build and write a single page with full navigation context.
    buildOnePage : String -> List SitePage -> SitePage -> IO ()
    buildOnePage tmpl allPages page = do
      let nav = genNav page.slug allPages
      let rendered = applyTemplateWith tmpl page.frontmatter page.htmlContent nav page.slug
      writePage outputDir page rendered

-- ============================================================================
-- Tests (preserved from original)
-- ============================================================================

testMarkdown : IO ()
testMarkdown = do
  putStrLn "=== Test: Markdown ==="
  let md = "# Hello World\n\nThis is a **bold** test with *italic* text.\n\n- Item 1\n- Item 2\n\n```\ncode block\n```\n"
  putStrLn (parseMarkdown md)

testFrontmatter : IO ()
testFrontmatter = do
  putStrLn "=== Test: Frontmatter ==="
  let content = "---\ntitle: My Post\ndate: 2024-01-15\ntags: [idris, ssg]\ndraft: false\n---\n\nContent here\n"
  let (fm, body) = parseFrontmatter content
  putStrLn ("Title: " ++ fm.title)
  putStrLn ("Date: " ++ fm.date)
  putStrLn ("Tags: " ++ show fm.tags)
  putStrLn ("Draft: " ++ show fm.draft)
  putStrLn ("Slug: " ++ fm.slug)
  putStrLn ("NavOrder: " ++ show fm.navOrder)
  putStrLn ("Body: " ++ body)

testFull : IO ()
testFull = do
  putStrLn "=== Test: Full Pipeline ==="
  let content = "---\ntitle: Welcome\ndate: 2024-01-15\n---\n\n# Welcome\n\nThis is **Ddraig**, an Idris 2-powered SSG.\n\n- Dependently typed\n- Provably correct\n- Elegant\n"
  let (fm, body) = parseFrontmatter content
  let htmlOut = parseMarkdown body
  let output = applyTemplate fm htmlOut
  putStrLn output

-- | Test the navigation generation with a sample multi-page site.
testNav : IO ()
testNav = do
  putStrLn "=== Test: Navigation ==="
  let page1 = MkSitePage "index" "index.md"
        (MkFrontmatter "Home" "2026-01-01" [] False "default" "index" 1) "<p>Home content</p>"
  let page2 = MkSitePage "about" "about.md"
        (MkFrontmatter "About" "2026-01-01" [] False "default" "about" 2) "<p>About content</p>"
  let page3 = MkSitePage "contact" "contact.md"
        (MkFrontmatter "Contact" "2026-01-01" [] False "default" "contact" 3) "<p>Contact content</p>"
  let pages = [page1, page2, page3]
  putStrLn "Navigation for 'about' page:"
  putStrLn (genNav "about" pages)

-- ============================================================================
-- Main
-- ============================================================================
-- Command-line interface for ddraig-ssg.
--
-- Commands:
--   build <content-dir> <output-dir> <template-file>
--       Build a complete static site from markdown content files.
--
--   test-markdown       Run markdown parser test
--   test-frontmatter    Run frontmatter parser test
--   test-full           Run full pipeline test
--   test-nav            Run navigation generator test

main : IO ()
main = do
  args <- getArgs
  case args of
    [_, "build", contentDir, outputDir, templateFile] =>
      buildSite contentDir outputDir templateFile
    [_, "build", contentDir, outputDir] => do
      putStrLn "Warning: no template file specified, using default template"
      buildSite contentDir outputDir ""
    [_, "test-markdown"] => testMarkdown
    [_, "test-frontmatter"] => testFrontmatter
    [_, "test-full"] => testFull
    [_, "test-nav"] => testNav
    _ => do
      putStrLn "Ddraig SSG - Idris 2 powered static site generator"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn "  build <content-dir> <output-dir> <template-file>"
      putStrLn "      Build a complete static site from markdown content"
      putStrLn ""
      putStrLn "  test-markdown       Test markdown parser"
      putStrLn "  test-frontmatter    Test frontmatter parser"
      putStrLn "  test-full           Test full pipeline"
      putStrLn "  test-nav            Test navigation generator"
