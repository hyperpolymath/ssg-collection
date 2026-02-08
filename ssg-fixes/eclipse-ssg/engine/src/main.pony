// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// eclipse-ssg: Pony static site generator
// Actor-based, memory-safe site generation with reference capabilities
// "Obscure your old site with something better"

use "files"
use "collections"

class val PageMeta
  """Page metadata with immutable values."""
  let title: String val
  let path: String val
  let template: String val

  new val create(title': String val, path': String val,
                 template': String val = "default") =>
    title = title'
    path = path'
    template = template'

class val Page
  """A page with metadata and content."""
  let meta: PageMeta val
  let content: String val

  new val create(meta': PageMeta val, content': String val) =>
    meta = meta'
    content = content'

  fun render(): String val =>
    """Render page to HTML."""
    let escaped_title = _escape_html(meta.title)

    "<!DOCTYPE html>\n" +
    "<html lang=\"en\">\n" +
    "<head>\n" +
    "    <meta charset=\"UTF-8\">\n" +
    "    <title>" + escaped_title + "</title>\n" +
    "</head>\n" +
    "<body>\n" +
    "    <article>\n" +
    "        <h1>" + escaped_title + "</h1>\n" +
    "        " + content + "\n" +
    "    </article>\n" +
    "</body>\n" +
    "</html>\n"

  fun _escape_html(text: String val): String val =>
    """Escape HTML special characters."""
    var result: String iso = recover String end
    for char in text.values() do
      match char
      | '<' => result.append("&lt;")
      | '>' => result.append("&gt;")
      | '&' => result.append("&amp;")
      | '"' => result.append("&quot;")
      | '\'' => result.append("&#39;")
      else
        result.push(char)
      end
    end
    consume result

actor Site
  """Site actor that manages pages and builds the output."""
  let _env: Env
  let _name: String val
  let _output_dir: String val
  var _pages: Array[Page val] ref

  new create(env: Env, name: String val, output_dir: String val = "_site") =>
    _env = env
    _name = name
    _output_dir = output_dir
    _pages = Array[Page val]

  be add_page(page: Page val) =>
    """Add a page to the site."""
    _pages.push(page)

  be build() =>
    """Build the site, writing all pages to output directory."""
    _env.out.print("eclipse-ssg: Building site '" + _name + "'...")

    // Create output directory
    let auth = _env.root as AmbientAuth
    let path = FilePath(FileAuth(auth), _output_dir)

    try
      if not path.exists() then
        path.mkdir()
      end

      for page in _pages.values() do
        _build_page(page, auth)
      end

      _env.out.print("eclipse-ssg: Built " + _pages.size().string() + " pages")
    else
      _env.out.print("eclipse-ssg: Error building site")
    end

  fun _build_page(page: Page val, auth: AmbientAuth) =>
    """Build a single page."""
    let html = page.render()
    let out_path = _output_dir + "/" + page.meta.path + ".html"

    try
      let file_path = FilePath(FileAuth(auth), out_path)
      let file = CreateFile(file_path) as File

      file.write(html)
      file.dispose()

      _env.out.print("eclipse-ssg: Built " + out_path)
    else
      _env.out.print("eclipse-ssg: Error writing " + out_path)
    end

primitive Markdown
  """Simple Markdown parser."""
  fun parse(markdown: String val): String val =>
    """Convert Markdown to HTML."""
    var result: String iso = recover String end
    let lines = markdown.split("\n")

    for line in lines.values() do
      if line.size() == 0 then
        continue
      end

      if line.at("# ", 0) then
        result.append("<h1>")
        result.append(line.substring(2))
        result.append("</h1>\n")
      elseif line.at("## ", 0) then
        result.append("<h2>")
        result.append(line.substring(3))
        result.append("</h2>\n")
      elseif line.at("### ", 0) then
        result.append("<h3>")
        result.append(line.substring(4))
        result.append("</h3>\n")
      elseif line.at("- ", 0) or line.at("* ", 0) then
        result.append("<li>")
        result.append(line.substring(2))
        result.append("</li>\n")
      else
        result.append("<p>")
        result.append(line)
        result.append("</p>\n")
      end
    end

    consume result

actor Main
  """Main entry point for eclipse-ssg."""
  new create(env: Env) =>
    env.out.print("eclipse-ssg: Pony-powered static site generation")

    // Create site
    let site = Site(env, "eclipse-site")

    // Add example page
    let meta = PageMeta("Welcome to eclipse-ssg", "index")
    let page = Page(meta, "<p>Actor-based static site generation with Pony.</p>")
    site.add_page(page)

    // Build the site
    site.build()
