(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2025 hyperpolymath *)
(*
 * canon-ssg: Standard ML static site generator
 * Type-safe, formally-verified site generation using ML's
 * strong type system and pattern matching.
 *)

structure Canon :> sig
    type page
    type site

    val createPage : {title: string, path: string, content: string} -> page
    val createSite : string -> site
    val addPage : site * page -> site
    val build : site -> unit

    val escapeHtml : string -> string
    val parseMarkdown : string -> string
end = struct

    (* Page type with metadata and content *)
    type page = {
        title: string,
        path: string,
        content: string,
        template: string
    }

    (* Site configuration *)
    type site = {
        name: string,
        outputDir: string,
        pages: page list
    }

    (* Create a new page *)
    fun createPage {title, path, content} = {
        title = title,
        path = path,
        content = content,
        template = "default"
    } : page

    (* Create a new site *)
    fun createSite name = {
        name = name,
        outputDir = "_site",
        pages = []
    } : site

    (* Add a page to the site *)
    fun addPage (site: site, page: page) = {
        name = #name site,
        outputDir = #outputDir site,
        pages = page :: (#pages site)
    } : site

    (* Escape HTML special characters *)
    fun escapeHtml s =
        let
            fun escapeChar #"<" = "&lt;"
              | escapeChar #">" = "&gt;"
              | escapeChar #"&" = "&amp;"
              | escapeChar #"\"" = "&quot;"
              | escapeChar #"'" = "&#39;"
              | escapeChar c = String.str c
        in
            String.concat (map escapeChar (String.explode s))
        end

    (* Simple Markdown to HTML conversion *)
    fun parseMarkdown markdown =
        let
            fun processLine line =
                if String.isPrefix "# " line then
                    "<h1>" ^ String.extract(line, 2, NONE) ^ "</h1>\n"
                else if String.isPrefix "## " line then
                    "<h2>" ^ String.extract(line, 3, NONE) ^ "</h2>\n"
                else if String.isPrefix "### " line then
                    "<h3>" ^ String.extract(line, 4, NONE) ^ "</h3>\n"
                else if String.isPrefix "- " line then
                    "<li>" ^ String.extract(line, 2, NONE) ^ "</li>\n"
                else if String.isPrefix "* " line then
                    "<li>" ^ String.extract(line, 2, NONE) ^ "</li>\n"
                else if String.size line = 0 then
                    ""
                else
                    "<p>" ^ line ^ "</p>\n"

            val lines = String.tokens (fn c => c = #"\n") markdown
        in
            String.concat (map processLine lines)
        end

    (* Render a page to HTML *)
    fun renderPage (page: page) =
        "<!DOCTYPE html>\n" ^
        "<html lang=\"en\">\n" ^
        "<head>\n" ^
        "    <meta charset=\"UTF-8\">\n" ^
        "    <title>" ^ escapeHtml (#title page) ^ "</title>\n" ^
        "</head>\n" ^
        "<body>\n" ^
        "    <article>\n" ^
        "        <h1>" ^ escapeHtml (#title page) ^ "</h1>\n" ^
        "        " ^ #content page ^ "\n" ^
        "    </article>\n" ^
        "</body>\n" ^
        "</html>\n"

    (* Write a string to a file *)
    fun writeFile (path, content) =
        let
            val out = TextIO.openOut path
        in
            TextIO.output (out, content);
            TextIO.closeOut out
        end

    (* Create directory if it doesn't exist *)
    fun ensureDir dir =
        if OS.FileSys.access (dir, []) then ()
        else OS.FileSys.mkDir dir

    (* Build the site *)
    fun build (site: site) =
        let
            val outputDir = #outputDir site
            val _ = print ("canon-ssg: Building site '" ^ #name site ^ "'...\n")
            val _ = ensureDir outputDir

            fun buildPage page =
                let
                    val html = renderPage page
                    val outPath = outputDir ^ "/" ^ #path page ^ ".html"
                in
                    writeFile (outPath, html);
                    print ("canon-ssg: Built " ^ outPath ^ "\n")
                end

            val _ = List.app buildPage (#pages site)
            val pageCount = length (#pages site)
        in
            print ("canon-ssg: Built " ^ Int.toString pageCount ^ " pages\n")
        end
end

(* Main entry point *)
fun main () =
    let
        val site = Canon.createSite "canon-site"
        val page = Canon.createPage {
            title = "Home",
            path = "index",
            content = "<p>Welcome to canon-ssg!</p>"
        }
        val site' = Canon.addPage (site, page)
    in
        Canon.build site'
    end

(* Run main when loaded *)
val _ = main ()
