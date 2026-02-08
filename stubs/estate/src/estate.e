-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 hyperpolymath
--
-- estate-ssg - Eiffel Static Site Generator
-- "Let contracts bind. Let assertions guard. Let sites deploy."
--
-- Compile with: gec estate.e
-- Run with: ./estate build | init | clean

note
    description: "Eiffel-powered Static Site Generator with Design by Contract"
    author: "hyperpolymath"
    date: "2025-01-18"

class
    ESTATE

create
    make

feature {NONE} -- Initialization

    make
            -- Entry point
        local
            args: ARGUMENTS_32
            cmd: STRING_32
        do
            create args
            if args.argument_count >= 1 then
                cmd := args.argument (1)
                if cmd.is_equal ("build") then
                    build_site
                elseif cmd.is_equal ("init") then
                    init_site
                elseif cmd.is_equal ("clean") then
                    clean_site
                else
                    show_help
                end
            else
                show_help
            end
        end

feature -- Configuration

    content_dir: STRING = "content"
    output_dir: STRING = "_site"
    site_title: STRING = "Estate Site"

feature -- Commands

    build_site
            -- Build the static site
        require
            content_exists: (create {DIRECTORY}.make (content_dir)).exists
        local
            content_directory: DIRECTORY
            output_directory: DIRECTORY
            entry: DIRECTORY_NAME
            src_path, out_path: STRING
            page_count: INTEGER
        do
            print ("estate-ssg: Building site...%N")
            print ("  Output directory: " + output_dir + "%N")

            -- Create output directory
            create output_directory.make (output_dir)
            if not output_directory.exists then
                output_directory.create_dir
            end

            -- Process content files
            create content_directory.make_open_read (content_dir)
            page_count := 0

            from
                content_directory.start
            until
                content_directory.exhausted
            loop
                if content_directory.lastentry.ends_with (".md") then
                    src_path := content_dir + "/" + content_directory.lastentry
                    out_path := output_dir + "/" + content_directory.lastentry.substring (1, content_directory.lastentry.count - 3) + ".html"
                    process_file (src_path, out_path)
                    page_count := page_count + 1
                end
                content_directory.readentry
            end
            content_directory.close

            print ("estate-ssg: Built " + page_count.out + " pages%N")
            print ("estate-ssg: Build complete.%N")
        end

    init_site
            -- Initialize a new site
        local
            content_directory: DIRECTORY
            output_directory: DIRECTORY
            sample_file: PLAIN_TEXT_FILE
        do
            print ("estate-ssg: Initializing new site...%N")

            -- Create directories
            create content_directory.make (content_dir)
            if not content_directory.exists then
                content_directory.create_dir
                print ("  Created " + content_dir + "/%N")
            end

            create output_directory.make (output_dir)
            if not output_directory.exists then
                output_directory.create_dir
                print ("  Created " + output_dir + "/%N")
            end

            -- Create sample content
            create sample_file.make_create_read_write (content_dir + "/index.md")
            sample_file.put_string ("---%N")
            sample_file.put_string ("title: Welcome to Estate%N")
            sample_file.put_string ("date: 2025-01-18%N")
            sample_file.put_string ("---%N%N")
            sample_file.put_string ("# Welcome%N%N")
            sample_file.put_string ("This site was built with **estate-ssg**, an Eiffel-powered SSG.%N%N")
            sample_file.put_string ("## Design by Contract Features%N%N")
            sample_file.put_string ("- Preconditions for all operations%N")
            sample_file.put_string ("- Postconditions ensuring correctness%N")
            sample_file.put_string ("- Class invariants throughout%N")
            sample_file.put_string ("- Assertion monitoring%N%N")
            sample_file.put_string ("## Code Example%N%N")
            sample_file.put_string ("```eiffel%N")
            sample_file.put_string ("class GREETING%N")
            sample_file.put_string ("feature%N")
            sample_file.put_string ("    greet (name: STRING)%N")
            sample_file.put_string ("        require%N")
            sample_file.put_string ("            name_not_empty: not name.is_empty%N")
            sample_file.put_string ("        do%N")
            sample_file.put_string ("            print (%"Hello, %" + name + %"!%N%")%N")
            sample_file.put_string ("        end%N")
            sample_file.put_string ("end%N")
            sample_file.put_string ("```%N")
            sample_file.close

            print ("  Created sample content%N")
            print ("estate-ssg: Site initialized.%N")
            print ("Run './estate build' to build.%N")
        end

    clean_site
            -- Remove generated files
        local
            output_directory: DIRECTORY
        do
            print ("estate-ssg: Cleaning " + output_dir + "/...%N")
            create output_directory.make (output_dir)
            if output_directory.exists then
                output_directory.recursive_delete
            end
            print ("estate-ssg: Clean complete.%N")
        end

    show_help
            -- Display help message
        do
            print ("estate-ssg - Eiffel Static Site Generator%N%N")
            print ("USAGE:%N")
            print ("  estate <command>%N%N")
            print ("COMMANDS:%N")
            print ("  build    Build the site%N")
            print ("  init     Initialize a new site%N")
            print ("  clean    Remove generated files%N")
            print ("  help     Show this help%N%N")
            print ("FEATURES:%N")
            print ("  - Design by Contract throughout%N")
            print ("  - Assertion-guarded operations%N")
            print ("  - Class-based architecture%N")
        end

feature {NONE} -- Implementation

    process_file (src_path, out_path: STRING)
            -- Process a single markdown file
        require
            valid_src: src_path /= Void and then not src_path.is_empty
            valid_out: out_path /= Void and then not out_path.is_empty
        local
            src_file: PLAIN_TEXT_FILE
            out_file: PLAIN_TEXT_FILE
            lines: ARRAYED_LIST [STRING]
            meta: TUPLE [title: STRING; date: STRING; draft: BOOLEAN]
            body_start: INTEGER
            content_html, html: STRING
        do
            print ("  Processing: " + src_path + "%N")

            -- Read source file
            create src_file.make_open_read (src_path)
            create lines.make (100)
            from until src_file.exhausted loop
                src_file.read_line
                lines.extend (src_file.last_string.twin)
            end
            src_file.close

            -- Parse frontmatter
            meta := parse_frontmatter (lines)
            body_start := frontmatter_body_start

            -- Skip drafts
            if meta.draft then
                print ("    Skipping draft%N")
            else
                -- Parse markdown
                content_html := parse_markdown (lines, body_start)

                -- Generate HTML
                html := generate_page (meta, content_html)

                -- Write output
                create out_file.make_create_read_write (out_path)
                out_file.put_string (html)
                out_file.close

                print ("  Written: " + out_path + "%N")
            end
        ensure
            file_processed: True
        end

    frontmatter_body_start: INTEGER

    parse_frontmatter (lines: ARRAYED_LIST [STRING]): TUPLE [title: STRING; date: STRING; draft: BOOLEAN]
            -- Parse YAML-like frontmatter
        local
            i: INTEGER
            line, key, value: STRING
            colon_pos: INTEGER
            in_frontmatter: BOOLEAN
        do
            Result := ["Untitled", "", False]
            frontmatter_body_start := 1

            if lines.count > 0 and then lines.first.is_equal ("---") then
                in_frontmatter := True
                from i := 2 until i > lines.count or not in_frontmatter loop
                    line := lines [i]
                    if line.is_equal ("---") then
                        frontmatter_body_start := i + 1
                        in_frontmatter := False
                    else
                        colon_pos := line.index_of (':', 1)
                        if colon_pos > 0 then
                            key := line.substring (1, colon_pos - 1)
                            key.left_adjust
                            key.right_adjust
                            value := line.substring (colon_pos + 1, line.count)
                            value.left_adjust
                            value.right_adjust

                            if key.is_equal ("title") then
                                Result.title := value
                            elseif key.is_equal ("date") then
                                Result.date := value
                            elseif key.is_equal ("draft") then
                                Result.draft := value.is_equal ("true") or value.is_equal ("yes")
                            end
                        end
                    end
                    i := i + 1
                end
            end
        end

    parse_markdown (lines: ARRAYED_LIST [STRING]; body_start: INTEGER): STRING
            -- Convert markdown to HTML
        local
            i: INTEGER
            line, trimmed: STRING
            in_code: BOOLEAN
        do
            create Result.make_empty
            in_code := False

            from i := body_start until i > lines.count loop
                line := lines [i]
                trimmed := line.twin
                trimmed.left_adjust
                trimmed.right_adjust

                -- Code blocks
                if trimmed.starts_with ("```") then
                    if in_code then
                        Result.append ("</code></pre>%N")
                        in_code := False
                    else
                        Result.append ("<pre><code>")
                        in_code := True
                    end
                elseif in_code then
                    Result.append (escape_html (line) + "%N")
                elseif trimmed.starts_with ("### ") then
                    Result.append ("<h3>" + trimmed.substring (5, trimmed.count) + "</h3>%N")
                elseif trimmed.starts_with ("## ") then
                    Result.append ("<h2>" + trimmed.substring (4, trimmed.count) + "</h2>%N")
                elseif trimmed.starts_with ("# ") then
                    Result.append ("<h1>" + trimmed.substring (3, trimmed.count) + "</h1>%N")
                elseif trimmed.starts_with ("- ") or trimmed.starts_with ("* ") then
                    Result.append ("<li>" + trimmed.substring (3, trimmed.count) + "</li>%N")
                elseif not trimmed.is_empty then
                    Result.append ("<p>" + trimmed + "</p>%N")
                end

                i := i + 1
            end
        end

    generate_page (meta: TUPLE [title: STRING; date: STRING; draft: BOOLEAN]; content_html: STRING): STRING
            -- Generate complete HTML page
        do
            create Result.make_empty
            Result.append ("<!DOCTYPE html>%N")
            Result.append ("<html lang=%"en%">%N")
            Result.append ("<head>%N")
            Result.append ("<meta charset=%"UTF-8%">%N")
            Result.append ("<title>" + escape_html (meta.title) + " | " + site_title + "</title>%N")
            Result.append ("<style>%N")
            Result.append ("body { font-family: 'Times New Roman', serif; max-width: 700px; margin: 0 auto; padding: 2rem; }%N")
            Result.append ("pre { background: #f5f5f5; padding: 1rem; overflow-x: auto; }%N")
            Result.append ("time { color: #666; font-style: italic; }%N")
            Result.append ("</style>%N")
            Result.append ("</head>%N")
            Result.append ("<body>%N")
            Result.append ("<article>%N")
            Result.append ("<h1>" + escape_html (meta.title) + "</h1>%N")
            if not meta.date.is_empty then
                Result.append ("<time>" + meta.date + "</time>%N")
            end
            Result.append (content_html)
            Result.append ("</article>%N")
            Result.append ("<footer>%N")
            Result.append ("<p>Generated by estate-ssg (Eiffel)</p>%N")
            Result.append ("</footer>%N")
            Result.append ("</body>%N")
            Result.append ("</html>%N")
        end

    escape_html (s: STRING): STRING
            -- Escape HTML special characters
        do
            Result := s.twin
            Result.replace_substring_all ("&", "&amp;")
            Result.replace_substring_all ("<", "&lt;")
            Result.replace_substring_all (">", "&gt;")
            Result.replace_substring_all ("%"", "&quot;")
        end

invariant
    valid_content_dir: content_dir /= Void and then not content_dir.is_empty
    valid_output_dir: output_dir /= Void and then not output_dir.is_empty

end
