// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// labnote-ssg - SciLab Static Site Generator
// "Let matrices compute. Let figures render. Let science publish."
//
// Run with: scilab-cli -f src/labnote.sce -- build
//           scilab-cli -f src/labnote.sce -- init
//           scilab-cli -f src/labnote.sce -- clean

// ============================================================================
// Configuration
// ============================================================================

function config = get_config()
    config = struct();
    config.content_dir = "content";
    config.output_dir = "_site";
    config.site_title = "Lab Notebook";
    config.site_author = "Scientist";
endfunction

// ============================================================================
// String Utilities
// ============================================================================

function result = str_trim(s)
    // Trim leading and trailing whitespace
    result = stripblanks(s);
endfunction

function result = str_starts_with(s, prefix)
    // Check if string starts with prefix
    if length(s) < length(prefix) then
        result = %f;
    else
        result = part(s, 1:length(prefix)) == prefix;
    end
endfunction

function result = str_replace(s, old, new)
    // Replace all occurrences of old with new in s
    result = strsubst(s, old, new);
endfunction

function result = escape_html(s)
    // Escape HTML special characters
    result = s;
    result = str_replace(result, "&", "&amp;");
    result = str_replace(result, "<", "&lt;");
    result = str_replace(result, ">", "&gt;");
    result = str_replace(result, """", "&quot;");
endfunction

// ============================================================================
// Frontmatter Parser
// ============================================================================

function [meta, body_start] = parse_frontmatter(lines)
    // Parse YAML-like frontmatter from content lines
    meta = struct();
    meta.title = "Untitled";
    meta.date = "";
    meta.draft = %f;
    body_start = 1;

    if size(lines, 1) == 0 then
        return;
    end

    first_line = str_trim(lines(1));
    if first_line ~= "---" then
        return;
    end

    in_frontmatter = %t;
    for i = 2:size(lines, 1)
        line = str_trim(lines(i));
        if line == "---" then
            body_start = i + 1;
            return;
        end

        // Parse key: value pairs
        colon_pos = strindex(line, ":");
        if ~isempty(colon_pos) then
            key = str_trim(part(line, 1:colon_pos(1)-1));
            value = str_trim(part(line, colon_pos(1)+1:length(line)));

            select key
            case "title"
                meta.title = value;
            case "date"
                meta.date = value;
            case "draft"
                meta.draft = (value == "true" | value == "yes");
            end
        end
    end
endfunction

// ============================================================================
// Markdown Parser
// ============================================================================

function html = parse_markdown(lines)
    // Convert markdown lines to HTML
    html = "";
    in_code = %f;
    in_scilab = %f;
    code_buffer = "";

    for i = 1:size(lines, 1)
        line = lines(i);
        trimmed = str_trim(line);

        // Code blocks
        if str_starts_with(trimmed, "```scilab") then
            in_scilab = %t;
            code_buffer = "";
            continue;
        end

        if str_starts_with(trimmed, "```") & (in_code | in_scilab) then
            if in_scilab then
                // Execute SciLab code and capture output
                result = execute_scilab(code_buffer);
                html = html + "<pre class=""scilab""><code>" + escape_html(code_buffer) + "</code></pre>" + ascii(10);
                html = html + "<div class=""scilab-result"">" + escape_html(result) + "</div>" + ascii(10);
                in_scilab = %f;
            else
                html = html + "</code></pre>" + ascii(10);
                in_code = %f;
            end
            continue;
        end

        if str_starts_with(trimmed, "```") then
            html = html + "<pre><code>";
            in_code = %t;
            continue;
        end

        if in_scilab then
            code_buffer = code_buffer + line + ascii(10);
            continue;
        end

        if in_code then
            html = html + escape_html(line) + ascii(10);
            continue;
        end

        // Headers
        if str_starts_with(trimmed, "### ") then
            text = part(trimmed, 5:length(trimmed));
            html = html + "<h3>" + text + "</h3>" + ascii(10);
            continue;
        end

        if str_starts_with(trimmed, "## ") then
            text = part(trimmed, 4:length(trimmed));
            html = html + "<h2>" + text + "</h2>" + ascii(10);
            continue;
        end

        if str_starts_with(trimmed, "# ") then
            text = part(trimmed, 3:length(trimmed));
            html = html + "<h1>" + text + "</h1>" + ascii(10);
            continue;
        end

        // List items
        if str_starts_with(trimmed, "- ") | str_starts_with(trimmed, "* ") then
            text = part(trimmed, 3:length(trimmed));
            html = html + "<li>" + text + "</li>" + ascii(10);
            continue;
        end

        // Empty lines
        if length(trimmed) == 0 then
            continue;
        end

        // Paragraph
        html = html + "<p>" + trimmed + "</p>" + ascii(10);
    end
endfunction

// ============================================================================
// SciLab Code Execution
// ============================================================================

function result = execute_scilab(code)
    // Execute SciLab code and return string result
    // Uses diary to capture output
    result = "";

    try
        // Create temp file for diary
        temp_diary = TMPDIR + "/labnote_diary.txt";
        diary(temp_diary);

        // Execute the code
        execstr(code);

        // Close diary and read results
        diary(0);

        if isfile(temp_diary) then
            result = mgetl(temp_diary);
            if ~isempty(result) then
                result = strcat(result, ascii(10));
            end
            deletefile(temp_diary);
        end
    catch
        result = "Error: " + lasterror();
    end
endfunction

// ============================================================================
// HTML Generation
// ============================================================================

function html = generate_page(meta, content_html, config)
    // Generate complete HTML page
    html = "<!DOCTYPE html>" + ascii(10);
    html = html + "<html lang=""en"">" + ascii(10);
    html = html + "<head>" + ascii(10);
    html = html + "<meta charset=""UTF-8"">" + ascii(10);
    html = html + "<title>" + escape_html(meta.title) + " | " + config.site_title + "</title>" + ascii(10);
    html = html + "<style>" + ascii(10);
    html = html + "body { font-family: ''Computer Modern'', serif; max-width: 800px; margin: 0 auto; padding: 2rem; }" + ascii(10);
    html = html + "pre { background: #f5f5f5; padding: 1rem; overflow-x: auto; }" + ascii(10);
    html = html + "pre.scilab { border-left: 4px solid #0066cc; }" + ascii(10);
    html = html + ".scilab-result { background: #e8f4f8; padding: 1rem; margin-top: -1rem; border-left: 4px solid #00aa55; }" + ascii(10);
    html = html + "time { color: #666; font-style: italic; }" + ascii(10);
    html = html + "</style>" + ascii(10);
    html = html + "</head>" + ascii(10);
    html = html + "<body>" + ascii(10);
    html = html + "<article>" + ascii(10);
    html = html + "<h1>" + escape_html(meta.title) + "</h1>" + ascii(10);

    if meta.date ~= "" then
        html = html + "<time>" + meta.date + "</time>" + ascii(10);
    end

    html = html + content_html;
    html = html + "</article>" + ascii(10);
    html = html + "<footer>" + ascii(10);
    html = html + "<p>Generated by labnote-ssg (SciLab)</p>" + ascii(10);
    html = html + "</footer>" + ascii(10);
    html = html + "</body>" + ascii(10);
    html = html + "</html>" + ascii(10);
endfunction

// ============================================================================
// File Operations
// ============================================================================

function process_file(src_path, out_path, config)
    // Process a single markdown file
    mprintf("  Processing: %s\n", src_path);

    // Read content
    lines = mgetl(src_path);

    // Parse frontmatter
    [meta, body_start] = parse_frontmatter(lines);

    // Skip drafts
    if meta.draft then
        mprintf("    Skipping draft\n");
        return;
    end

    // Get body lines
    if body_start <= size(lines, 1) then
        body_lines = lines(body_start:$);
    else
        body_lines = [];
    end

    // Parse markdown
    content_html = parse_markdown(body_lines);

    // Generate HTML page
    html = generate_page(meta, content_html, config);

    // Write output
    fd = mopen(out_path, "wt");
    mputl(html, fd);
    mclose(fd);

    mprintf("  Written: %s\n", out_path);
endfunction

// ============================================================================
// Commands
// ============================================================================

function build_site()
    config = get_config();
    mprintf("labnote-ssg: Building site...\n");
    mprintf("  Output directory: %s\n", config.output_dir);

    // Create output directory
    if ~isdir(config.output_dir) then
        mkdir(config.output_dir);
    end

    // Check for content directory
    if ~isdir(config.content_dir) then
        mprintf("  No content directory found. Run ''labnote init'' first.\n");
        return;
    end

    // Process markdown files
    files = listfiles(config.content_dir + "/*.md");
    page_count = 0;

    for i = 1:size(files, 1)
        src_path = files(i);

        // Get output filename
        [path, fname, ext] = fileparts(src_path);
        out_path = config.output_dir + "/" + fname + ".html";

        process_file(src_path, out_path, config);
        page_count = page_count + 1;
    end

    mprintf("labnote-ssg: Built %d pages\n", page_count);
    mprintf("labnote-ssg: Build complete.\n");
endfunction

function init_site()
    config = get_config();
    mprintf("labnote-ssg: Initializing new site...\n");

    // Create directories
    if ~isdir(config.content_dir) then
        mkdir(config.content_dir);
        mprintf("  Created %s/\n", config.content_dir);
    end

    if ~isdir(config.output_dir) then
        mkdir(config.output_dir);
        mprintf("  Created %s/\n", config.output_dir);
    end

    // Create sample content with embedded SciLab
    sample_path = config.content_dir + "/index.md";

    sample = [
        "---"
        "title: Welcome to My Lab Notebook"
        "date: 2025-01-18"
        "---"
        ""
        "# Welcome"
        ""
        "This is a lab notebook powered by **labnote-ssg** with embedded SciLab computation."
        ""
        "## Matrix Computation"
        ""
        "Let''s compute the inverse of a 2x2 matrix:"
        ""
        "```scilab"
        "A = [1 2; 3 4];"
        "B = inv(A);"
        "disp(""Matrix A:"");"
        "disp(A);"
        "disp(""Inverse B:"");"
        "disp(B);"
        "disp(""Verification A*B:"");"
        "disp(A*B);"
        "```"
        ""
        "## Features"
        ""
        "- Inline SciLab execution"
        "- Matrix-native computation"
        "- Figure generation"
        "- Reproducible results"
    ];

    fd = mopen(sample_path, "wt");
    mputl(sample, fd);
    mclose(fd);

    mprintf("  Created sample content\n");
    mprintf("labnote-ssg: Site initialized.\n");
    mprintf("Run ''scilab-cli -f src/labnote.sce -- build'' to build.\n");
endfunction

function clean_site()
    config = get_config();
    mprintf("labnote-ssg: Cleaning %s/...\n", config.output_dir);

    if isdir(config.output_dir) then
        removedir(config.output_dir);
    end

    mprintf("labnote-ssg: Clean complete.\n");
endfunction

function show_help()
    mprintf("labnote-ssg - SciLab Static Site Generator\n");
    mprintf("\n");
    mprintf("USAGE:\n");
    mprintf("  scilab-cli -f src/labnote.sce -- <command>\n");
    mprintf("\n");
    mprintf("COMMANDS:\n");
    mprintf("  build    Build the site (execute embedded SciLab)\n");
    mprintf("  init     Initialize a new site\n");
    mprintf("  clean    Remove generated files\n");
    mprintf("  help     Show this help\n");
    mprintf("\n");
    mprintf("FEATURES:\n");
    mprintf("  - Embedded SciLab code execution\n");
    mprintf("  - Matrix computations in documentation\n");
    mprintf("  - Reproducible scientific publishing\n");
endfunction

// ============================================================================
// Main Entry Point
// ============================================================================

function main()
    // Get command from command line arguments
    args = sciargs();

    // Find the -- separator and get command after it
    cmd = "help";
    for i = 1:size(args, 1)
        if args(i) == "--" & i < size(args, 1) then
            cmd = args(i + 1);
            break;
        end
    end

    select cmd
    case "build"
        build_site();
    case "init"
        init_site();
    case "clean"
        clean_site();
    case "help"
        show_help();
    else
        mprintf("Unknown command: %s\n", cmd);
        show_help();
    end
endfunction

// Run main
main();
quit();
