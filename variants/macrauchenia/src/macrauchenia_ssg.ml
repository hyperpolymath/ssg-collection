(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell *)
(*
   macrauchenia_ssg.ml - OCaml-powered static site generator

   "Macrauchenia" - Pattern matching elegance in site generation

   OCaml's powerful type system and pattern matching make parsing
   frontmatter and markdown elegant and safe.
*)

(* ============================================================================
   Types
   ============================================================================ *)

type frontmatter = {
  title: string;
  date: string;
  tags: string list;
  draft: bool;
  template: string;
}

type site_config = {
  content_dir: string;
  output_dir: string;
  template_dir: string;
  static_dir: string;
  site_title: string;
}

type parse_result = {
  frontmatter: frontmatter;
  body: string;
}

(* ============================================================================
   Configuration
   ============================================================================ *)

let empty_frontmatter = {
  title = "";
  date = "";
  tags = [];
  draft = false;
  template = "default";
}

let default_config = {
  content_dir = "content";
  output_dir = "public";
  template_dir = "templates";
  static_dir = "static";
  site_title = "My Site";
}

(* ============================================================================
   String Utilities
   ============================================================================ *)

let trim s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\n' || s.[!i] = '\r') do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '\n' || s.[!j] = '\r') do
    decr j
  done;
  if !i > !j then "" else String.sub s !i (!j - !i + 1)

let starts_with prefix s =
  let plen = String.length prefix in
  let slen = String.length s in
  plen <= slen && String.sub s 0 plen = prefix

let drop_prefix prefix s =
  if starts_with prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else s

let split_on_char sep s =
  let rec aux acc i =
    match String.index_from_opt s i sep with
    | Some j ->
        aux (String.sub s i (j - i) :: acc) (j + 1)
    | None ->
        List.rev (String.sub s i (String.length s - i) :: acc)
  in
  if String.length s = 0 then [""] else aux [] 0

let split_lines s = split_on_char '\n' s

let join_lines lines = String.concat "\n" lines

let escape_html s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (function
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(* ============================================================================
   Frontmatter Parser
   ============================================================================ *)

let parse_fm_line line fm =
  let trimmed = trim line in
  match String.index_opt trimmed ':' with
  | None -> fm
  | Some i ->
      let key = trim (String.sub trimmed 0 i) in
      let value = trim (String.sub trimmed (i + 1) (String.length trimmed - i - 1)) in
      match key with
      | "title" -> { fm with title = value }
      | "date" -> { fm with date = value }
      | "template" -> { fm with template = value }
      | "draft" -> { fm with draft = (value = "true" || value = "yes") }
      | "tags" ->
          let tags = List.map trim (split_on_char ',' value) in
          { fm with tags }
      | _ -> fm

let parse_frontmatter content =
  let lines = split_lines content in
  match lines with
  | first :: rest when trim first = "---" ->
      let rec find_end acc = function
        | [] -> { frontmatter = empty_frontmatter; body = "" }
        | line :: rest when trim line = "---" ->
            let fm = List.fold_left (fun fm l -> parse_fm_line l fm) empty_frontmatter (List.rev acc) in
            { frontmatter = fm; body = join_lines rest }
        | line :: rest -> find_end (line :: acc) rest
      in
      find_end [] rest
  | _ -> { frontmatter = empty_frontmatter; body = content }

(* ============================================================================
   Markdown Parser
   ============================================================================ *)

type parser_state = {
  html: Buffer.t;
  in_para: bool;
  in_code: bool;
  in_list: bool;
}

let init_state () = {
  html = Buffer.create 1024;
  in_para = false;
  in_code = false;
  in_list = false;
}

let close_para state =
  if state.in_para then begin
    Buffer.add_string state.html "</p>\n";
    { state with in_para = false }
  end else state

let close_list state =
  if state.in_list then begin
    Buffer.add_string state.html "</ul>\n";
    { state with in_list = false }
  end else state

let process_inline text =
  let buf = Buffer.create (String.length text * 2) in
  let len = String.length text in
  let rec go i bold italic =
    if i >= len then ()
    else if i + 1 < len && text.[i] = '*' && text.[i + 1] = '*' then begin
      if bold then Buffer.add_string buf "</strong>"
      else Buffer.add_string buf "<strong>";
      go (i + 2) (not bold) italic
    end
    else if text.[i] = '*' then begin
      if italic then Buffer.add_string buf "</em>"
      else Buffer.add_string buf "<em>";
      go (i + 1) bold (not italic)
    end
    else if text.[i] = '`' then begin
      let j = ref (i + 1) in
      while !j < len && text.[!j] <> '`' do incr j done;
      if !j < len then begin
        Buffer.add_string buf "<code>";
        Buffer.add_string buf (String.sub text (i + 1) (!j - i - 1));
        Buffer.add_string buf "</code>";
        go (!j + 1) bold italic
      end else begin
        Buffer.add_char buf text.[i];
        go (i + 1) bold italic
      end
    end
    else begin
      Buffer.add_char buf text.[i];
      go (i + 1) bold italic
    end
  in
  go 0 false false;
  Buffer.contents buf

let process_line line state =
  let trimmed = trim line in

  (* Code fence toggle *)
  if starts_with "```" trimmed then
    if state.in_code then begin
      Buffer.add_string state.html "</code></pre>\n";
      { state with in_code = false }
    end else begin
      let state = close_list (close_para state) in
      Buffer.add_string state.html "<pre><code>";
      { state with in_code = true }
    end

  (* Inside code block *)
  else if state.in_code then begin
    Buffer.add_string state.html (escape_html line);
    Buffer.add_char state.html '\n';
    state
  end

  (* Empty line *)
  else if trimmed = "" then
    close_list (close_para state)

  (* Headers - check longest first *)
  else if starts_with "###### " trimmed then begin
    let state = close_list (close_para state) in
    let content = process_inline (trim (drop_prefix "###### " trimmed)) in
    Buffer.add_string state.html (Printf.sprintf "<h6>%s</h6>\n" content);
    state
  end
  else if starts_with "##### " trimmed then begin
    let state = close_list (close_para state) in
    let content = process_inline (trim (drop_prefix "##### " trimmed)) in
    Buffer.add_string state.html (Printf.sprintf "<h5>%s</h5>\n" content);
    state
  end
  else if starts_with "#### " trimmed then begin
    let state = close_list (close_para state) in
    let content = process_inline (trim (drop_prefix "#### " trimmed)) in
    Buffer.add_string state.html (Printf.sprintf "<h4>%s</h4>\n" content);
    state
  end
  else if starts_with "### " trimmed then begin
    let state = close_list (close_para state) in
    let content = process_inline (trim (drop_prefix "### " trimmed)) in
    Buffer.add_string state.html (Printf.sprintf "<h3>%s</h3>\n" content);
    state
  end
  else if starts_with "## " trimmed then begin
    let state = close_list (close_para state) in
    let content = process_inline (trim (drop_prefix "## " trimmed)) in
    Buffer.add_string state.html (Printf.sprintf "<h2>%s</h2>\n" content);
    state
  end
  else if starts_with "# " trimmed then begin
    let state = close_list (close_para state) in
    let content = process_inline (trim (drop_prefix "# " trimmed)) in
    Buffer.add_string state.html (Printf.sprintf "<h1>%s</h1>\n" content);
    state
  end

  (* List items *)
  else if starts_with "- " trimmed || starts_with "* " trimmed then begin
    let state = close_para state in
    let state =
      if not state.in_list then begin
        Buffer.add_string state.html "<ul>\n";
        { state with in_list = true }
      end else state
    in
    let item = process_inline (trim (String.sub trimmed 2 (String.length trimmed - 2))) in
    Buffer.add_string state.html (Printf.sprintf "<li>%s</li>\n" item);
    state
  end

  (* Paragraph *)
  else begin
    let state = close_list state in
    if not state.in_para then begin
      Buffer.add_string state.html "<p>";
      Buffer.add_string state.html (process_inline trimmed);
      { state with in_para = true }
    end else begin
      Buffer.add_char state.html ' ';
      Buffer.add_string state.html (process_inline trimmed);
      state
    end
  end

let parse_markdown content =
  let lines = split_lines content in
  let state = List.fold_left (fun state line -> process_line line state) (init_state ()) lines in
  let state = close_list (close_para state) in
  let state =
    if state.in_code then begin
      Buffer.add_string state.html "</code></pre>\n";
      { state with in_code = false }
    end else state
  in
  Buffer.contents state.html

(* ============================================================================
   Template Engine
   ============================================================================ *)

let default_template =
  {|<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>{{title}}</title>
<style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style>
</head>
<body>
<article>
<h1>{{title}}</h1>
<time>{{date}}</time>
{{content}}
</article>
</body>
</html>|}

let replace_all needle replacement str =
  let nlen = String.length needle in
  if nlen = 0 then str
  else
    let buf = Buffer.create (String.length str * 2) in
    let rec go i =
      if i >= String.length str then ()
      else if i + nlen <= String.length str && String.sub str i nlen = needle then begin
        Buffer.add_string buf replacement;
        go (i + nlen)
      end else begin
        Buffer.add_char buf str.[i];
        go (i + 1)
      end
    in
    go 0;
    Buffer.contents buf

let apply_template fm html =
  default_template
  |> replace_all "{{title}}" fm.title
  |> replace_all "{{date}}" fm.date
  |> replace_all "{{content}}" html

(* ============================================================================
   File System Operations
   ============================================================================ *)

let ensure_dir path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755

let rec ensure_dir_all path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if parent <> path && parent <> "" then
      ensure_dir_all parent;
    Unix.mkdir path 0o755
  end

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let list_md_files dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
  else []

let copy_file src dst =
  let content = read_file src in
  write_file dst content

(* ============================================================================
   Build Command
   ============================================================================ *)

let build_site config =
  Printf.printf "Building site...\n%!";

  (* Ensure output directory exists *)
  ensure_dir_all config.output_dir;

  (* Check if content directory exists *)
  if not (Sys.file_exists config.content_dir) then begin
    Printf.printf "No content directory found. Creating %s/\n%!" config.content_dir;
    ensure_dir_all config.content_dir;
    0
  end else begin
    (* Process markdown files *)
    let files = list_md_files config.content_dir in
    let count = List.fold_left (fun count file ->
      let src_path = Filename.concat config.content_dir file in
      try
        let content = read_file src_path in
        let result = parse_frontmatter content in
        let html = parse_markdown result.body in
        let output = apply_template result.frontmatter html in

        (* Output path: replace .md with .html *)
        let out_name = Filename.chop_suffix file ".md" ^ ".html" in
        let out_path = Filename.concat config.output_dir out_name in

        write_file out_path output;
        Printf.printf "  %s -> %s\n%!" file out_path;
        count + 1
      with _ ->
        Printf.printf "  Error processing %s\n%!" file;
        count
    ) 0 files in

    (* Copy static files *)
    let static_count =
      if Sys.file_exists config.static_dir && Sys.is_directory config.static_dir then begin
        let files = Sys.readdir config.static_dir |> Array.to_list in
        List.iter (fun file ->
          let src = Filename.concat config.static_dir file in
          let dst = Filename.concat config.output_dir file in
          if Sys.is_directory src then ()
          else copy_file src dst
        ) files;
        List.length files
      end else 0
    in

    Printf.printf "\nBuild complete: %d pages, %d static files\n%!" count static_count;
    count
  end

(* ============================================================================
   Init Command
   ============================================================================ *)

let init_site name =
  Printf.printf "Initializing new site: %s\n%!" name;

  (* Create directories *)
  let dirs = [
    name;
    Filename.concat name "content";
    Filename.concat name "templates";
    Filename.concat name "static";
    Filename.concat name "public";
  ] in
  List.iter (fun dir ->
    ensure_dir_all dir;
    Printf.printf "  Created %s/\n%!" dir
  ) dirs;

  (* Create sample content *)
  let sample_content = {|---
title: Welcome to Macrauchenia
date: 2025-01-18
---

# Welcome

This is your first post built with **Macrauchenia SSG**.

## Features

- OCaml's powerful type system
- Pattern matching for elegant parsing
- Fast compilation and execution

```ocaml
let () = print_endline "Hello, Macrauchenia!"
```
|} in
  let content_path = Filename.concat (Filename.concat name "content") "index.md" in
  write_file content_path sample_content;
  Printf.printf "  Created sample content\n%!";

  Printf.printf "\nSite initialized! Run 'macrauchenia-ssg build' in %s/ to build.\n%!" name

(* ============================================================================
   Clean Command
   ============================================================================ *)

let rec remove_dir_contents dir =
  let files = Sys.readdir dir in
  Array.iter (fun file ->
    let path = Filename.concat dir file in
    if Sys.is_directory path then begin
      remove_dir_contents path;
      Unix.rmdir path
    end else
      Sys.remove path
  ) files

let clean_site config =
  Printf.printf "Cleaning %s/...\n%!" config.output_dir;
  if Sys.file_exists config.output_dir then begin
    remove_dir_contents config.output_dir;
    Unix.rmdir config.output_dir
  end;
  Printf.printf "Clean complete.\n%!"

(* ============================================================================
   Tests
   ============================================================================ *)

let test_markdown () =
  Printf.printf "=== Test: Markdown ===\n%!";
  let md = {|# Hello World

This is a **bold** test with *italic* text.

- Item 1
- Item 2

```
code block
```
|} in
  let html = parse_markdown md in
  Printf.printf "%s\n%!" html

let test_frontmatter () =
  Printf.printf "=== Test: Frontmatter ===\n%!";
  let content = {|---
title: My Post
date: 2024-01-15
tags: ocaml, ssg
draft: false
---

Content here
|} in
  let result = parse_frontmatter content in
  Printf.printf "Title: %s\n%!" result.frontmatter.title;
  Printf.printf "Date: %s\n%!" result.frontmatter.date;
  Printf.printf "Tags: %s\n%!" (String.concat ", " result.frontmatter.tags);
  Printf.printf "Draft: %b\n%!" result.frontmatter.draft;
  Printf.printf "Body: %s\n%!" result.body

let test_full () =
  Printf.printf "=== Test: Full Pipeline ===\n%!";
  let content = {|---
title: Welcome
date: 2024-01-15
---

# Welcome

This is **Macrauchenia**, an OCaml SSG.

- Type safe
- Pattern matching
- Elegant
|} in
  let result = parse_frontmatter content in
  let html = parse_markdown result.body in
  let output = apply_template result.frontmatter html in
  Printf.printf "%s\n%!" output
