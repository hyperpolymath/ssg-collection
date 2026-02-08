(* yocaml_lite.ml - OCaml-powered static site generator
 *
 * "YoCamlLite" - Functional precision in site generation
 *
 * OCaml's type system and pattern matching make parsing elegant.
 * Inspired by the YOCaml project.
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

type parser_state = {
  html: Buffer.t;
  in_para: bool;
  in_code: bool;
  in_list: bool;
}

let empty_frontmatter = {
  title = "";
  date = "";
  tags = [];
  draft = false;
  template = "default";
}

let init_state () = {
  html = Buffer.create 1024;
  in_para = false;
  in_code = false;
  in_list = false;
}

(* ============================================================================
   String Utilities
   ============================================================================ *)

let starts_with prefix str =
  let plen = String.length prefix in
  let slen = String.length str in
  plen <= slen && String.sub str 0 plen = prefix

let strip_prefix prefix str =
  if starts_with prefix str then
    String.sub str (String.length prefix) (String.length str - String.length prefix)
  else str

let trim s =
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_space s.[!i] do incr i done;
  let j = ref (len - 1) in
  while !j >= !i && is_space s.[!j] do decr j done;
  if !i > !j then "" else String.sub s !i (!j - !i + 1)

let split_on_char sep s =
  let rec aux acc i =
    match String.index_from_opt s i sep with
    | None -> List.rev (String.sub s i (String.length s - i) :: acc)
    | Some j -> aux (String.sub s i (j - i) :: acc) (j + 1)
  in
  if s = "" then [""] else aux [] 0

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
  match String.index_opt line ':' with
  | None -> fm
  | Some i ->
    let key = trim (String.sub line 0 i) in
    let value = trim (String.sub line (i + 1) (String.length line - i - 1)) in
    match key with
    | "title" -> { fm with title = value }
    | "date" -> { fm with date = value }
    | "template" -> { fm with template = value }
    | "draft" -> { fm with draft = (value = "true" || value = "yes") }
    | "tags" ->
      let tag_str = if starts_with "[" value then
        String.sub value 1 (String.length value - 2)
      else value in
      let tags = List.map trim (split_on_char ',' tag_str) in
      { fm with tags = List.filter (fun s -> String.length s > 0) tags }
    | _ -> fm

let parse_frontmatter content =
  let lines = split_on_char '\n' content in
  match lines with
  | first :: rest when trim first = "---" ->
    let rec find_end acc = function
      | [] -> (List.fold_left (fun fm l -> parse_fm_line l fm) empty_frontmatter (List.rev acc), "")
      | l :: ls when trim l = "---" ->
        (List.fold_left (fun fm line -> parse_fm_line line fm) empty_frontmatter (List.rev acc),
         String.concat "\n" ls)
      | l :: ls -> find_end (l :: acc) ls
    in
    find_end [] rest
  | _ -> (empty_frontmatter, content)

(* ============================================================================
   Markdown Parser
   ============================================================================ *)

let process_inline text =
  let buf = Buffer.create (String.length text * 2) in
  let len = String.length text in
  let rec go i in_bold in_ital =
    if i >= len then ()
    else if i + 1 < len && text.[i] = '*' && text.[i+1] = '*' then begin
      Buffer.add_string buf (if in_bold then "</strong>" else "<strong>");
      go (i + 2) (not in_bold) in_ital
    end
    else if text.[i] = '*' then begin
      Buffer.add_string buf (if in_ital then "</em>" else "<em>");
      go (i + 1) in_bold (not in_ital)
    end
    else if text.[i] = '`' then begin
      let j = ref (i + 1) in
      while !j < len && text.[!j] <> '`' do incr j done;
      if !j < len then begin
        Buffer.add_string buf "<code>";
        Buffer.add_string buf (String.sub text (i + 1) (!j - i - 1));
        Buffer.add_string buf "</code>";
        go (!j + 1) in_bold in_ital
      end else begin
        Buffer.add_char buf '`';
        go (i + 1) in_bold in_ital
      end
    end
    else begin
      Buffer.add_char buf text.[i];
      go (i + 1) in_bold in_ital
    end
  in
  go 0 false false;
  Buffer.contents buf

let close_para st =
  if st.in_para then begin
    Buffer.add_string st.html "</p>\n";
    { st with in_para = false }
  end else st

let close_list st =
  if st.in_list then begin
    Buffer.add_string st.html "</ul>\n";
    { st with in_list = false }
  end else st

let process_line line st =
  let tr = trim line in

  (* Code fence *)
  if starts_with "```" tr then
    if st.in_code then begin
      Buffer.add_string st.html "</code></pre>\n";
      { st with in_code = false }
    end else begin
      let st = close_list (close_para st) in
      Buffer.add_string st.html "<pre><code>";
      { st with in_code = true }
    end

  (* Inside code block *)
  else if st.in_code then begin
    Buffer.add_string st.html (escape_html line);
    Buffer.add_char st.html '\n';
    st
  end

  (* Empty line *)
  else if tr = "" then
    close_list (close_para st)

  (* Headers *)
  else if starts_with "###" tr then begin
    let st = close_list (close_para st) in
    let content = process_inline (trim (strip_prefix "###" tr)) in
    Buffer.add_string st.html (Printf.sprintf "<h3>%s</h3>\n" content);
    st
  end
  else if starts_with "##" tr then begin
    let st = close_list (close_para st) in
    let content = process_inline (trim (strip_prefix "##" tr)) in
    Buffer.add_string st.html (Printf.sprintf "<h2>%s</h2>\n" content);
    st
  end
  else if starts_with "#" tr then begin
    let st = close_list (close_para st) in
    let content = process_inline (trim (strip_prefix "#" tr)) in
    Buffer.add_string st.html (Printf.sprintf "<h1>%s</h1>\n" content);
    st
  end

  (* List items *)
  else if starts_with "- " tr || starts_with "* " tr then begin
    let st = close_para st in
    let st = if not st.in_list then begin
      Buffer.add_string st.html "<ul>\n";
      { st with in_list = true }
    end else st in
    let item = process_inline (trim (String.sub tr 2 (String.length tr - 2))) in
    Buffer.add_string st.html (Printf.sprintf "<li>%s</li>\n" item);
    st
  end

  (* Paragraph *)
  else begin
    let st = if not st.in_para then begin
      Buffer.add_string st.html "<p>";
      { st with in_para = true }
    end else begin
      Buffer.add_string st.html " ";
      st
    end in
    Buffer.add_string st.html (process_inline tr);
    st
  end

let parse_markdown content =
  let lines = split_on_char '\n' content in
  let st = List.fold_left (fun st line -> process_line line st) (init_state ()) lines in
  let st = close_list (close_para st) in
  let st = if st.in_code then begin
    Buffer.add_string st.html "</code></pre>\n";
    st
  end else st in
  Buffer.contents st.html

(* ============================================================================
   Template Engine
   ============================================================================ *)

let default_template = {|<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>{{title}}</title>
<style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style>
</head><body><article><h1>{{title}}</h1><time>{{date}}</time>
{{content}}
</article></body></html>|}

let replace_all needle replacement haystack =
  let nlen = String.length needle in
  if nlen = 0 then haystack
  else
    let buf = Buffer.create (String.length haystack) in
    let rec go i =
      if i > String.length haystack - nlen then
        Buffer.add_substring buf haystack i (String.length haystack - i)
      else if String.sub haystack i nlen = needle then begin
        Buffer.add_string buf replacement;
        go (i + nlen)
      end else begin
        Buffer.add_char buf haystack.[i];
        go (i + 1)
      end
    in
    go 0;
    Buffer.contents buf

let apply_template fm html =
  let t1 = replace_all "{{title}}" fm.title default_template in
  let t2 = replace_all "{{date}}" fm.date t1 in
  let t3 = replace_all "{{content}}" html t2 in
  t3

(* ============================================================================
   Tests
   ============================================================================ *)

let test_markdown () =
  print_endline "=== Test: Markdown ===";
  let md = "# Hello World\n\nThis is a **bold** test with *italic* text.\n\n- Item 1\n- Item 2\n\n```\ncode block\n```\n" in
  print_endline (parse_markdown md)

let test_frontmatter () =
  print_endline "=== Test: Frontmatter ===";
  let content = "---\ntitle: My Post\ndate: 2024-01-15\ntags: [ocaml, ssg]\ndraft: false\n---\n\nContent here\n" in
  let (fm, body) = parse_frontmatter content in
  Printf.printf "Title: %s\n" fm.title;
  Printf.printf "Date: %s\n" fm.date;
  Printf.printf "Tags: [%s]\n" (String.concat ", " fm.tags);
  Printf.printf "Draft: %b\n" fm.draft;
  Printf.printf "Body: %s\n" body

let test_full () =
  print_endline "=== Test: Full Pipeline ===";
  let content = "---\ntitle: Welcome\ndate: 2024-01-15\n---\n\n# Welcome\n\nThis is **YoCamlLite**, an OCaml SSG.\n\n- Type safe\n- Functional\n- Fast\n" in
  let (fm, body) = parse_frontmatter content in
  let html = parse_markdown body in
  let output = apply_template fm html in
  print_endline output

(* ============================================================================
   Main
   ============================================================================ *)

let () =
  match Array.to_list Sys.argv with
  | [_; "test-markdown"] -> test_markdown ()
  | [_; "test-frontmatter"] -> test_frontmatter ()
  | [_; "test-full"] -> test_full ()
  | _ ->
    print_endline "YoCamlLite - OCaml powered SSG";
    print_endline "Commands: test-markdown test-frontmatter test-full"
