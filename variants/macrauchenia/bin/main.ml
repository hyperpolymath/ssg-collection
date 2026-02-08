(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell *)
(*
   main.ml - CLI entry point for Macrauchenia SSG
*)

open Cmdliner
open Macrauchenia_ssg

(* Build command *)
let build_cmd =
  let doc = "Build the site" in
  let info = Cmd.info "build" ~doc in
  Cmd.v info (Term.(const (fun () -> ignore (build_site default_config)) $ const ()))

(* Init command *)
let init_cmd =
  let doc = "Create a new site" in
  let info = Cmd.info "init" ~doc in
  let name =
    let doc = "Name of the site to create" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  Cmd.v info Term.(const init_site $ name)

(* Clean command *)
let clean_cmd =
  let doc = "Remove generated files" in
  let info = Cmd.info "clean" ~doc in
  Cmd.v info (Term.(const (fun () -> clean_site default_config) $ const ()))

(* Test commands *)
let test_markdown_cmd =
  let doc = "Test markdown parser" in
  let info = Cmd.info "test-markdown" ~doc in
  Cmd.v info (Term.(const test_markdown $ const ()))

let test_frontmatter_cmd =
  let doc = "Test frontmatter parser" in
  let info = Cmd.info "test-frontmatter" ~doc in
  Cmd.v info (Term.(const test_frontmatter $ const ()))

let test_full_cmd =
  let doc = "Test full pipeline" in
  let info = Cmd.info "test-full" ~doc in
  Cmd.v info (Term.(const test_full $ const ()))

(* Main command group *)
let main_cmd =
  let doc = "OCaml-powered static site generator" in
  let man = [
    `S Manpage.s_description;
    `P "Macrauchenia SSG is a static site generator written in OCaml,";
    `P "showcasing pattern matching and type safety.";
    `S Manpage.s_examples;
    `P "$(mname) init my-blog";
    `P "cd my-blog && $(mname) build";
  ] in
  let info = Cmd.info "macrauchenia-ssg" ~version:"0.1.0" ~doc ~man in
  Cmd.group info [
    build_cmd;
    init_cmd;
    clean_cmd;
    test_markdown_cmd;
    test_frontmatter_cmd;
    test_full_cmd;
  ]

let () = exit (Cmd.eval main_cmd)
