(** ReScript WASM-GC Compiler - CLI Entry Point *)

open Rescript_wasm
open Lambda_ir
open Wasm_compile
open Wasm_emit
open Wasm_binary
open Wasm_optimize
open Wasm_sourcemap

(** {1 CLI Options} *)

type output_format = WAT | WASM

type cli_options = {
  mutable format : output_format;
  mutable output_file : string option;
  mutable opt_level : opt_level;
  mutable sourcemap : bool;
  mutable example : string option;
  mutable show_help : bool;
  mutable run_test : bool;
  mutable list_examples : bool;
}

let default_options () = {
  format = WAT;
  output_file = None;
  opt_level = O1;
  sourcemap = false;
  example = None;
  show_help = false;
  run_test = false;
  list_examples = false;
}

(** {1 Example Programs} *)

(** Example: let add = (a, b) => a + b *)
let example_add () =
  let a = make_ident "a" in
  let b = make_ident "b" in
  let body = Lprim (Paddint, [Lvar a; Lvar b]) in
  let func = Lfunction ({ params = [a; b]; return_type = Some Tint }, body) in
  [("add", func)]

(** Example: let result = 1 + 2 *)
let example_simple_add () =
  let expr = Lprim (Paddint, [const_int 1; const_int 2]) in
  [("result", expr)]

(** Example: let max = (a, b) => if a > b then a else b *)
let example_max () =
  let a = make_ident "a" in
  let b = make_ident "b" in
  let cond = Lprim (Pintcomp Cgt, [Lvar a; Lvar b]) in
  let body = Lifthenelse (cond, Lvar a, Lvar b) in
  let func = Lfunction ({ params = [a; b]; return_type = Some Tint }, body) in
  [("max", func)]

(** Example: let square = x => x * x *)
let example_square () =
  let x = make_ident "x" in
  let body = Lprim (Pmulint, [Lvar x; Lvar x]) in
  let func = Lfunction ({ params = [x]; return_type = Some Tint }, body) in
  [("square", func)]

(** Example: let fact = n => ... (iterative) *)
let example_factorial () =
  let n = make_ident "n" in
  let result = make_ident "result" in
  let i = make_ident "i" in

  (* result = 1 *)
  (* for i = 1 to n do result = result * i done *)
  (* result *)
  let body =
    Llet (Strict, result, const_int 1,
      Lsequence (
        Lfor (i, const_int 1, Lvar n, true,
          Llet (Strict, result,
            Lprim (Pmulint, [Lvar result; Lvar i]),
            const_unit)),
        Lvar result))
  in
  let func = Lfunction ({ params = [n]; return_type = Some Tint }, body) in
  [("factorial", func)]

(** Example: Float operations *)
let example_float () =
  let x = make_ident "x" in
  let y = make_ident "y" in
  let body = Lprim (Paddfloat, [
    Lprim (Pmulfloat, [Lvar x; Lvar x]);
    Lprim (Pmulfloat, [Lvar y; Lvar y])
  ]) in
  let func = Lfunction ({ params = [x; y]; return_type = Some Tfloat }, body) in
  [("sum_of_squares", func)]

(** Example: Tuple operations (Phase 2) *)
let example_tuple () =
  let x = make_ident "x" in
  let y = make_ident "y" in
  let body = tuple2 (Lvar x) (Lvar y) in
  let func = Lfunction ({ params = [x; y]; return_type = Some Tany }, body) in
  [("make_pair", func)]

(** Example: Array operations (Phase 2) *)
let example_array () =
  let arr = make_ident "arr" in
  let body = arraylength (Lvar arr) in
  let func = Lfunction ({ params = [arr]; return_type = Some Tint }, body) in
  [("array_len", func)]

(** Example: Closure (Phase 3) *)
let example_closure () =
  let n = make_ident "n" in
  let x = make_ident "x" in
  let inner_body = Lprim (Paddint, [Lvar x; Lvar n]) in
  let inner = Lfunction ({ params = [x]; return_type = Some Tint }, inner_body) in
  let func = Lfunction ({ params = [n]; return_type = Some Tany }, inner) in
  [("make_adder", func)]

(** Example: Variant check (Phase 4) *)
let example_variant () =
  let v = make_ident "v" in
  let body = isint (Lvar v) in
  let func = Lfunction ({ params = [v]; return_type = Some Tbool }, body) in
  [("is_immediate", func)]

(** Combined example with multiple functions *)
let example_combined () =
  example_add () @
  example_max () @
  example_square () @
  example_simple_add ()

(** {1 CLI} *)

let examples = [
  ("add", example_add);
  ("simple", example_simple_add);
  ("max", example_max);
  ("square", example_square);
  ("factorial", example_factorial);
  ("float", example_float);
  ("tuple", example_tuple);
  ("array", example_array);
  ("closure", example_closure);
  ("variant", example_variant);
  ("combined", example_combined);
]

let print_usage () =
  print_endline "ReScript WASM-GC Compiler";
  print_endline "Phases 1-7 Complete (MVP)";
  print_endline "";
  print_endline "Usage: rescript_wasm [options] [example]";
  print_endline "";
  print_endline "Output Options:";
  print_endline "  -o, --output FILE    Write output to FILE";
  print_endline "  -b, --binary         Output binary WASM (default: WAT text)";
  print_endline "  --sourcemap          Generate source map";
  print_endline "";
  print_endline "Optimization Options:";
  print_endline "  -O0                  No optimization";
  print_endline "  -O1                  Basic optimization (default)";
  print_endline "  -O2                  Full optimization";
  print_endline "";
  print_endline "General Options:";
  print_endline "  --help, -h           Show this help message";
  print_endline "  --test               Run built-in test";
  print_endline "  --list               List available examples";
  print_endline "";
  print_endline "Examples:";
  List.iter (fun (name, _) ->
    print_endline ("  " ^ name)
  ) examples;
  print_endline "";
  print_endline "Example usage:";
  print_endline "  rescript_wasm add              # Compile 'add' to WAT";
  print_endline "  rescript_wasm -b -o out.wasm add  # Binary WASM";
  print_endline "  rescript_wasm -O2 combined     # Optimized output"

let run_test () =
  print_endline "(* Running built-in test *)";
  print_endline "";

  (* Test 1: Simple add function *)
  print_endline "(* Test 1: add function *)";
  let program = example_add () in
  let wasm_module = compile_program program in
  let wat = emit_module wasm_module in
  print_endline wat;
  print_endline "";

  (* Test 2: Max function with conditional *)
  print_endline "(* Test 2: max function *)";
  let program = example_max () in
  let wasm_module = compile_program program in
  let wat = emit_module wasm_module in
  print_endline wat;
  print_endline "";

  (* Test 3: Combined *)
  print_endline "(* Test 3: combined *)";
  let program = example_combined () in
  let wasm_module = compile_program program in
  let wat = emit_module wasm_module in
  print_endline wat;

  print_endline "";
  print_endline "(* All tests completed *)"

let compile_example opts name =
  match List.assoc_opt name examples with
  | Some example_fn ->
      let program = example_fn () in
      let wasm_module = compile_program program in

      (* Apply optimizations *)
      let wasm_module = optimize ~level:opts.opt_level wasm_module in

      (* Output *)
      begin match opts.format with
      | WAT ->
          let wat = emit_module wasm_module in
          begin match opts.output_file with
          | Some file ->
              let oc = open_out file in
              output_string oc wat;
              close_out oc;
              Printf.printf "Wrote WAT to %s\n" file
          | None ->
              print_endline wat
          end
      | WASM ->
          let bytes = module_to_bytes wasm_module in
          begin match opts.output_file with
          | Some file ->
              let oc = open_out_bin file in
              output_bytes oc bytes;
              close_out oc;
              Printf.printf "Wrote %d bytes to %s\n" (Bytes.length bytes) file
          | None ->
              Printf.eprintf "Binary output requires -o/--output option\n";
              exit 1
          end
      end;

      (* Generate source map if requested *)
      if opts.sourcemap then begin
        let map = empty_source_map in
        let map = add_source map (name ^ ".res") in
        let map = add_mapping map ~wasm_offset:0 ~file:(name ^ ".res") ~line:1 ~column:0 in
        match opts.output_file with
        | Some file ->
            let map_file = file ^ ".map" in
            write_source_map map_file map;
            Printf.printf "Wrote source map to %s\n" map_file
        | None ->
            let json = to_json map in
            print_endline "(* Source Map *)";
            print_endline json
      end

  | None ->
      Printf.eprintf "Unknown example: %s\n" name;
      Printf.eprintf "Use --list to see available examples\n";
      exit 1

let parse_args args =
  let opts = default_options () in
  let rec loop = function
    | [] -> opts
    | ("--help" | "-h") :: _ ->
        opts.show_help <- true; opts
    | "--test" :: rest ->
        opts.run_test <- true; loop rest
    | "--list" :: rest ->
        opts.list_examples <- true; loop rest
    | ("-b" | "--binary") :: rest ->
        opts.format <- WASM; loop rest
    | "--sourcemap" :: rest ->
        opts.sourcemap <- true; loop rest
    | "-O0" :: rest ->
        opts.opt_level <- O0; loop rest
    | "-O1" :: rest ->
        opts.opt_level <- O1; loop rest
    | "-O2" :: rest ->
        opts.opt_level <- O2; loop rest
    | ("-o" | "--output") :: file :: rest ->
        opts.output_file <- Some file; loop rest
    | ("-o" | "--output") :: [] ->
        Printf.eprintf "Error: -o/--output requires a filename\n";
        exit 1
    | name :: rest when not (String.starts_with ~prefix:"-" name) ->
        opts.example <- Some name; loop rest
    | unknown :: _ ->
        Printf.eprintf "Unknown option: %s\n" unknown;
        exit 1
  in
  loop args

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let opts = parse_args args in

  if opts.show_help || args = [] then
    print_usage ()
  else if opts.run_test then
    run_test ()
  else if opts.list_examples then begin
    print_endline "Available examples:";
    List.iter (fun (name, _) -> print_endline ("  " ^ name)) examples
  end
  else match opts.example with
    | Some name -> compile_example opts name
    | None ->
        print_endline "Error: No example specified";
        print_usage ();
        exit 1
