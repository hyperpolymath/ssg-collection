(** Source Map Generation (Phase 6)

    Generate DWARF-style debug info for WASM modules.
    Supports mapping WASM instructions back to ReScript source locations.

    Reference: https://yurydelendik.github.io/webassembly-dwarf/
*)

(** {1 Source Location} *)

type source_location = {
  file : string;
  line : int;
  column : int;
}

(** {1 Source Map Entry} *)

type source_map_entry = {
  wasm_offset : int;      (** Byte offset in WASM binary *)
  source_loc : source_location;
}

(** {1 Source Map} *)

type source_map = {
  version : int;
  sources : string list;
  mappings : source_map_entry list;
}

(** Create an empty source map *)
let empty_source_map = {
  version = 3;
  sources = [];
  mappings = [];
}

(** Add a source file to the map *)
let add_source map file =
  if List.mem file map.sources then map
  else { map with sources = map.sources @ [file] }

(** Add a mapping entry *)
let add_mapping map ~wasm_offset ~file ~line ~column =
  let map = add_source map file in
  let entry = {
    wasm_offset;
    source_loc = { file; line; column };
  } in
  { map with mappings = map.mappings @ [entry] }

(** {1 VLQ Encoding} *)

(** Base64 VLQ encoding for source maps *)
let base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let encode_vlq n =
  let sign_bit = if n < 0 then 1 else 0 in
  let n = if n < 0 then -n else n in
  let n = (n lsl 1) lor sign_bit in
  let rec loop n acc =
    let digit = n land 0x1f in
    let n' = n lsr 5 in
    let char_idx = if n' > 0 then digit lor 0x20 else digit in
    let acc' = base64_chars.[char_idx] :: acc in
    if n' > 0 then loop n' acc'
    else acc'
  in
  loop n [] |> List.rev |> List.to_seq |> String.of_seq

(** {1 Source Map JSON Generation} *)

(** Generate JSON source map format *)
let to_json map =
  let sources_json =
    map.sources
    |> List.map (Printf.sprintf "\"%s\"")
    |> String.concat ", "
  in

  (* Generate VLQ-encoded mappings *)
  let prev_gen_col = ref 0 in
  let prev_source = ref 0 in
  let prev_line = ref 0 in
  let prev_col = ref 0 in

  let mapping_segments =
    map.mappings
    |> List.map (fun entry ->
        let source_idx =
          match List.find_index ((=) entry.source_loc.file) map.sources with
          | Some i -> i
          | None -> 0
        in
        let gen_col_delta = entry.wasm_offset - !prev_gen_col in
        let source_delta = source_idx - !prev_source in
        let line_delta = entry.source_loc.line - !prev_line in
        let col_delta = entry.source_loc.column - !prev_col in

        prev_gen_col := entry.wasm_offset;
        prev_source := source_idx;
        prev_line := entry.source_loc.line;
        prev_col := entry.source_loc.column;

        encode_vlq gen_col_delta ^
        encode_vlq source_delta ^
        encode_vlq line_delta ^
        encode_vlq col_delta
      )
    |> String.concat ","
  in

  Printf.sprintf {|{
  "version": %d,
  "sources": [%s],
  "names": [],
  "mappings": "%s"
}|} map.version sources_json mapping_segments

(** Write source map to file *)
let write_source_map filename map =
  let json = to_json map in
  let oc = open_out filename in
  output_string oc json;
  close_out oc

(** {1 DWARF-style Debug Sections} *)

(** Debug section for WASM custom section *)
type debug_section = {
  name : string;
  data : bytes;
}

(** Generate minimal DWARF-like debug info *)
let generate_debug_section map =
  (* For now, generate a simple custom section with source info *)
  let buffer = Buffer.create 256 in

  (* Write source count *)
  Buffer.add_uint8 buffer (List.length map.sources);

  (* Write source file names *)
  List.iter (fun src ->
    let len = String.length src in
    Buffer.add_uint8 buffer len;
    Buffer.add_string buffer src
  ) map.sources;

  (* Write mapping count *)
  Buffer.add_uint8 buffer (min 255 (List.length map.mappings));

  (* Write mappings (simplified) *)
  List.iteri (fun i entry ->
    if i < 255 then begin
      Buffer.add_uint16_le buffer entry.wasm_offset;
      Buffer.add_uint16_le buffer entry.source_loc.line;
      Buffer.add_uint8 buffer entry.source_loc.column
    end
  ) map.mappings;

  {
    name = "sourceMappingURL";
    data = Buffer.to_bytes buffer;
  }

(** {1 Source Map Builder} *)

(** Builder for constructing source maps during compilation *)
module Builder = struct
  type t = {
    mutable current_offset : int;
    mutable map : source_map;
    mutable current_file : string option;
    mutable current_line : int;
  }

  let create () = {
    current_offset = 0;
    map = empty_source_map;
    current_file = None;
    current_line = 1;
  }

  let set_file builder file =
    builder.current_file <- Some file;
    builder.map <- add_source builder.map file

  let set_line builder line =
    builder.current_line <- line

  let add_instruction builder ~bytes_emitted =
    begin match builder.current_file with
    | Some file ->
        builder.map <- add_mapping builder.map
          ~wasm_offset:builder.current_offset
          ~file
          ~line:builder.current_line
          ~column:0
    | None -> ()
    end;
    builder.current_offset <- builder.current_offset + bytes_emitted

  let finish builder = builder.map
end
