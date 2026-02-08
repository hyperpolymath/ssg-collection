(** Binary WASM Emission (Phase 6)

    Emits WASM modules in binary format using LEB128 encoding.
    Reference: https://webassembly.github.io/spec/core/binary/
*)

open Wasm_types

(** {1 LEB128 Encoding} *)

(** Encode unsigned integer as unsigned LEB128 *)
let encode_uleb128 n =
  let rec loop n acc =
    let byte = n land 0x7f in
    let n' = n lsr 7 in
    if n' = 0 then
      List.rev (byte :: acc)
    else
      loop n' ((byte lor 0x80) :: acc)
  in
  loop n []

(** Encode signed integer as signed LEB128 *)
let encode_sleb128 n =
  let rec loop n acc =
    let byte = n land 0x7f in
    let n' = n asr 7 in
    let more = not ((n' = 0 && byte land 0x40 = 0) ||
                    (n' = -1 && byte land 0x40 <> 0)) in
    if more then
      loop n' ((byte lor 0x80) :: acc)
    else
      List.rev (byte :: acc)
  in
  loop n []

(** Encode i32 as LEB128 *)
let encode_i32 n = encode_sleb128 (Int32.to_int n)

(** Encode i64 as LEB128 *)
let encode_i64 n = encode_sleb128 (Int64.to_int n)

(** Encode f32 (IEEE 754 little-endian) *)
let encode_f32 f =
  let bits = Int32.bits_of_float f in
  [Int32.to_int (Int32.logand bits 0xffl);
   Int32.to_int (Int32.logand (Int32.shift_right_logical bits 8) 0xffl);
   Int32.to_int (Int32.logand (Int32.shift_right_logical bits 16) 0xffl);
   Int32.to_int (Int32.logand (Int32.shift_right_logical bits 24) 0xffl)]

(** Encode f64 (IEEE 754 little-endian) *)
let encode_f64 f =
  let bits = Int64.bits_of_float f in
  List.init 8 (fun i ->
    Int64.to_int (Int64.logand (Int64.shift_right_logical bits (i * 8)) 0xffL))

(** Encode string as length-prefixed UTF-8 *)
let encode_string s =
  let bytes = String.to_seq s |> List.of_seq |> List.map Char.code in
  encode_uleb128 (List.length bytes) @ bytes

(** Encode a vector (length-prefixed list) *)
let encode_vec encoder items =
  let encoded = List.concat_map encoder items in
  encode_uleb128 (List.length items) @ encoded

(** {1 Type Section Encoding} *)

(** Value type opcodes *)
let rec val_type_opcode = function
  | I32 -> [0x7f]
  | I64 -> [0x7e]
  | F32 -> [0x7d]
  | F64 -> [0x7c]
  | Ref ht -> [0x6b] @ heap_type_opcode ht
  | RefNull ht -> [0x6c] @ heap_type_opcode ht

and heap_type_opcode = function
  | Func _ -> [0x70]  (* funcref *)
  | Extern -> [0x6f]  (* externref *)
  | Any -> [0x6e]     (* anyref *)
  | Eq -> [0x6d]      (* eqref *)
  | I31 -> [0x6a]     (* i31ref *)
  | Struct name -> encode_sleb128 (Hashtbl.hash name land 0x7fffffff)  (* type index *)
  | Array name -> encode_sleb128 (Hashtbl.hash name land 0x7fffffff)
  | None_ -> [0x71]   (* nullref *)

(** Encode function type *)
let encode_func_type ft =
  [0x60] @  (* func type indicator *)
  encode_vec val_type_opcode ft.ft_params @
  encode_vec val_type_opcode ft.ft_results

(** Encode struct type (GC extension) *)
let encode_struct_type st =
  let encode_field f =
    val_type_opcode f.field_type @
    [if f.field_mutable then 0x01 else 0x00]
  in
  [0x5f] @  (* struct type indicator *)
  encode_vec encode_field st.struct_fields

(** Encode array type (GC extension) *)
let encode_array_type at =
  [0x5e] @  (* array type indicator *)
  val_type_opcode at.array_elem_type @
  [if at.array_elem_mutable then 0x01 else 0x00]

(** Encode type definition *)
let encode_type_def = function
  | TypeStruct st -> encode_struct_type st
  | TypeArray at -> encode_array_type at
  | TypeFunc (_, ft) -> encode_func_type ft

(** {1 Instruction Encoding} *)

(** Instruction opcodes *)
let rec encode_instr instr =
  match instr with
  | I32Const n -> [0x41] @ encode_i32 n
  | I64Const n -> [0x42] @ encode_i64 n
  | F32Const f -> [0x43] @ encode_f32 f
  | F64Const f -> [0x44] @ encode_f64 f

  | LocalGet idx -> [0x20] @ encode_uleb128 idx
  | LocalSet (idx, e) -> encode_instr e @ [0x21] @ encode_uleb128 idx
  | LocalTee (idx, e) -> encode_instr e @ [0x22] @ encode_uleb128 idx
  | GlobalGet name -> [0x23] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | GlobalSet (name, e) -> encode_instr e @ [0x24] @ encode_uleb128 (Hashtbl.hash name land 0xffff)

  | I32Add (a, b) -> encode_instr a @ encode_instr b @ [0x6a]
  | I32Sub (a, b) -> encode_instr a @ encode_instr b @ [0x6b]
  | I32Mul (a, b) -> encode_instr a @ encode_instr b @ [0x6c]
  | I32DivS (a, b) -> encode_instr a @ encode_instr b @ [0x6d]
  | I32RemS (a, b) -> encode_instr a @ encode_instr b @ [0x6f]
  | I32And (a, b) -> encode_instr a @ encode_instr b @ [0x71]
  | I32Or (a, b) -> encode_instr a @ encode_instr b @ [0x72]
  | I32Xor (a, b) -> encode_instr a @ encode_instr b @ [0x73]
  | I32Shl (a, b) -> encode_instr a @ encode_instr b @ [0x74]
  | I32ShrS (a, b) -> encode_instr a @ encode_instr b @ [0x75]
  | I32Eq (a, b) -> encode_instr a @ encode_instr b @ [0x46]
  | I32Ne (a, b) -> encode_instr a @ encode_instr b @ [0x47]
  | I32LtS (a, b) -> encode_instr a @ encode_instr b @ [0x48]
  | I32LeS (a, b) -> encode_instr a @ encode_instr b @ [0x4c]
  | I32GtS (a, b) -> encode_instr a @ encode_instr b @ [0x4a]
  | I32GeS (a, b) -> encode_instr a @ encode_instr b @ [0x4e]
  | I32LtU (a, b) -> encode_instr a @ encode_instr b @ [0x49]
  | I32LeU (a, b) -> encode_instr a @ encode_instr b @ [0x4d]
  | I32GtU (a, b) -> encode_instr a @ encode_instr b @ [0x4b]
  | I32GeU (a, b) -> encode_instr a @ encode_instr b @ [0x4f]
  | I32Eqz e -> encode_instr e @ [0x45]

  | F64Add (a, b) -> encode_instr a @ encode_instr b @ [0xa0]
  | F64Sub (a, b) -> encode_instr a @ encode_instr b @ [0xa1]
  | F64Mul (a, b) -> encode_instr a @ encode_instr b @ [0xa2]
  | F64Div (a, b) -> encode_instr a @ encode_instr b @ [0xa3]
  | F64Neg e -> encode_instr e @ [0x9a]
  | F64Eq (a, b) -> encode_instr a @ encode_instr b @ [0x61]
  | F64Ne (a, b) -> encode_instr a @ encode_instr b @ [0x62]
  | F64Lt (a, b) -> encode_instr a @ encode_instr b @ [0x63]
  | F64Le (a, b) -> encode_instr a @ encode_instr b @ [0x65]
  | F64Gt (a, b) -> encode_instr a @ encode_instr b @ [0x64]
  | F64Ge (a, b) -> encode_instr a @ encode_instr b @ [0x66]

  | Block (_, body) ->
      [0x02; 0x40] @  (* block with void type *)
      List.concat_map encode_instr body @
      [0x0b]  (* end *)
  | Loop (_, body) ->
      [0x03; 0x40] @  (* loop with void type *)
      List.concat_map encode_instr body @
      [0x0b]
  | If (cond, then_, else_) ->
      encode_instr cond @
      [0x04; 0x40] @  (* if with void type *)
      List.concat_map encode_instr then_ @
      [0x05] @  (* else *)
      List.concat_map encode_instr else_ @
      [0x0b]
  | Br n -> [0x0c] @ encode_uleb128 n
  | BrIf (n, cond) -> encode_instr cond @ [0x0d] @ encode_uleb128 n
  | BrTable (labels, default, idx) ->
      encode_instr idx @
      [0x0e] @
      encode_vec encode_uleb128 labels @
      encode_uleb128 default
  | Return None -> [0x0f]
  | Return (Some e) -> encode_instr e @ [0x0f]
  | Call (name, args) ->
      List.concat_map encode_instr args @
      [0x10] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | CallRef (_, fn, args) ->
      List.concat_map encode_instr args @
      encode_instr fn @
      [0x14; 0x00]  (* call_ref with type 0 *)
  | Unreachable -> [0x00]

  (* GC instructions use 0xfb prefix *)
  | StructNew (name, fields) ->
      List.concat_map encode_instr fields @
      [0xfb; 0x00] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | StructGet (name, _, obj) ->
      encode_instr obj @
      [0xfb; 0x02] @ encode_uleb128 (Hashtbl.hash name land 0xffff) @ [0x00]
  | StructSet (name, _, obj, v) ->
      encode_instr obj @ encode_instr v @
      [0xfb; 0x05] @ encode_uleb128 (Hashtbl.hash name land 0xffff) @ [0x00]
  | ArrayNew (name, init, len) ->
      encode_instr init @ encode_instr len @
      [0xfb; 0x06] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | ArrayNewFixed (name, elems) ->
      List.concat_map encode_instr elems @
      [0xfb; 0x08] @ encode_uleb128 (Hashtbl.hash name land 0xffff) @ encode_uleb128 (List.length elems)
  | ArrayNewDefault (name, len) ->
      encode_instr len @
      [0xfb; 0x07] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | ArrayGet (name, arr, idx) ->
      encode_instr arr @ encode_instr idx @
      [0xfb; 0x0b] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | ArrayGetS (name, arr, idx) ->
      encode_instr arr @ encode_instr idx @
      [0xfb; 0x0c] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | ArraySet (name, arr, idx, v) ->
      encode_instr arr @ encode_instr idx @ encode_instr v @
      [0xfb; 0x0e] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | ArrayLen arr ->
      encode_instr arr @ [0xfb; 0x0f]
  | ArrayCopy (dst, src, d, di, s, si, l) ->
      encode_instr d @ encode_instr di @ encode_instr s @ encode_instr si @ encode_instr l @
      [0xfb; 0x11] @
      encode_uleb128 (Hashtbl.hash dst land 0xffff) @
      encode_uleb128 (Hashtbl.hash src land 0xffff)
  | RefCast (ht, e) ->
      encode_instr e @ [0xfb; 0x17] @ heap_type_opcode ht
  | RefTest (ht, e) ->
      encode_instr e @ [0xfb; 0x14] @ heap_type_opcode ht
  | RefIsNull e ->
      encode_instr e @ [0xd1]
  | RefAsNonNull e ->
      encode_instr e @ [0xd3]
  | RefEq (a, b) ->
      encode_instr a @ encode_instr b @ [0xd5]
  | RefNull ht ->
      [0xd0] @ heap_type_opcode ht
  | RefI31 e ->
      encode_instr e @ [0xfb; 0x1c]
  | RefFunc name ->
      [0xd2] @ encode_uleb128 (Hashtbl.hash name land 0xffff)
  | I31GetS e ->
      encode_instr e @ [0xfb; 0x1d]
  | I31GetU e ->
      encode_instr e @ [0xfb; 0x1e]
  | Seq instrs ->
      List.concat_map encode_instr instrs
  | Drop e ->
      encode_instr e @ [0x1a]
  | Nop -> [0x01]

(** {1 Section Encoding} *)

(** Section IDs *)
let section_custom = 0
let section_type = 1
let section_import = 2
let section_function = 3
let section_table = 4
let section_memory = 5
let section_global = 6
let section_export = 7
let section_start = 8
let section_element = 9
let section_code = 10
let section_data = 11

let _unused = section_custom + section_table + section_memory + section_element + section_data

(** Encode a section with ID and content *)
let encode_section id content =
  if content = [] then []
  else
    [id] @
    encode_uleb128 (List.length content) @
    content

(** Encode type section *)
let encode_type_section types =
  let content = encode_vec encode_type_def types in
  encode_section section_type content

(** Encode import section *)
let encode_import_section imports =
  let encode_import imp =
    encode_string imp.import_module @
    encode_string imp.import_name @
    (match imp.import_desc with
     | ImportFunc ft -> [0x00] @ encode_uleb128 (Hashtbl.hash (Printf.sprintf "%d%d" (List.length ft.ft_params) (List.length ft.ft_results)) land 0xffff)
     | ImportGlobal (typ, is_mut) -> [0x03] @ val_type_opcode typ @ [if is_mut then 0x01 else 0x00])
  in
  let content = encode_vec encode_import imports in
  encode_section section_import content

(** Encode function section (type indices only) *)
let encode_function_section funcs =
  let encode_func_idx f =
    encode_uleb128 (Hashtbl.hash (Printf.sprintf "%d%d"
      (List.length f.func_type.ft_params)
      (List.length f.func_type.ft_results)) land 0xffff)
  in
  let content = encode_vec encode_func_idx funcs in
  encode_section section_function content

(** Encode global section *)
let encode_global_section globals =
  let encode_global g =
    val_type_opcode g.global_type @
    [if g.global_mutable then 0x01 else 0x00] @
    encode_instr g.global_init @
    [0x0b]  (* end *)
  in
  let content = encode_vec encode_global globals in
  encode_section section_global content

(** Encode export section *)
let encode_export_section funcs globals =
  let func_exports = List.filter_map (fun f ->
    match f.func_export with
    | Some name -> Some (name, `Func, f.func_name)
    | None -> None
  ) funcs in
  let global_exports = List.filter_map (fun g ->
    match g.global_export with
    | Some name -> Some (name, `Global, g.global_name)
    | None -> None
  ) globals in
  let exports = func_exports @ global_exports in
  let encode_export (name, kind, _ref_name) =
    let kind_byte = match kind with
      | `Func -> 0x00
      | `Table -> 0x01
      | `Memory -> 0x02
      | `Global -> 0x03
    in
    encode_string name @
    [kind_byte] @
    encode_uleb128 (Hashtbl.hash _ref_name land 0xffff)
  in
  let content = encode_vec encode_export exports in
  encode_section section_export content

(** Encode start section *)
let encode_start_section start_func =
  match start_func with
  | None -> []
  | Some name -> encode_section section_start (encode_uleb128 (Hashtbl.hash name land 0xffff))

(** Encode code section *)
let encode_code_section funcs =
  let encode_func_body f =
    (* Local declarations *)
    let locals = List.filter_map (fun (_, typ) ->
      Some (1, typ)
    ) f.func_locals in
    let encode_local (count, typ) =
      encode_uleb128 count @ val_type_opcode typ
    in
    let local_bytes = encode_vec encode_local locals in
    let body_bytes = encode_instr f.func_body @ [0x0b] in  (* end *)
    let func_bytes = local_bytes @ body_bytes in
    encode_uleb128 (List.length func_bytes) @ func_bytes
  in
  let content = encode_vec encode_func_body funcs in
  encode_section section_code content

(** {1 Module Encoding} *)

(** WASM magic number and version *)
let wasm_magic = [0x00; 0x61; 0x73; 0x6d]  (* \0asm *)
let wasm_version = [0x01; 0x00; 0x00; 0x00]  (* version 1 *)

(** Encode a complete WASM module to binary *)
let encode_module m =
  let bytes =
    wasm_magic @
    wasm_version @
    encode_type_section m.mod_types @
    encode_import_section m.mod_imports @
    encode_function_section m.mod_funcs @
    encode_global_section m.mod_globals @
    encode_export_section m.mod_funcs m.mod_globals @
    encode_start_section m.mod_start @
    encode_code_section m.mod_funcs
  in
  Bytes.init (List.length bytes) (fun i -> Char.chr (List.nth bytes i))

(** Write binary WASM to file *)
let write_module filename m =
  let bytes = encode_module m in
  let oc = open_out_bin filename in
  output_bytes oc bytes;
  close_out oc

(** Convert module to binary string *)
let module_to_bytes m =
  encode_module m
