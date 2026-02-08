(** WAT Text Format Emitter *)

open Wasm_types

type emit_options = {
  indent_size : int;
  emit_comments : bool;
  fold_instructions : bool;
}

let default_options = {
  indent_size = 2;
  emit_comments = false;
  fold_instructions = true;
}

let indent n = String.make n ' '

let rec emit_heap_type = function
  | Func ft -> Printf.sprintf "(func %s)" (emit_func_type_inline ft)
  | Struct name -> name
  | Array name -> name
  | Extern -> "extern"
  | Any -> "any"
  | Eq -> "eq"
  | I31 -> "i31"
  | None_ -> "none"

and emit_val_type = function
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"
  | Ref ht -> Printf.sprintf "(ref %s)" (emit_heap_type ht)
  | RefNull ht -> Printf.sprintf "(ref null %s)" (emit_heap_type ht)

and emit_func_type_inline ft =
  let params = String.concat " " (List.map (fun t -> Printf.sprintf "(param %s)" (emit_val_type t)) ft.ft_params) in
  let results = String.concat " " (List.map (fun t -> Printf.sprintf "(result %s)" (emit_val_type t)) ft.ft_results) in
  String.concat " " (List.filter (fun s -> s <> "") [params; results])

let emit_func_type ft = Printf.sprintf "(func %s)" (emit_func_type_inline ft)

let emit_field f =
  let mut = if f.field_mutable then "(mut " else "" in
  let mut_close = if f.field_mutable then ")" else "" in
  Printf.sprintf "(field $%s %s%s%s)" f.field_name mut (emit_val_type f.field_type) mut_close

let emit_struct_type st =
  let super = match st.struct_supertype with Some s -> Printf.sprintf "(sub %s " s | None -> "" in
  let super_close = if st.struct_supertype <> None then ")" else "" in
  let fields = String.concat " " (List.map emit_field st.struct_fields) in
  Printf.sprintf "(type %s %s(struct %s)%s)" st.struct_name super fields super_close

let emit_array_type at =
  let mut = if at.array_elem_mutable then "(mut " else "" in
  let mut_close = if at.array_elem_mutable then ")" else "" in
  Printf.sprintf "(type %s (array %s%s%s))" at.array_name mut (emit_val_type at.array_elem_type) mut_close

let emit_func_type_def name ft =
  Printf.sprintf "(type %s (func %s))" name (emit_func_type_inline ft)

let emit_type_def = function
  | TypeStruct st -> emit_struct_type st
  | TypeArray at -> emit_array_type at
  | TypeFunc (name, ft) -> emit_func_type_def name ft

let rec emit_instr_inner depth instr =
  let ind = indent (depth * 2) in
  let emit = emit_instr_inner depth in
  let emit_nested = emit_instr_inner (depth + 1) in
  match instr with
  | I32Const n -> Printf.sprintf "(i32.const %ld)" n
  | I64Const n -> Printf.sprintf "(i64.const %Ld)" n
  | F32Const f -> Printf.sprintf "(f32.const %g)" f
  | F64Const f -> Printf.sprintf "(f64.const %g)" f
  | RefNull ht -> Printf.sprintf "(ref.null %s)" (emit_heap_type ht)
  | RefI31 e -> Printf.sprintf "(ref.i31 %s)" (emit e)
  | RefFunc name -> Printf.sprintf "(ref.func %s)" name
  | LocalGet idx -> Printf.sprintf "(local.get %d)" idx
  | LocalSet (idx, e) -> Printf.sprintf "(local.set %d %s)" idx (emit e)
  | LocalTee (idx, e) -> Printf.sprintf "(local.tee %d %s)" idx (emit e)
  | GlobalGet name -> Printf.sprintf "(global.get %s)" name
  | GlobalSet (name, e) -> Printf.sprintf "(global.set %s %s)" name (emit e)
  | I32Add (a, b) -> Printf.sprintf "(i32.add %s %s)" (emit a) (emit b)
  | I32Sub (a, b) -> Printf.sprintf "(i32.sub %s %s)" (emit a) (emit b)
  | I32Mul (a, b) -> Printf.sprintf "(i32.mul %s %s)" (emit a) (emit b)
  | I32DivS (a, b) -> Printf.sprintf "(i32.div_s %s %s)" (emit a) (emit b)
  | I32RemS (a, b) -> Printf.sprintf "(i32.rem_s %s %s)" (emit a) (emit b)
  | I32And (a, b) -> Printf.sprintf "(i32.and %s %s)" (emit a) (emit b)
  | I32Or (a, b) -> Printf.sprintf "(i32.or %s %s)" (emit a) (emit b)
  | I32Xor (a, b) -> Printf.sprintf "(i32.xor %s %s)" (emit a) (emit b)
  | I32Shl (a, b) -> Printf.sprintf "(i32.shl %s %s)" (emit a) (emit b)
  | I32ShrS (a, b) -> Printf.sprintf "(i32.shr_s %s %s)" (emit a) (emit b)
  | I32Eq (a, b) -> Printf.sprintf "(i32.eq %s %s)" (emit a) (emit b)
  | I32Ne (a, b) -> Printf.sprintf "(i32.ne %s %s)" (emit a) (emit b)
  | I32LtS (a, b) -> Printf.sprintf "(i32.lt_s %s %s)" (emit a) (emit b)
  | I32LeS (a, b) -> Printf.sprintf "(i32.le_s %s %s)" (emit a) (emit b)
  | I32GtS (a, b) -> Printf.sprintf "(i32.gt_s %s %s)" (emit a) (emit b)
  | I32GeS (a, b) -> Printf.sprintf "(i32.ge_s %s %s)" (emit a) (emit b)
  | I32LtU (a, b) -> Printf.sprintf "(i32.lt_u %s %s)" (emit a) (emit b)
  | I32LeU (a, b) -> Printf.sprintf "(i32.le_u %s %s)" (emit a) (emit b)
  | I32GtU (a, b) -> Printf.sprintf "(i32.gt_u %s %s)" (emit a) (emit b)
  | I32GeU (a, b) -> Printf.sprintf "(i32.ge_u %s %s)" (emit a) (emit b)
  | I32Eqz e -> Printf.sprintf "(i32.eqz %s)" (emit e)
  | F64Add (a, b) -> Printf.sprintf "(f64.add %s %s)" (emit a) (emit b)
  | F64Sub (a, b) -> Printf.sprintf "(f64.sub %s %s)" (emit a) (emit b)
  | F64Mul (a, b) -> Printf.sprintf "(f64.mul %s %s)" (emit a) (emit b)
  | F64Div (a, b) -> Printf.sprintf "(f64.div %s %s)" (emit a) (emit b)
  | F64Neg e -> Printf.sprintf "(f64.neg %s)" (emit e)
  | F64Eq (a, b) -> Printf.sprintf "(f64.eq %s %s)" (emit a) (emit b)
  | F64Ne (a, b) -> Printf.sprintf "(f64.ne %s %s)" (emit a) (emit b)
  | F64Lt (a, b) -> Printf.sprintf "(f64.lt %s %s)" (emit a) (emit b)
  | F64Le (a, b) -> Printf.sprintf "(f64.le %s %s)" (emit a) (emit b)
  | F64Gt (a, b) -> Printf.sprintf "(f64.gt %s %s)" (emit a) (emit b)
  | F64Ge (a, b) -> Printf.sprintf "(f64.ge %s %s)" (emit a) (emit b)
  | Block (label, body) ->
      let lbl = match label with Some l -> " " ^ l | None -> "" in
      let body_str = String.concat ("\n" ^ ind ^ "  ") (List.map emit_nested body) in
      Printf.sprintf "(block%s\n%s  %s\n%s)" lbl ind body_str ind
  | Loop (label, body) ->
      let lbl = match label with Some l -> " " ^ l | None -> "" in
      let body_str = String.concat ("\n" ^ ind ^ "  ") (List.map emit_nested body) in
      Printf.sprintf "(loop%s\n%s  %s\n%s)" lbl ind body_str ind
  | If (cond, then_, else_) ->
      let then_str = String.concat ("\n" ^ ind ^ "    ") (List.map emit_nested then_) in
      let else_str = String.concat ("\n" ^ ind ^ "    ") (List.map emit_nested else_) in
      Printf.sprintf "(if %s\n%s  (then\n%s    %s\n%s  )\n%s  (else\n%s    %s\n%s  )\n%s)"
        (emit cond) ind ind then_str ind ind ind else_str ind ind
  | Br n -> Printf.sprintf "(br %d)" n
  | BrIf (n, cond) -> Printf.sprintf "(br_if %d %s)" n (emit cond)
  | BrTable (labels, default, idx) ->
      let labels_str = String.concat " " (List.map string_of_int labels) in
      Printf.sprintf "(br_table %s %d %s)" labels_str default (emit idx)
  | Return None -> "(return)"
  | Return (Some e) -> Printf.sprintf "(return %s)" (emit e)
  | Call (name, args) ->
      let args_str = String.concat " " (List.map emit args) in
      if args_str = "" then Printf.sprintf "(call %s)" name
      else Printf.sprintf "(call %s %s)" name args_str
  | CallRef (ft, fn, args) ->
      let args_str = String.concat " " (List.map emit args) in
      Printf.sprintf "(call_ref %s %s %s)" (emit_func_type ft) args_str (emit fn)
  | Unreachable -> "(unreachable)"
  | StructNew (name, fields) ->
      let fields_str = String.concat " " (List.map emit fields) in
      if fields_str = "" then Printf.sprintf "(struct.new %s)" name
      else Printf.sprintf "(struct.new %s %s)" name fields_str
  | StructGet (sn, fn, obj) -> Printf.sprintf "(struct.get %s $%s %s)" sn fn (emit obj)
  | StructSet (sn, fn, obj, v) -> Printf.sprintf "(struct.set %s $%s %s %s)" sn fn (emit obj) (emit v)
  | ArrayNew (name, init, len) -> Printf.sprintf "(array.new %s %s %s)" name (emit init) (emit len)
  | ArrayNewFixed (name, elems) ->
      let elems_str = String.concat " " (List.map emit elems) in
      Printf.sprintf "(array.new_fixed %s %d %s)" name (List.length elems) elems_str
  | ArrayNewDefault (name, len) -> Printf.sprintf "(array.new_default %s %s)" name (emit len)
  | ArrayGet (name, arr, idx) -> Printf.sprintf "(array.get %s %s %s)" name (emit arr) (emit idx)
  | ArrayGetS (name, arr, idx) -> Printf.sprintf "(array.get_s %s %s %s)" name (emit arr) (emit idx)
  | ArraySet (name, arr, idx, v) -> Printf.sprintf "(array.set %s %s %s %s)" name (emit arr) (emit idx) (emit v)
  | ArrayLen arr -> Printf.sprintf "(array.len %s)" (emit arr)
  | ArrayCopy (dt, st, d, di, s, si, l) ->
      Printf.sprintf "(array.copy %s %s %s %s %s %s %s)" dt st (emit d) (emit di) (emit s) (emit si) (emit l)
  | RefCast (ht, e) -> Printf.sprintf "(ref.cast (ref %s) %s)" (emit_heap_type ht) (emit e)
  | RefTest (ht, e) -> Printf.sprintf "(ref.test (ref %s) %s)" (emit_heap_type ht) (emit e)
  | RefIsNull e -> Printf.sprintf "(ref.is_null %s)" (emit e)
  | RefAsNonNull e -> Printf.sprintf "(ref.as_non_null %s)" (emit e)
  | RefEq (a, b) -> Printf.sprintf "(ref.eq %s %s)" (emit a) (emit b)
  | I31GetS e -> Printf.sprintf "(i31.get_s %s)" (emit e)
  | I31GetU e -> Printf.sprintf "(i31.get_u %s)" (emit e)
  | Seq instrs -> String.concat "\n" (List.map (fun i -> ind ^ emit_nested i) instrs)
  | Drop e -> Printf.sprintf "(drop %s)" (emit e)
  | Nop -> "(nop)"

let emit_instr instr = emit_instr_inner 0 instr

let emit_import imp =
  let desc = match imp.import_desc with
    | ImportFunc ft -> Printf.sprintf "(func %s %s)" imp.import_local_name (emit_func_type_inline ft)
    | ImportGlobal (typ, is_mut) ->
        let mut = if is_mut then "(mut " else "" in
        let mut_close = if is_mut then ")" else "" in
        Printf.sprintf "(global %s %s%s%s)" imp.import_local_name mut (emit_val_type typ) mut_close
  in
  Printf.sprintf "  (import \"%s\" \"%s\" %s)" imp.import_module imp.import_name desc

let emit_func func =
  let params = List.mapi (fun _ (_, t) -> Printf.sprintf "(param %s)" (emit_val_type t)) func.func_locals in
  let results = List.map (fun t -> Printf.sprintf "(result %s)" (emit_val_type t)) func.func_type.ft_results in
  let num_params = List.length func.func_type.ft_params in
  let additional_locals =
    if List.length func.func_locals > num_params then
      List.filteri (fun i _ -> i >= num_params) func.func_locals
    else [] in
  let locals_decl = List.map (fun (name, t) ->
    Printf.sprintf "(local $%s %s)" name (emit_val_type t)) additional_locals in
  let export = match func.func_export with Some name -> Printf.sprintf " (export \"%s\")" name | None -> "" in
  let signature = String.concat " " (List.filter (fun s -> s <> "") (params @ results)) in
  let locals_str = if locals_decl = [] then "" else "\n    " ^ String.concat "\n    " locals_decl in
  let body = emit_instr_inner 2 func.func_body in
  Printf.sprintf "  (func %s%s %s%s\n    %s\n  )" func.func_name export signature locals_str body

let emit_global global =
  let mut = if global.global_mutable then "(mut " else "" in
  let mut_close = if global.global_mutable then ")" else "" in
  let export = match global.global_export with Some name -> Printf.sprintf " (export \"%s\")" name | None -> "" in
  Printf.sprintf "  (global %s%s %s%s%s %s)"
    global.global_name export mut (emit_val_type global.global_type) mut_close (emit_instr global.global_init)

let emit_module_with_options _opts m =
  let types = List.map (fun t -> "  " ^ emit_type_def t) m.mod_types in
  let imports = List.map emit_import m.mod_imports in
  let funcs = List.map emit_func m.mod_funcs in
  let globals = List.map emit_global m.mod_globals in
  let sections = List.filter (fun s -> s <> []) [types; imports; globals; funcs] in
  let body = String.concat "\n\n" (List.map (String.concat "\n") sections) in
  Printf.sprintf "(module\n%s\n)" body

let emit_module m = emit_module_with_options default_options m

let pp_instr fmt instr = Format.fprintf fmt "%s" (emit_instr instr)
let pp_module fmt m = Format.fprintf fmt "%s" (emit_module m)
