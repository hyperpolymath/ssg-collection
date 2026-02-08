(** Compilation Environment *)

open Lambda_ir
open Wasm_types

type t = {
  locals : (int, int * wasm_val_type) Hashtbl.t;
  local_list : (string * wasm_val_type) list ref;
  next_local : int ref;
  globals : (string, wasm_global) Hashtbl.t;
  global_list : wasm_global list ref;
  functions : wasm_func list ref;
  func_counter : int ref;
  struct_types : (string, wasm_struct_type) Hashtbl.t;
  array_types : (string, wasm_array_type) Hashtbl.t;
  func_types : (string, wasm_func_type) Hashtbl.t;
  type_list : wasm_type_def list ref;
  imports : wasm_import list ref;
  parent_locals : (int, int * wasm_val_type) Hashtbl.t option;
  parent_next_local : int option;
}

let empty () = {
  locals = Hashtbl.create 32;
  local_list = ref [];
  next_local = ref 0;
  globals = Hashtbl.create 16;
  global_list = ref [];
  functions = ref [];
  func_counter = ref 0;
  struct_types = Hashtbl.create 16;
  array_types = Hashtbl.create 8;
  func_types = Hashtbl.create 16;
  type_list = ref [];
  imports = ref [];
  parent_locals = None;
  parent_next_local = None;
}

let alloc_local env id typ =
  let idx = !(env.next_local) in
  Hashtbl.add env.locals id.stamp (idx, typ);
  env.local_list := !(env.local_list) @ [(id.name, typ)];
  incr env.next_local;
  (env, idx)

let lookup_local env id =
  match Hashtbl.find_opt env.locals id.stamp with
  | Some (idx, _) -> Some idx
  | None -> None

let lookup_local_exn env id =
  match lookup_local env id with
  | Some idx -> idx
  | None -> failwith (Printf.sprintf "Unbound local variable: %s" id.name)

(** Bind an identifier to an existing local index (for closure captures) *)
let bind_local env id idx typ =
  Hashtbl.add env.locals id.stamp (idx, typ);
  env

let get_locals env = !(env.local_list)
let num_locals env = !(env.next_local)

let add_global env name typ is_mutable init =
  let global = {
    global_name = name; global_type = typ; global_mutable = is_mutable;
    global_init = init; global_export = None;
  } in
  Hashtbl.add env.globals name global;
  env.global_list := global :: !(env.global_list);
  env

let lookup_global env name = Hashtbl.mem env.globals name
let get_globals env = List.rev !(env.global_list)

let add_function env func =
  env.functions := func :: !(env.functions);
  env

let get_functions env = List.rev !(env.functions)

let fresh_func_name env prefix =
  let n = !(env.func_counter) in
  incr env.func_counter;
  (env, Printf.sprintf "$%s_%d" prefix n)

let add_struct_type env st =
  if not (Hashtbl.mem env.struct_types st.struct_name) then begin
    Hashtbl.add env.struct_types st.struct_name st;
    env.type_list := TypeStruct st :: !(env.type_list)
  end;
  env

let add_array_type env at =
  if not (Hashtbl.mem env.array_types at.array_name) then begin
    Hashtbl.add env.array_types at.array_name at;
    env.type_list := TypeArray at :: !(env.type_list)
  end;
  env

let add_func_type env name ft =
  if not (Hashtbl.mem env.func_types name) then begin
    Hashtbl.add env.func_types name ft;
    env.type_list := TypeFunc (name, ft) :: !(env.type_list)
  end;
  env

let get_types env = List.rev !(env.type_list)
let lookup_struct env name = Hashtbl.find_opt env.struct_types name
let lookup_array env name = Hashtbl.find_opt env.array_types name

let add_import env imp =
  env.imports := imp :: !(env.imports);
  env

let get_imports env = List.rev !(env.imports)

let enter_scope env =
  let saved_locals = Hashtbl.copy env.locals in
  let saved_next = !(env.next_local) in
  { env with
    locals = Hashtbl.create 32;
    local_list = ref [];
    next_local = ref 0;
    parent_locals = Some saved_locals;
    parent_next_local = Some saved_next;
  }

let exit_scope env =
  match env.parent_locals, env.parent_next_local with
  | Some pl, Some pn ->
      { env with locals = pl; next_local = ref pn;
        parent_locals = None; parent_next_local = None; }
  | _ -> env

let to_module env = {
  mod_types = get_types env;
  mod_imports = get_imports env;
  mod_funcs = get_functions env;
  mod_globals = get_globals env;
  mod_exports = [];
  mod_start = None;
}

let string_type_name = "$string"
let unit_type_name = "$unit"
let variant_type_name = "$variant"

(** Track which imports have been registered to avoid duplicates *)
let registered_imports : (string, bool) Hashtbl.t = Hashtbl.create 16

(** Ensure a JS import exists for an external function *)
let ensure_js_import env module_name func_name local_name arity =
  let key = Printf.sprintf "%s.%s" module_name func_name in
  if not (Hashtbl.mem registered_imports key) then begin
    Hashtbl.add registered_imports key true;
    let param_types = List.init arity (fun _ -> Ref Any) in
    let ft = { ft_params = param_types; ft_results = [Ref Any] } in
    let imp = {
      import_module = module_name;
      import_name = func_name;
      import_local_name = local_name;
      import_desc = ImportFunc ft;
    } in
    add_import env imp
  end else
    env

(** Clear registered imports (for testing) *)
let clear_import_cache () = Hashtbl.clear registered_imports

let with_standard_types env =
  let string_array = {
    array_name = string_type_name;
    array_elem_type = I32;
    array_elem_mutable = true;
  } in
  let unit_struct = {
    struct_name = unit_type_name;
    struct_fields = [];
    struct_supertype = None;
  } in
  (* Variant struct for tagged unions - tag + data *)
  let variant_struct = {
    struct_name = variant_type_name;
    struct_fields = [
      { field_name = "tag"; field_type = I32; field_mutable = false };
      { field_name = "data"; field_type = Ref Any; field_mutable = false };
    ];
    struct_supertype = None;
  } in
  env |> fun e -> add_array_type e string_array
      |> fun e -> add_struct_type e unit_struct
      |> fun e -> add_struct_type e variant_struct
