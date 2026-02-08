(** Compilation Environment *)

open Lambda_ir
open Wasm_types

type t

val empty : unit -> t
val alloc_local : t -> ident -> wasm_val_type -> t * int
val lookup_local : t -> ident -> int option
val lookup_local_exn : t -> ident -> int
val bind_local : t -> ident -> int -> wasm_val_type -> t
val get_locals : t -> (string * wasm_val_type) list
val num_locals : t -> int

val add_global : t -> string -> wasm_val_type -> bool -> wasm_instr -> t
val lookup_global : t -> string -> bool
val get_globals : t -> wasm_global list

val add_function : t -> wasm_func -> t
val get_functions : t -> wasm_func list
val fresh_func_name : t -> string -> t * string

val add_struct_type : t -> wasm_struct_type -> t
val add_array_type : t -> wasm_array_type -> t
val add_func_type : t -> string -> wasm_func_type -> t
val get_types : t -> wasm_type_def list
val lookup_struct : t -> string -> wasm_struct_type option
val lookup_array : t -> string -> wasm_array_type option

val add_import : t -> wasm_import -> t
val get_imports : t -> wasm_import list
val ensure_js_import : t -> string -> string -> string -> int -> t
val clear_import_cache : unit -> unit

val enter_scope : t -> t
val exit_scope : t -> t

val to_module : t -> wasm_module
val with_standard_types : t -> t
val string_type_name : string
val unit_type_name : string
val variant_type_name : string
