(** WAT Text Format Emitter *)

open Wasm_types

type emit_options = {
  indent_size : int;
  emit_comments : bool;
  fold_instructions : bool;
}

val default_options : emit_options
val emit_module : wasm_module -> string
val emit_instr : wasm_instr -> string
val emit_val_type : wasm_val_type -> string
val emit_func_type : wasm_func_type -> string
val pp_module : Format.formatter -> wasm_module -> unit
val pp_instr : Format.formatter -> wasm_instr -> unit
val emit_module_with_options : emit_options -> wasm_module -> string
