(** Binary WASM Emission (Phase 6) *)

open Wasm_types

(** {1 LEB128 Encoding} *)

val encode_uleb128 : int -> int list
val encode_sleb128 : int -> int list
val encode_i32 : int32 -> int list
val encode_i64 : int64 -> int list
val encode_f32 : float -> int list
val encode_f64 : float -> int list
val encode_string : string -> int list

(** {1 Module Encoding} *)

(** Encode a complete WASM module to binary bytes *)
val encode_module : wasm_module -> bytes

(** Write binary WASM module to file *)
val write_module : string -> wasm_module -> unit

(** Convert module to binary bytes *)
val module_to_bytes : wasm_module -> bytes
