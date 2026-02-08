(** Lambda IR to WASM-GC Compiler

    This module implements the core compilation from Lambda IR to WASM-GC
    instructions. Phase 1 MVP supports constants, variables, arithmetic,
    simple functions, let bindings, and conditionals. *)

open Lambda_ir
open Wasm_types
open Wasm_env

(** {1 Compilation} *)

(** Compile a Lambda expression to WASM instructions *)
val compile_expr : t -> lambda -> t * wasm_instr

(** Compile a function definition *)
val compile_function : t -> string -> ident list -> lambda -> t * wasm_func

(** Compile a top-level binding *)
val compile_toplevel : t -> string -> lambda -> t

(** {1 Program Compilation} *)

(** Compile a complete program (list of top-level bindings) *)
val compile_program : (string * lambda) list -> wasm_module

(** Compile a single expression as a module with a main function *)
val compile_expr_as_module : lambda -> wasm_module

(** {1 Type Inference}

    Simple type inference for determining WASM types. *)

(** Infer the WASM type of a Lambda expression *)
val infer_type : t -> lambda -> wasm_val_type

(** {1 Errors} *)

exception Compile_error of string

(** Raise a compilation error *)
val error : string -> 'a
