(** WASM Optimization Passes (Phase 6) *)

open Wasm_types

(** {1 Optimization Levels} *)

type opt_level =
  | O0  (** No optimization *)
  | O1  (** Basic optimizations (constant folding, algebraic simplification) *)
  | O2  (** Full optimizations (+ dead code elimination) *)

(** {1 High-Level API} *)

(** Apply optimizations to a module based on optimization level *)
val optimize : ?level:opt_level -> wasm_module -> wasm_module

(** Optimize a single function *)
val optimize_func : wasm_func -> wasm_func

(** Optimize a complete module *)
val optimize_module : wasm_module -> wasm_module

(** {1 Individual Passes} *)

(** Constant folding for integer operations *)
val fold_const_int : wasm_instr -> wasm_instr

(** Constant folding for float operations *)
val fold_const_float : wasm_instr -> wasm_instr

(** Simplify algebraic identities (x+0=x, x*1=x, etc.) *)
val simplify_algebra : wasm_instr -> wasm_instr

(** Remove unreachable code after return/unreachable/br *)
val eliminate_dead_code : wasm_instr list -> wasm_instr list

(** Simplify if-then-else with constant conditions *)
val simplify_if : wasm_instr -> wasm_instr

(** Apply all optimizations to a single instruction *)
val optimize_instr : wasm_instr -> wasm_instr

(** Recursively optimize an instruction tree *)
val optimize_deep : wasm_instr -> wasm_instr
