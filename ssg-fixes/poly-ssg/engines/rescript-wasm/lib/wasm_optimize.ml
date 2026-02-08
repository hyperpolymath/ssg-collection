(** WASM Optimization Passes (Phase 6)

    Basic optimization passes for WASM-GC code:
    - Constant folding
    - Dead code elimination
    - Instruction simplification
*)

open Wasm_types

(** {1 Constant Folding} *)

(** Fold constant integer operations *)
let rec fold_const_int instr =
  match instr with
  | I32Add (I32Const a, I32Const b) -> I32Const (Int32.add a b)
  | I32Sub (I32Const a, I32Const b) -> I32Const (Int32.sub a b)
  | I32Mul (I32Const a, I32Const b) -> I32Const (Int32.mul a b)
  | I32DivS (I32Const a, I32Const b) when b <> 0l -> I32Const (Int32.div a b)
  | I32And (I32Const a, I32Const b) -> I32Const (Int32.logand a b)
  | I32Or (I32Const a, I32Const b) -> I32Const (Int32.logor a b)
  | I32Xor (I32Const a, I32Const b) -> I32Const (Int32.logxor a b)
  | I32Eq (I32Const a, I32Const b) -> I32Const (if a = b then 1l else 0l)
  | I32Ne (I32Const a, I32Const b) -> I32Const (if a <> b then 1l else 0l)
  | I32LtS (I32Const a, I32Const b) -> I32Const (if a < b then 1l else 0l)
  | I32LeS (I32Const a, I32Const b) -> I32Const (if a <= b then 1l else 0l)
  | I32GtS (I32Const a, I32Const b) -> I32Const (if a > b then 1l else 0l)
  | I32GeS (I32Const a, I32Const b) -> I32Const (if a >= b then 1l else 0l)
  | I32Eqz (I32Const a) -> I32Const (if a = 0l then 1l else 0l)

  (* Recursive folding *)
  | I32Add (a, b) ->
      let a' = fold_const_int a in
      let b' = fold_const_int b in
      fold_const_int (I32Add (a', b'))
  | I32Sub (a, b) ->
      let a' = fold_const_int a in
      let b' = fold_const_int b in
      fold_const_int (I32Sub (a', b'))
  | I32Mul (a, b) ->
      let a' = fold_const_int a in
      let b' = fold_const_int b in
      fold_const_int (I32Mul (a', b'))

  | _ -> instr

(** Fold constant float operations *)
let rec fold_const_float instr =
  match instr with
  | F64Add (F64Const a, F64Const b) -> F64Const (a +. b)
  | F64Sub (F64Const a, F64Const b) -> F64Const (a -. b)
  | F64Mul (F64Const a, F64Const b) -> F64Const (a *. b)
  | F64Div (F64Const a, F64Const b) when b <> 0.0 -> F64Const (a /. b)
  | F64Neg (F64Const a) -> F64Const (-. a)
  | F64Eq (F64Const a, F64Const b) -> I32Const (if a = b then 1l else 0l)
  | F64Ne (F64Const a, F64Const b) -> I32Const (if a <> b then 1l else 0l)
  | F64Lt (F64Const a, F64Const b) -> I32Const (if a < b then 1l else 0l)
  | F64Le (F64Const a, F64Const b) -> I32Const (if a <= b then 1l else 0l)
  | F64Gt (F64Const a, F64Const b) -> I32Const (if a > b then 1l else 0l)
  | F64Ge (F64Const a, F64Const b) -> I32Const (if a >= b then 1l else 0l)

  | F64Add (a, b) ->
      let a' = fold_const_float a in
      let b' = fold_const_float b in
      fold_const_float (F64Add (a', b'))
  | F64Sub (a, b) ->
      let a' = fold_const_float a in
      let b' = fold_const_float b in
      fold_const_float (F64Sub (a', b'))
  | F64Mul (a, b) ->
      let a' = fold_const_float a in
      let b' = fold_const_float b in
      fold_const_float (F64Mul (a', b'))
  | F64Div (a, b) ->
      let a' = fold_const_float a in
      let b' = fold_const_float b in
      fold_const_float (F64Div (a', b'))

  | _ -> instr

(** {1 Algebraic Simplification} *)

(** Simplify algebraic identities *)
let rec simplify_algebra instr =
  match instr with
  (* x + 0 = x *)
  | I32Add (x, I32Const 0l) -> simplify_algebra x
  | I32Add (I32Const 0l, x) -> simplify_algebra x
  (* x - 0 = x *)
  | I32Sub (x, I32Const 0l) -> simplify_algebra x
  (* x * 1 = x *)
  | I32Mul (x, I32Const 1l) -> simplify_algebra x
  | I32Mul (I32Const 1l, x) -> simplify_algebra x
  (* x * 0 = 0 *)
  | I32Mul (_, I32Const 0l) -> I32Const 0l
  | I32Mul (I32Const 0l, _) -> I32Const 0l
  (* x / 1 = x *)
  | I32DivS (x, I32Const 1l) -> simplify_algebra x
  (* x & 0 = 0 *)
  | I32And (_, I32Const 0l) -> I32Const 0l
  | I32And (I32Const 0l, _) -> I32Const 0l
  (* x | 0 = x *)
  | I32Or (x, I32Const 0l) -> simplify_algebra x
  | I32Or (I32Const 0l, x) -> simplify_algebra x
  (* x ^ 0 = x *)
  | I32Xor (x, I32Const 0l) -> simplify_algebra x
  | I32Xor (I32Const 0l, x) -> simplify_algebra x

  (* Float identities *)
  | F64Add (x, F64Const 0.0) -> simplify_algebra x
  | F64Add (F64Const 0.0, x) -> simplify_algebra x
  | F64Sub (x, F64Const 0.0) -> simplify_algebra x
  | F64Mul (x, F64Const 1.0) -> simplify_algebra x
  | F64Mul (F64Const 1.0, x) -> simplify_algebra x
  | F64Mul (_, F64Const 0.0) -> F64Const 0.0
  | F64Mul (F64Const 0.0, _) -> F64Const 0.0
  | F64Div (x, F64Const 1.0) -> simplify_algebra x
  | F64Neg (F64Neg x) -> simplify_algebra x

  | _ -> instr

(** {1 Dead Code Elimination} *)

(** Remove unreachable code after return/unreachable *)
let rec eliminate_dead_code instrs =
  match instrs with
  | [] -> []
  | (Return _ as ret) :: _ -> [ret]  (* Everything after return is dead *)
  | Unreachable :: _ -> [Unreachable]  (* Everything after unreachable is dead *)
  | Br n :: _ -> [Br n]  (* Everything after unconditional br is dead *)
  | instr :: rest -> instr :: eliminate_dead_code rest

(** {1 Control Flow Simplification} *)

(** Simplify if-then-else with constant condition *)
let simplify_if instr =
  match instr with
  | If (I32Const 0l, _, else_) -> Seq else_  (* false: take else branch *)
  | If (I32Const _, then_, _) -> Seq then_   (* non-zero: take then branch *)
  | If (I32Eqz (I32Const 0l), then_, _) -> Seq then_  (* eqz(0) = true *)
  | If (I32Eqz (I32Const _), _, else_) -> Seq else_   (* eqz(n) = false *)
  | _ -> instr

(** {1 Instruction Transformation} *)

(** Apply all optimizations to a single instruction *)
let optimize_instr instr =
  instr
  |> fold_const_int
  |> fold_const_float
  |> simplify_algebra
  |> simplify_if

(** Recursively optimize an instruction tree *)
let rec optimize_deep instr =
  let optimized = match instr with
    | Block (label, body) -> Block (label, List.map optimize_deep body |> eliminate_dead_code)
    | Loop (label, body) -> Loop (label, List.map optimize_deep body)
    | If (cond, then_, else_) ->
        If (optimize_deep cond,
            List.map optimize_deep then_ |> eliminate_dead_code,
            List.map optimize_deep else_ |> eliminate_dead_code)
    | Seq instrs -> Seq (List.map optimize_deep instrs |> eliminate_dead_code)
    | LocalSet (idx, e) -> LocalSet (idx, optimize_deep e)
    | LocalTee (idx, e) -> LocalTee (idx, optimize_deep e)
    | GlobalSet (name, e) -> GlobalSet (name, optimize_deep e)
    | BrIf (n, cond) -> BrIf (n, optimize_deep cond)
    | Return (Some e) -> Return (Some (optimize_deep e))
    | Call (name, args) -> Call (name, List.map optimize_deep args)
    | CallRef (ft, fn, args) -> CallRef (ft, optimize_deep fn, List.map optimize_deep args)
    | StructNew (name, fields) -> StructNew (name, List.map optimize_deep fields)
    | StructGet (sn, fn, obj) -> StructGet (sn, fn, optimize_deep obj)
    | StructSet (sn, fn, obj, v) -> StructSet (sn, fn, optimize_deep obj, optimize_deep v)
    | ArrayNew (name, init, len) -> ArrayNew (name, optimize_deep init, optimize_deep len)
    | ArrayNewFixed (name, elems) -> ArrayNewFixed (name, List.map optimize_deep elems)
    | ArrayNewDefault (name, len) -> ArrayNewDefault (name, optimize_deep len)
    | ArrayGet (name, arr, idx) -> ArrayGet (name, optimize_deep arr, optimize_deep idx)
    | ArrayGetS (name, arr, idx) -> ArrayGetS (name, optimize_deep arr, optimize_deep idx)
    | ArraySet (name, arr, idx, v) -> ArraySet (name, optimize_deep arr, optimize_deep idx, optimize_deep v)
    | ArrayLen arr -> ArrayLen (optimize_deep arr)
    | ArrayCopy (dst, src, d, di, s, si, l) ->
        ArrayCopy (dst, src, optimize_deep d, optimize_deep di,
                   optimize_deep s, optimize_deep si, optimize_deep l)
    | RefCast (ht, e) -> RefCast (ht, optimize_deep e)
    | RefTest (ht, e) -> RefTest (ht, optimize_deep e)
    | RefIsNull e -> RefIsNull (optimize_deep e)
    | RefAsNonNull e -> RefAsNonNull (optimize_deep e)
    | RefEq (a, b) -> RefEq (optimize_deep a, optimize_deep b)
    | RefI31 e -> RefI31 (optimize_deep e)
    | I31GetS e -> I31GetS (optimize_deep e)
    | I31GetU e -> I31GetU (optimize_deep e)
    | Drop e -> Drop (optimize_deep e)
    | I32Add (a, b) -> I32Add (optimize_deep a, optimize_deep b)
    | I32Sub (a, b) -> I32Sub (optimize_deep a, optimize_deep b)
    | I32Mul (a, b) -> I32Mul (optimize_deep a, optimize_deep b)
    | I32DivS (a, b) -> I32DivS (optimize_deep a, optimize_deep b)
    | I32RemS (a, b) -> I32RemS (optimize_deep a, optimize_deep b)
    | I32And (a, b) -> I32And (optimize_deep a, optimize_deep b)
    | I32Or (a, b) -> I32Or (optimize_deep a, optimize_deep b)
    | I32Xor (a, b) -> I32Xor (optimize_deep a, optimize_deep b)
    | I32Shl (a, b) -> I32Shl (optimize_deep a, optimize_deep b)
    | I32ShrS (a, b) -> I32ShrS (optimize_deep a, optimize_deep b)
    | I32Eq (a, b) -> I32Eq (optimize_deep a, optimize_deep b)
    | I32Ne (a, b) -> I32Ne (optimize_deep a, optimize_deep b)
    | I32LtS (a, b) -> I32LtS (optimize_deep a, optimize_deep b)
    | I32LeS (a, b) -> I32LeS (optimize_deep a, optimize_deep b)
    | I32GtS (a, b) -> I32GtS (optimize_deep a, optimize_deep b)
    | I32GeS (a, b) -> I32GeS (optimize_deep a, optimize_deep b)
    | I32LtU (a, b) -> I32LtU (optimize_deep a, optimize_deep b)
    | I32LeU (a, b) -> I32LeU (optimize_deep a, optimize_deep b)
    | I32GtU (a, b) -> I32GtU (optimize_deep a, optimize_deep b)
    | I32GeU (a, b) -> I32GeU (optimize_deep a, optimize_deep b)
    | I32Eqz e -> I32Eqz (optimize_deep e)
    | F64Add (a, b) -> F64Add (optimize_deep a, optimize_deep b)
    | F64Sub (a, b) -> F64Sub (optimize_deep a, optimize_deep b)
    | F64Mul (a, b) -> F64Mul (optimize_deep a, optimize_deep b)
    | F64Div (a, b) -> F64Div (optimize_deep a, optimize_deep b)
    | F64Neg e -> F64Neg (optimize_deep e)
    | F64Eq (a, b) -> F64Eq (optimize_deep a, optimize_deep b)
    | F64Ne (a, b) -> F64Ne (optimize_deep a, optimize_deep b)
    | F64Lt (a, b) -> F64Lt (optimize_deep a, optimize_deep b)
    | F64Le (a, b) -> F64Le (optimize_deep a, optimize_deep b)
    | F64Gt (a, b) -> F64Gt (optimize_deep a, optimize_deep b)
    | F64Ge (a, b) -> F64Ge (optimize_deep a, optimize_deep b)
    | BrTable (labels, default, idx) -> BrTable (labels, default, optimize_deep idx)
    | other -> other
  in
  optimize_instr optimized

(** {1 Function and Module Optimization} *)

(** Optimize a single function *)
let optimize_func func =
  { func with func_body = optimize_deep func.func_body }

(** Optimize a complete module *)
let optimize_module m =
  { m with mod_funcs = List.map optimize_func m.mod_funcs }

(** {1 Optimization Levels} *)

type opt_level =
  | O0  (** No optimization *)
  | O1  (** Basic optimizations (constant folding, algebraic simplification) *)
  | O2  (** Full optimizations (+ dead code elimination) *)

(** Apply optimizations based on level *)
let optimize ?(level=O1) m =
  match level with
  | O0 -> m
  | O1 | O2 -> optimize_module m
