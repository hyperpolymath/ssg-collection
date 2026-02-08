(** Minimal Lambda IR for ReScript WASM-GC Backend *)

type ident = { name : string; stamp : int; }

val make_ident : string -> ident
val ident_name : ident -> string

type constant =
  | Const_int of int
  | Const_float of float
  | Const_string of string
  | Const_char of char

type comparison = Ceq | Cne | Clt | Cle | Cgt | Cge

(** Mutability flag for record fields and array elements *)
type mutable_flag = Immutable | Mutable

(** Array kind for typed arrays *)
type array_kind = Pgenarray | Paddrarray | Pintarray | Pfloatarray

(** External function call description (Phase 5) *)
type external_call = {
  prim_name : string;
  prim_arity : int;
  prim_native_name : string;
}

type primitive =
  (* Integer arithmetic *)
  | Paddint | Psubint | Pmulint | Pdivint | Pmodint | Pnegint
  | Pandint | Porint | Pxorint | Plslint | Pasrint
  | Pintcomp of comparison
  (* Float arithmetic *)
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat | Pnegfloat
  | Pfloatcomp of comparison
  (* Conversions *)
  | Pintoffloat | Pfloatofint
  (* Identity *)
  | Pidentity
  (* Records/blocks (Phase 2) *)
  | Pmakeblock of int * mutable_flag
  | Pfield of int
  | Psetfield of int * mutable_flag
  (* Arrays (Phase 2) *)
  | Pmakearray of array_kind * mutable_flag
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Variants (Phase 4) *)
  | Pisint
  | Pisout
  | Pgettag
  (* JS Interop (Phase 5) *)
  | Pccall of external_call
  | Pjs_unsafe_downgrade

type let_kind = Strict | Alias | Variable

type function_repr = {
  params : ident list;
  return_type : lambda_type option;
}

and lambda_type =
  | Tint | Tfloat | Tstring | Tunit | Tbool
  | Tfunc of lambda_type list * lambda_type
  | Tvariant of string
  | Tany

(** Switch case for pattern matching (Phase 4) *)
and switch_case = {
  sw_numconsts : int;
  sw_consts : (int * lambda) list;
  sw_numblocks : int;
  sw_blocks : (int * lambda) list;
  sw_failaction : lambda option;
}

and lambda =
  | Lconst of constant
  | Lvar of ident
  | Lapply of lambda * lambda list
  | Lfunction of function_repr * lambda
  | Llet of let_kind * ident * lambda * lambda
  | Lletrec of (ident * lambda) list * lambda
  | Lprim of primitive * lambda list
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of ident * lambda * lambda * bool * lambda
  (* Phase 4: Pattern matching *)
  | Lswitch of lambda * switch_case
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * ident list) * lambda
  | Ltrywith of lambda * ident * lambda

(** {1 Constants} *)

val const_int : int -> lambda
val const_float : float -> lambda
val const_string : string -> lambda
val const_unit : lambda
val const_true : lambda
val const_false : lambda

(** {1 Variables and Functions} *)

val var : string -> lambda
val var_id : ident -> lambda
val apply : lambda -> lambda list -> lambda
val func : string list -> lambda -> lambda
val func_typed : (string * lambda_type) list -> lambda_type -> lambda -> lambda
val let_ : string -> lambda -> lambda -> lambda
val let_id : ident -> lambda -> lambda -> lambda
val letrec : (string * lambda) list -> lambda -> lambda

(** {1 Primitives} *)

val prim : primitive -> lambda list -> lambda
val add : lambda -> lambda -> lambda
val sub : lambda -> lambda -> lambda
val mul : lambda -> lambda -> lambda
val div : lambda -> lambda -> lambda

(** {1 Records and Tuples (Phase 2)} *)

val makeblock : int -> lambda list -> lambda
val makeblock_mut : int -> lambda list -> lambda
val field : int -> lambda -> lambda
val setfield : int -> lambda -> lambda -> lambda
val tuple : lambda list -> lambda
val tuple2 : lambda -> lambda -> lambda
val tuple3 : lambda -> lambda -> lambda -> lambda

(** {1 Arrays (Phase 2)} *)

val makearray : lambda list -> lambda
val arraylength : lambda -> lambda
val arrayget : lambda -> lambda -> lambda
val arrayset : lambda -> lambda -> lambda -> lambda

(** {1 Control Flow} *)

val if_ : lambda -> lambda -> lambda -> lambda
val seq : lambda -> lambda -> lambda
val seqs : lambda list -> lambda

(** {1 Variants (Phase 4)} *)

val isint : lambda -> lambda
val gettag : lambda -> lambda
val switch : lambda -> consts:(int * lambda) list -> blocks:(int * lambda) list -> default:lambda option -> lambda
val staticraise : int -> lambda list -> lambda
val staticcatch : lambda -> int -> ident list -> lambda -> lambda
val trywith : lambda -> ident -> lambda -> lambda

(** {1 JS Interop (Phase 5)} *)

val ccall : string -> int -> string -> lambda list -> lambda
val js_call : string -> lambda list -> lambda

(** {1 Analysis} *)

val free_vars : lambda -> ident list
val is_closed : lambda -> bool

(** {1 Pretty Printing} *)

val pp_lambda : Format.formatter -> lambda -> unit
val lambda_to_string : lambda -> string

(** {1 Compatibility Layer} *)

module Compat : sig
  type t = lambda
  val of_rescript : 'a -> t
  val is_standalone : unit -> bool
end
