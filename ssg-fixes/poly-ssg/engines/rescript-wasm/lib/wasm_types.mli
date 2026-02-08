(** WASM-GC Type Definitions *)

type wasm_val_type =
  | I32
  | I64
  | F32
  | F64
  | Ref of wasm_heap_type
  | RefNull of wasm_heap_type

and wasm_heap_type =
  | Func of wasm_func_type
  | Struct of string
  | Array of string
  | Extern
  | Any
  | Eq
  | I31
  | None_

and wasm_func_type = {
  ft_params : wasm_val_type list;
  ft_results : wasm_val_type list;
}

type wasm_field = {
  field_name : string;
  field_type : wasm_val_type;
  field_mutable : bool;
}

type wasm_struct_type = {
  struct_name : string;
  struct_fields : wasm_field list;
  struct_supertype : string option;
}

type wasm_array_type = {
  array_name : string;
  array_elem_type : wasm_val_type;
  array_elem_mutable : bool;
}

type wasm_type_def =
  | TypeStruct of wasm_struct_type
  | TypeArray of wasm_array_type
  | TypeFunc of string * wasm_func_type

type comparison = Eq | Ne | Lt | Le | Gt | Ge

type wasm_instr =
  | I32Const of int32
  | I64Const of int64
  | F32Const of float
  | F64Const of float
  | RefNull of wasm_heap_type
  | RefI31 of wasm_instr
  | RefFunc of string
  | LocalGet of int
  | LocalSet of int * wasm_instr
  | LocalTee of int * wasm_instr
  | GlobalGet of string
  | GlobalSet of string * wasm_instr
  | I32Add of wasm_instr * wasm_instr
  | I32Sub of wasm_instr * wasm_instr
  | I32Mul of wasm_instr * wasm_instr
  | I32DivS of wasm_instr * wasm_instr
  | I32RemS of wasm_instr * wasm_instr
  | I32And of wasm_instr * wasm_instr
  | I32Or of wasm_instr * wasm_instr
  | I32Xor of wasm_instr * wasm_instr
  | I32Shl of wasm_instr * wasm_instr
  | I32ShrS of wasm_instr * wasm_instr
  | I32Eq of wasm_instr * wasm_instr
  | I32Ne of wasm_instr * wasm_instr
  | I32LtS of wasm_instr * wasm_instr
  | I32LeS of wasm_instr * wasm_instr
  | I32GtS of wasm_instr * wasm_instr
  | I32GeS of wasm_instr * wasm_instr
  | I32LtU of wasm_instr * wasm_instr
  | I32LeU of wasm_instr * wasm_instr
  | I32GtU of wasm_instr * wasm_instr
  | I32GeU of wasm_instr * wasm_instr
  | I32Eqz of wasm_instr
  | F64Add of wasm_instr * wasm_instr
  | F64Sub of wasm_instr * wasm_instr
  | F64Mul of wasm_instr * wasm_instr
  | F64Div of wasm_instr * wasm_instr
  | F64Neg of wasm_instr
  | F64Eq of wasm_instr * wasm_instr
  | F64Ne of wasm_instr * wasm_instr
  | F64Lt of wasm_instr * wasm_instr
  | F64Le of wasm_instr * wasm_instr
  | F64Gt of wasm_instr * wasm_instr
  | F64Ge of wasm_instr * wasm_instr
  | Block of string option * wasm_instr list
  | Loop of string option * wasm_instr list
  | If of wasm_instr * wasm_instr list * wasm_instr list
  | Br of int
  | BrIf of int * wasm_instr
  | BrTable of int list * int * wasm_instr
  | Return of wasm_instr option
  | Call of string * wasm_instr list
  | CallRef of wasm_func_type * wasm_instr * wasm_instr list
  | Unreachable
  | StructNew of string * wasm_instr list
  | StructGet of string * string * wasm_instr
  | StructSet of string * string * wasm_instr * wasm_instr
  | ArrayNew of string * wasm_instr * wasm_instr
  | ArrayNewFixed of string * wasm_instr list
  | ArrayNewDefault of string * wasm_instr
  | ArrayGet of string * wasm_instr * wasm_instr
  | ArrayGetS of string * wasm_instr * wasm_instr
  | ArraySet of string * wasm_instr * wasm_instr * wasm_instr
  | ArrayLen of wasm_instr
  | ArrayCopy of string * string * wasm_instr * wasm_instr * wasm_instr * wasm_instr * wasm_instr
  | RefCast of wasm_heap_type * wasm_instr
  | RefTest of wasm_heap_type * wasm_instr
  | RefIsNull of wasm_instr
  | RefAsNonNull of wasm_instr
  | RefEq of wasm_instr * wasm_instr
  | I31GetS of wasm_instr
  | I31GetU of wasm_instr
  | Seq of wasm_instr list
  | Drop of wasm_instr
  | Nop

type wasm_func = {
  func_name : string;
  func_type : wasm_func_type;
  func_locals : (string * wasm_val_type) list;
  func_body : wasm_instr;
  func_export : string option;
}

type wasm_global = {
  global_name : string;
  global_type : wasm_val_type;
  global_mutable : bool;
  global_init : wasm_instr;
  global_export : string option;
}

type wasm_import_desc =
  | ImportFunc of wasm_func_type
  | ImportGlobal of wasm_val_type * bool

type wasm_import = {
  import_module : string;
  import_name : string;
  import_local_name : string;
  import_desc : wasm_import_desc;
}

type wasm_export = {
  export_name : string;
  export_kind : [ `Func | `Global | `Memory | `Table ];
  export_ref : string;
}

type wasm_module = {
  mod_types : wasm_type_def list;
  mod_imports : wasm_import list;
  mod_funcs : wasm_func list;
  mod_globals : wasm_global list;
  mod_exports : wasm_export list;
  mod_start : string option;
}

val empty_module : wasm_module
val make_func_type : wasm_val_type list -> wasm_val_type list -> wasm_func_type
val i31ref : wasm_val_type
val string_type : wasm_val_type
val func_type_equal : wasm_func_type -> wasm_func_type -> bool
