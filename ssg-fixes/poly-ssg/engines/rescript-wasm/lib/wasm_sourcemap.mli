(** Source Map Generation (Phase 6) *)

(** {1 Source Location} *)

type source_location = {
  file : string;
  line : int;
  column : int;
}

(** {1 Source Map Entry} *)

type source_map_entry = {
  wasm_offset : int;
  source_loc : source_location;
}

(** {1 Source Map} *)

type source_map = {
  version : int;
  sources : string list;
  mappings : source_map_entry list;
}

(** Create an empty source map *)
val empty_source_map : source_map

(** Add a source file to the map *)
val add_source : source_map -> string -> source_map

(** Add a mapping entry *)
val add_mapping : source_map -> wasm_offset:int -> file:string -> line:int -> column:int -> source_map

(** {1 Source Map Output} *)

(** Generate JSON source map format (version 3) *)
val to_json : source_map -> string

(** Write source map to file *)
val write_source_map : string -> source_map -> unit

(** {1 Debug Sections} *)

type debug_section = {
  name : string;
  data : bytes;
}

(** Generate DWARF-like debug section for WASM custom section *)
val generate_debug_section : source_map -> debug_section

(** {1 Source Map Builder} *)

module Builder : sig
  type t

  (** Create a new builder *)
  val create : unit -> t

  (** Set the current source file *)
  val set_file : t -> string -> unit

  (** Set the current source line *)
  val set_line : t -> int -> unit

  (** Record an instruction emission *)
  val add_instruction : t -> bytes_emitted:int -> unit

  (** Finalize and return the source map *)
  val finish : t -> source_map
end
