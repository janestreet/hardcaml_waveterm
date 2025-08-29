type t [@@deriving sexp]

val create : unit -> t
val num_cursors : int
val get_offset : t -> int option
val get_offset_as_string : t -> string
val get_active_cursors : t -> [ `Primary of int | `Secondary of int ] list
val set_offset : t -> int option -> unit
val get_selected_as_string : t -> string
val set_selected : t -> int -> unit
val with_selected : t -> f:(int -> 'a) -> 'a
