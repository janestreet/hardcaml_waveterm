(** A dynamically-sized array, similar to std::vector in C++. *)

open! Import

type t [@@deriving sexp_of, compare, equal]

val create : int -> t
val init : int -> width:int -> f:(int -> Bits.t) -> t
val length : t -> int
val width : t -> int
val get : t -> int -> Bits.t
val set : t -> int -> Bits.t -> unit
val non_cache_hits : t -> int
