open Base
module Time : Event_store.Time with type t = int
module Data : Event_store.Data with type t = Hardcaml.Bits.t
module Event_store : Event_store.M(Time)(Data).S

type t [@@deriving sexp_of, equal]

val create : int -> int ref -> t
val event_store : t -> Event_store.t

(** Data.S interface *)

val width : t -> int
val length : t -> int
val get : t -> int -> Hardcaml.Bits.t
val get_digestible_string : t -> Bytes.t * int
