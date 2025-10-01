(** A rectangle *)

open Base

type t =
  { r : int (** Top left row *)
  ; c : int (** Top left column *)
  ; w : int (** Width *)
  ; h : int (** Hieght *)
  }
[@@deriving sexp_of]

val empty : t
val inside : t -> row:int -> col:int -> bool
