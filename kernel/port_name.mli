(** An abstract type for port-name strings. *)

open Base

type t [@@deriving compare ~localize, sexp_of]

include%template Comparable.S [@mode local] with type t := t
include%template Equal.S [@mode local] with type t := t

include Stringable.S with type t := t
