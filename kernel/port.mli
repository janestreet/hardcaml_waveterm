(** Simulation port description. *)

open Base

module Type : sig
  type t =
    | Input
    | Output
    | Internal
  [@@deriving compare ~localize, sexp_of]
end

type t =
  { type_ : Type.t
  ; port_name : Port_name.t
  ; width : int
  }
[@@deriving compare ~localize, sexp_of, equal ~localize]
