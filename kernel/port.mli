(** Simulation port description. *)

open Base
module Type = Hardcaml.Wave_data.Type

type t =
  { type_ : Type.t
  ; port_name : Port_name.t
  ; width : int
  }
[@@deriving compare ~localize, sexp_of, equal ~localize]
