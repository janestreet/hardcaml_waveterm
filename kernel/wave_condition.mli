(** Type to represent a specific condition you may be searching for in the waveform. *)

open Hardcaml

type t =
  { suffix : string
  ; condition : Bits.t -> bool
  }
