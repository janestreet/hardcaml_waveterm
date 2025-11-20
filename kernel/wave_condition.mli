(** Type to represent a specific condition you may be searching for in the waveform. The
    signal can be found by either suffix or a regex. *)

open Hardcaml

module How_to_find : sig
  type t =
    | Suffix of string
    | Regex of Re.re
end

type t =
  { how_to_find : How_to_find.t
  ; condition : Bits.t -> bool
  }
