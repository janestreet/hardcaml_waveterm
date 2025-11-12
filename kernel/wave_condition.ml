open Hardcaml

type t =
  { suffix : string
  ; condition : Bits.t -> bool
  }
