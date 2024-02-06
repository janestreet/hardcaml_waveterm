open Base

module Data = struct
  type t = Hardcaml.Bits.t [@@deriving sexp_of]

  let none = Hardcaml.Bits.empty
  let merge _ x = x
end

module Time = struct
  type t = int [@@deriving compare, sexp_of]

  let zero = 0
end

include Event_store.Make (Time) (Data)
