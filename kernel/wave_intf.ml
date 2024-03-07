open Base
open Hardcaml

module type S = sig
  module Data : Data.S

  type t =
    | Empty of string
    | Clock of string
    | Binary of string * Data.t
    | Data of string * Data.t * Wave_format.t * Text_alignment.t
  [@@deriving sexp_of, equal]

  val set_name : t -> string -> t
  val get_name : t -> string
  val get_data : t -> Data.t
  val get_to_str : t -> Bits.t -> string
  val get_alignment : t -> Text_alignment.t
  val get_format : t -> Wave_format.t
end

module M (Data : Data.S) = struct
  module type S = S with module Data := Data
end

module type Wave = sig
  module type S = S

  module M = M
  module Make (Data : Data.S) : M(Data).S
end
