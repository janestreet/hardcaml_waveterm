open Base
open Hardcaml

module type S = sig
  module Data : Data.S

  type wave_format_with_default =
    { mutable current : Wave_format.t
    ; default : Wave_format.t
    }
  [@@deriving sexp_of, equal ~localize]

  type mutable_style = { mutable style : Style.t }

  type t =
    | Empty of { mutable name : string }
    | Clock of
        { mutable name : string
        ; style : mutable_style
        }
    | Binary of
        { mutable name : string
        ; data : Data.t
        ; style : mutable_style
        }
    | Data of
        { mutable name : string
        ; data : Data.t
        ; wave_format : wave_format_with_default
        ; text_alignment : Text_alignment.t
        ; style : mutable_style
        }
  [@@deriving sexp_of, equal ~localize]

  val set_name : t -> string -> t
  val get_name : t -> string
  val get_data : t -> Data.t
  val get_to_str : t -> Bits.t -> string
  val get_alignment : t -> Text_alignment.t
  val get_format : t -> Wave_format.t
  val get_height_in_chars : t -> int
  val create_from_signal : ?style:Style.t -> string -> Signal.t -> Data.t -> t
end

module M (Data : Data.S) = struct
  module type S = S with module Data := Data
end

module type Wave = sig
  module type S = S

  module M = M
  module Make (Data : Data.S) : M(Data).S
end
