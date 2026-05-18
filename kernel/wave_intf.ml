open Base
open Hardcaml

module type S = sig
  type wave_format_with_default =
    { mutable current : Wave_format.t
    ; default : Wave_format.t
    }
  [@@deriving sexp_of, equal ~localize]

  type mutable_style = { mutable style : Style.t }

  type 'data t =
    | Empty of { mutable name : string }
    | Divider of
        { mutable name : string
        ; style : mutable_style
        }
    | Clock of
        { mutable name : string
        ; style : mutable_style
        }
    | Binary of
        { mutable name : string
        ; data : 'data
        ; style : mutable_style
        }
    | Data of
        { mutable name : string
        ; data : 'data
        ; width : int
        ; wave_format : wave_format_with_default
        ; text_alignment : Text_alignment.t
        ; style : mutable_style
        }
  [@@deriving sexp_of, equal ~localize]

  val set_name : 'a t -> string -> 'a t
  val get_name : 'a t -> string
  val get_data : 'data t -> 'data
  val get_to_str : 'a t -> Bits.t -> string
  val get_alignment : 'a t -> Text_alignment.t
  val get_format : 'a t -> Wave_format.t
  val get_height_in_chars : 'a t -> int
end

module type Wave = sig
  module type S = S

  include S
end
