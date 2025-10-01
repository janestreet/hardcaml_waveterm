open Base
open Hardcaml

module type S = Wave_intf.S

module M = Wave_intf.M

module Make (Data : Data.S) = struct
  type wave_format_with_default =
    { mutable current : Wave_format.t
    ; default : Wave_format.t
    }
  [@@deriving sexp_of, equal ~localize]

  (* we reconstruct the variants (see [set_name]) but we want the new nodes to reference
     the same modifiable style cell, hence the type here (it could have been done with a
     ref also) *)
  type mutable_style = { mutable style : Style.t } [@@deriving sexp_of, equal ~localize]

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

  let set_name t name =
    match t with
    | Empty { name = _ } -> Empty { name }
    | Clock { name = _; style } -> Clock { name; style }
    | Binary { name = _; data; style } -> Binary { name; data; style }
    | Data { name = _; data; wave_format; text_alignment; style } ->
      Data { name; data; wave_format; text_alignment; style }
  ;;

  let get_name = function
    | Empty { name } -> name
    | Clock { name; _ } -> name
    | Binary { name; _ } -> name
    | Data { name; _ } -> name
  ;;

  let get_data = function
    | Empty _ -> failwith "no empty data"
    | Clock _ -> failwith "no clock data"
    | Binary { data; _ } -> data
    | Data { data; _ } -> data
  ;;

  let get_to_str = function
    | Empty _ -> failwith "no empty to_str"
    | Clock _ -> failwith "no clock to_str"
    | Binary _ -> failwith "no binary to_str"
    | Data { wave_format; _ } ->
      Staged.unstage (Wave_format.to_string wave_format.current)
  ;;

  let get_alignment = function
    | Empty _ -> failwith "no empty get_alignment"
    | Clock _ -> failwith "no clock get_alignment"
    | Binary _ -> failwith "no binary get_alignment"
    | Data { text_alignment; _ } -> text_alignment
  ;;

  let get_format : t -> Wave_format.t = function
    | Empty _ -> Binary
    | Clock _ -> Binary
    | Binary _ -> Binary
    | Data { wave_format; _ } -> wave_format.current
  ;;

  let get_height_in_chars t =
    match t with
    | Empty _ ->
      (* You can set this it 1 to get a more compact render, but it looks a bit weird *)
      2
    | Clock _ -> 2
    | Data _ -> 3
    | Binary _ -> 2
  ;;

  let create_from_signal ?style name signal data =
    let width = Signal.width signal in
    let wave_format = Signal.Type.get_wave_format signal in
    if width = 1
       &&
       match wave_format with
       | Bit | Bit_or _ -> true
       | _ -> false
    then
      Binary { name; data; style = { style = Option.value ~default:Style.default style } }
    else
      Data
        { name
        ; data
        ; wave_format = { current = wave_format; default = wave_format }
        ; text_alignment = Left
        ; style = { style = Option.value ~default:Style.default style }
        }
  ;;
end
