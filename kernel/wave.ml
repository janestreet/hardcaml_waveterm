open Base
open Hardcaml

module type S = Wave_intf.S

type wave_format_with_default =
  { mutable current : Wave_format.t
  ; default : Wave_format.t
  }
[@@deriving sexp_of, equal ~localize]

(* we reconstruct the variants (see [set_name]) but we want the new nodes to reference the
   same modifiable style cell, hence the type here (it could have been done with a ref
   also) *)
type mutable_style = { mutable style : Style.t } [@@deriving sexp_of, equal ~localize]

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

let set_name t name =
  match t with
  | Empty { name = _ } -> Empty { name }
  | Divider { name = _; style } -> Divider { name; style }
  | Clock { name = _; style } -> Clock { name; style }
  | Binary { name = _; data; style } -> Binary { name; data; style }
  | Data { name = _; data; width; wave_format; text_alignment; style } ->
    Data { name; data; width; wave_format; text_alignment; style }
;;

let get_name = function
  | Empty { name } -> name
  | Divider { name; _ } -> name
  | Clock { name; _ } -> name
  | Binary { name; _ } -> name
  | Data { name; _ } -> name
;;

let get_data = function
  | Empty _ -> failwith "no empty data"
  | Divider _ -> failwith "no divider data"
  | Clock _ -> failwith "no clock data"
  | Binary { data; _ } -> data
  | Data { data; _ } -> data
;;

let get_to_str = function
  | Empty _ -> failwith "no empty to_str"
  | Divider _ -> failwith "no divider to_str"
  | Clock _ -> failwith "no clock to_str"
  | Binary _ -> failwith "no binary to_str"
  | Data { wave_format; _ } -> Staged.unstage (Wave_format.to_string wave_format.current)
;;

let get_alignment = function
  | Empty _ -> failwith "no empty get_alignment"
  | Divider _ -> failwith "no divider get_alignment"
  | Clock _ -> failwith "no clock get_alignment"
  | Binary _ -> failwith "no binary get_alignment"
  | Data { text_alignment; _ } -> text_alignment
;;

let get_format : _ t -> Wave_format.t = function
  | Empty _ | Divider _ | Clock _ | Binary _ -> Bit
  | Data { wave_format; _ } -> wave_format.current
;;

let get_height_in_chars t =
  match t with
  | Empty _ ->
    (* You can set this it 1 to get a more compact render, but it looks a bit weird *)
    2
  | Divider _ -> 1
  | Clock _ -> 2
  | Data _ -> 3
  | Binary _ -> 2
;;
