open Base
open Hardcaml

module type S = Wave_intf.S

module M = Wave_intf.M

module Make (Data : Data.S) = struct
  type t =
    | Empty of string
    | Clock of string
    | Binary of string * Data.t
    | Data of string * Data.t * Wave_format.t * Text_alignment.t
  [@@deriving sexp_of, equal]

  let set_name t n =
    match t with
    | Empty _ -> Empty n
    | Clock _ -> Clock n
    | Binary (_, a) -> Binary (n, a)
    | Data (_, a, b, c) -> Data (n, a, b, c)
  ;;

  let get_name = function
    | Empty n -> n
    | Clock n -> n
    | Binary (n, _) -> n
    | Data (n, _, _, _) -> n
  ;;

  let get_data = function
    | Empty _ -> failwith "no empty data"
    | Clock _ -> failwith "no clock data"
    | Binary (_, d) -> d
    | Data (_, d, _, _) -> d
  ;;

  let get_to_str = function
    | Empty _ -> failwith "no empty to_str"
    | Clock _ -> failwith "no clock to_str"
    | Binary (_, _) -> failwith "no binary to_str"
    | Data (_, _, f, _) -> Staged.unstage (Wave_format.to_string f)
  ;;

  let get_alignment = function
    | Empty _ -> failwith "no empty get_alignment"
    | Clock _ -> failwith "no clock get_alignment"
    | Binary _ -> failwith "no binary get_alignment"
    | Data (_, _, _, alignment) -> alignment
  ;;

  let get_format : t -> Wave_format.t = function
    | Empty _ -> Binary
    | Clock _ -> Binary
    | Binary _ -> Binary
    | Data (_, _, f, _) -> f
  ;;
end
