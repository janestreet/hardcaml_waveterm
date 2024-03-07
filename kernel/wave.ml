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

  module Bits_conv = struct
    open Bits

    let to_hstr b =
      let to_char i =
        Char.of_int_exn (if i < 10 then Char.to_int '0' + i else Char.to_int 'A' + i - 10)
      in
      let blen = width b in
      let slen = (blen + 3) / 4 in
      String.init slen ~f:(fun i ->
        let i = slen - i - 1 in
        let l = i * 4 in
        let h = min blen (l + 4) - 1 in
        to_char (to_int (select b h l)))
    ;;

    (* convert to integer using arbitrary precision. *)
    let to_ustr b =
      let max = 29 in
      (* safe max positive int bits *)
      if width b <= max
      then Int.to_string (to_int b)
      else (
        (* convert with big ints *)
        let rec f b acc =
          let ( +: ) = Big_int.add_big_int in
          let ( <<: ) = Big_int.shift_left_big_int in
          let to_big b = Big_int.big_int_of_int (to_int b) in
          if width b <= max
          then (* result *)
            (acc <<: width b) +: to_big b
          else (
            let t, b = sel_top b max, drop_top b max in
            f b ((acc <<: max) +: to_big t))
        in
        Big_int.(string_of_big_int (f b zero_big_int)))
    ;;

    (* signed conversion uses unsigned conversion with detection of sign *)
    let to_sstr b =
      let max = 29 in
      (* safe max positive int bits *)
      if width b <= max
      then Int.to_string (to_sint b)
      else if to_int (msb b) = 0
      then to_ustr b
      else "-" ^ to_ustr (~:b +:. 1)
    ;;
  end

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
    | Data (_, _, f, _) ->
      let rec to_f : Wave_format.t -> _ = function
        | Binary -> Bits.to_bstr
        | Bit -> Bits.to_bstr
        | Bit_or t -> to_f t
        | Hex -> Bits_conv.to_hstr
        | Unsigned_int -> Bits_conv.to_ustr
        | Int -> Bits_conv.to_sstr
        | Custom f -> f
        | Index s ->
          fun elt ->
            (try List.nth_exn s (Bits.to_int elt) with
             | _ -> "-")
      in
      to_f f
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
