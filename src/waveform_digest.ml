open Base
open Hardcaml

module type S = Waveform_digest_intf.S

type t = string

let create
  (type data)
  (module Data : Hardcaml.Wave_data.S with type t = data)
  (waves : data Hardcaml.Wave_data.Wave.t array)
  =
  (* Create a (shallow) copy of the waves then sort by name *)
  let waves = Array.copy waves in
  Array.sort waves ~compare:(fun a b -> String.compare a.name b.name);
  let hash =
    Crypto.Cryptokit.MAC.aes ~pad:Crypto.Cryptokit.Padding._8000 "HardcamlHardcaml"
  in
  for i = 0 to Array.length waves - 1 do
    let raw, length = Data.get_digestible_string waves.(i).wave_data in
    hash#add_substring raw 0 length
  done;
  hash#result
;;

let create (t : Hardcaml.Wave_data.t) =
  match t with
  | By_cycle waves -> create (module Wave_data_in_cycles) waves
  | By_event waves -> create (module Wave_data_in_events.Bits) waves
;;

let to_hex_string t =
  String.to_list t
  |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c))
  |> String.concat
;;
