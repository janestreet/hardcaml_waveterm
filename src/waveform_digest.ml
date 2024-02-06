open Base
open Hardcaml_waveterm_kernel

type t = string

let create (waves : Waveform.t) =
  let hash =
    Crypto.Cryptokit.MAC.aes ~pad:Crypto.Cryptokit.Padding._8000 "HardcamlHardcaml"
  in
  let waves = Waveform.waves waves in
  for i = 0 to Array.length waves - 1 do
    let wave = waves.(i) in
    match wave with
    | Empty _ | Clock _ ->
      (* [Expert.Wave.get_data] raises if we try to access a [Clock] or [Empty] data wave.
         Just ignore it. *)
      ()
    | _ ->
      let data = Expert.Wave.get_data waves.(i) in
      let raw = Expert.Data.raw_data data in
      hash#add_substring raw 0 (Expert.Data.used_raw_data_bytes data)
  done;
  hash#result
;;

let to_hex_string t =
  String.to_list t
  |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c))
  |> String.concat
;;
