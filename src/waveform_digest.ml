open Base
open Hardcaml_waveterm_kernel.Expert
module M = Waveform_digest_intf.M

module Make
  (Data : Data.S)
  (Wave : Wave.M(Data).S)
  (Waves : Waves.M(Data)(Wave).S)
  (Render : Render.M(Data)(Wave)(Waves).S)
  (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) =
struct
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
        let data = Wave.get_data waves.(i) in
        let raw, length = Data.get_digestible_string data in
        hash#add_substring raw 0 length
    done;
    hash#result
  ;;

  let to_hex_string t =
    String.to_list t
    |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c))
    |> String.concat
  ;;
end
