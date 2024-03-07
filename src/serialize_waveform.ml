open Base
open Hardcaml_waveterm_kernel.Expert
module M = Serialize_waveform_intf.M

module Make
  (Data : Data.S)
  (Wave : Wave.M(Data).S)
  (Waves : Waves.M(Data)(Wave).S)
  (Render : Render.M(Data)(Wave)(Waves).S)
  (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) =
struct
  open Waveform

  let equal = equal

  (* serialization of waveform onto disk. while we can in theory write a better
     serialization format with bit-packing, gzip is a quick-and-easy way to get
     them reasonably compressed.

     on waveforms with 300 cycles and 200 ports, we see a 10x decrease in file size.
  *)

  let sanitize (t : t) =
    let waves =
      Array.map (waves t) ~f:(fun wave ->
        match wave with
        | Empty _ | Clock _ | Binary _ -> wave
        | Data (name, data, wave_format, alignment) ->
          (match wave_format with
           | Binary | Bit | Bit_or _ | Hex | Unsigned_int | Int | Index _ -> wave
           | Custom _ -> Data (name, data, Bit_or Hex, alignment)))
    in
    update_waves t waves
  ;;

  let marshall (t : t) filename =
    let t = sanitize t in
    let oc = Unix.open_process_out (Printf.sprintf "gzip -c >%s" filename) in
    Stdlib.Marshal.to_channel oc t [];
    match Unix.close_process_out oc with
    | WEXITED 0 -> ()
    | WEXITED exit_code ->
      raise_s [%message "[gzip -c] terminated with non 0 exit code" (exit_code : int)]
    | WSIGNALED signal ->
      raise_s [%message "[gzip -c] terminated due to signal" (signal : int)]
    | WSTOPPED signal ->
      raise_s [%message "[gzip -c] stopped due to signal" (signal : int)]
  ;;

  let unmarshall filename : t =
    let ic = Unix.open_process_in (Printf.sprintf "zcat %s" filename) in
    let ret = Stdlib.Marshal.from_channel ic in
    match Unix.close_process_in ic with
    | WEXITED 0 -> ret
    | WEXITED exit_code ->
      raise_s
        [%message
          "Unix.close_process_in terminated with non zero exit code" (exit_code : int)]
    | WSIGNALED signal ->
      raise_s [%message "Unix.close_process_in terminated due to signal" (signal : int)]
    | WSTOPPED signal ->
      raise_s [%message "Unix.close_process_in stopped due to signal" (signal : int)]
  ;;
end
