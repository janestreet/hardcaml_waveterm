open Base
open Hardcaml_waveterm_kernel.Waveform

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
  | WSTOPPED signal -> raise_s [%message "[gzip -c] stopped due to signal" (signal : int)]
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

let serialize_expect_test_output =
  lazy
    (match Sys.getenv "EXPECT_TEST_WAVEFORM" with
     | None -> false
     | Some "1" -> true
     | Some "true" -> true
     | _ -> false)
;;

let expect
      ?display_rules
      ?display_width
      ?display_height
      ?display_values
      ?wave_width
      ?wave_height
      ?signals_width
      ?start_cycle
      ?signals_alignment
      ?(show_digest = true)
      ?serialize_to
      t
  =
  let extension = "hardcamlwaveform" in
  print
    ?display_rules
    ?display_width
    ?display_height
    ?display_values
    ?wave_width
    ?wave_height
    ?signals_width
    ?start_cycle
    ?signals_alignment
    ~show_digest
    t;
  match serialize_to with
  | None -> ()
  | Some serialize_to ->
    let serialize_to =
      if String.equal (Stdlib.Filename.extension serialize_to) extension
      then serialize_to
      else Printf.sprintf "%s.%s" serialize_to extension
    in
    if Lazy.force serialize_expect_test_output then marshall t serialize_to
;;
