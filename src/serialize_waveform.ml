open Base

module type S = Serialize_waveform_intf.S

(* serialization of waveform onto disk. while we can in theory write a better
   serialization format with bit-packing, gzip is a quick-and-easy way to get them
   reasonably compressed.

   on waveforms with 300 cycles and 200 ports, we see a 10x decrease in file size.
*)

let sanitize (waves : _ Hardcaml.Wave_data.Wave.t array) =
  Array.map waves ~f:(fun wave ->
    { wave with
      wave_format =
        (match wave.wave_format with
         | Custom _ -> Bit_or Hex
         | _ as x -> x)
    })
;;

let marshall_waveterm (t : Hardcaml.Wave_data.t) filename =
  let t =
    match t with
    | By_cycle waves -> Hardcaml.Wave_data.By_cycle (sanitize waves)
    | By_event waves -> By_event (sanitize waves)
  in
  let oc =
    Unix.open_process_out (Printf.sprintf "gzip -c >%s" (Stdlib.Filename.quote filename))
  in
  Stdlib.Marshal.to_channel oc t [];
  match Unix.close_process_out oc with
  | WEXITED 0 -> ()
  | WEXITED exit_code ->
    raise_s [%message "[gzip -c] terminated with non 0 exit code" (exit_code : int)]
  | WSIGNALED signal ->
    raise_s [%message "[gzip -c] terminated due to signal" (signal : int)]
  | WSTOPPED signal -> raise_s [%message "[gzip -c] stopped due to signal" (signal : int)]
;;

(* Convert a test filename to a reasonable waveform file name *)
let default_waveform_filename filename =
  let name = Stdlib.Filename.basename filename |> Stdlib.Filename.chop_extension in
  let valid_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$' in
  String.map name ~f:(fun c -> if valid_char c then c else '_') ^ ".hardcamlwaveform"
;;

let marshall_here ~(here : [%call_pos]) t =
  if phys_equal here Lexing.dummy_pos
  then raise_s [%message "Must provide ~here:[%here] when using [marshall_here]"];
  let filename = default_waveform_filename here.pos_fname in
  marshall_waveterm t filename
;;

let unmarshall_waveterm filename : Hardcaml.Wave_data.t =
  let ic =
    Unix.open_process_in (Printf.sprintf "zcat %s" (Stdlib.Filename.quote filename))
  in
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

let unmarshall_vcd filename =
  Hardcaml.Wave_data.By_event
    (Hardcaml.Vcd.read_event_based (Hardcaml_vcd.from_file filename))
;;

let unmarshall filename =
  if String.is_suffix filename ~suffix:".vcd"
  then unmarshall_vcd filename
  else unmarshall_waveterm filename
;;

let marshall_vcd (t : Hardcaml.Wave_data.t) filename =
  match t with
  | By_cycle cycles ->
    Stdio.Out_channel.with_file filename ~f:(fun chan ->
      Hardcaml.Vcd.write_cycle_based chan cycles)
  | By_event events ->
    Stdio.Out_channel.with_file filename ~f:(fun chan ->
      Hardcaml.Vcd.write_event_based chan events)
;;

let marshall (t : Hardcaml.Wave_data.t) filename =
  if String.is_suffix filename ~suffix:".vcd"
  then marshall_vcd t filename
  else marshall_waveterm t filename
;;
