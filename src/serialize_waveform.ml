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

(* Convert a test filename to a reasonable waveform file name *)
let default_waveform_filename filename =
  let name = Stdlib.Filename.basename filename |> Stdlib.Filename.chop_extension in
  let valid_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$' in
  String.map name ~f:(fun c -> if valid_char c then c else '_') ^ ".hardcamlwaveform"
;;

let marshall_here ?(here = Stdlib.Lexing.dummy_pos) t =
  if phys_equal here Lexing.dummy_pos
  then raise_s [%message "Must provide ~here:[%here] when using [marshall_here]"];
  let filename = default_waveform_filename here.pos_fname in
  marshall_waveterm t filename
;;

let unmarshall filename : Hardcaml.Wave_data.t =
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

let marshall_vcd
  (type data)
  (module Data : Hardcaml.Wave_data.S with type t = data)
  (t : data Hardcaml.Wave_data.Wave.t array)
  filename
  =
  let in_ports =
    Array.filter_map t ~f:(fun { name; wave_data; _ } ->
      Some (name, wave_data, ref (Hardcaml.Bits.zero (Data.width wave_data))))
  in
  (* replay the waveform *)
  let sim =
    Hardcaml.Cyclesim.Private.create
      ~in_ports:
        (Array.map in_ports ~f:(fun (name, _, bits) -> name, bits) |> Array.to_list)
      ~out_ports_before_clock_edge:[]
      ~out_ports_after_clock_edge:[]
      ~reset:Fn.id
      ~clock_mode:`All_one_domain
      ~clocks_aligned:(Fn.const true)
      ~cycle_multiple:1
      ~cycle_check:Fn.id
      ~cycle_before_clock_edge:Fn.id
      ~cycle_at_clock_edge:Fn.id
      ~cycle_after_clock_edge:Fn.id
      ~traced:{ input_ports = []; output_ports = []; internal_signals = [] }
      ~lookup_node_by_id:(Fn.const None)
      ~lookup_node:(Fn.const None)
      ~lookup_reg_by_id:(Fn.const None)
      ~lookup_reg:(Fn.const None)
      ~lookup_mem:(Fn.const None)
      ()
  in
  Stdio.Out_channel.with_file filename ~f:(fun file_out ->
    let sim = Hardcaml.Vcd.wrap file_out sim in
    let num_cycles =
      let _, data, _ = in_ports.(0) in
      Data.length data
    in
    for cycle = 0 to num_cycles - 1 do
      Array.iter in_ports ~f:(fun (_, data, port) -> port := Data.get data cycle);
      Hardcaml.Cyclesim.cycle sim
    done)
;;

let marshall_vcd (t : Hardcaml.Wave_data.t) filename =
  match t with
  | By_cycle cycles -> marshall_vcd (module Hardcaml.Wave_data_in_cycles) cycles filename
  | By_event events ->
    marshall_vcd (module Hardcaml.Wave_data_in_events.Bits) events filename
;;

let marshall (t : Hardcaml.Wave_data.t) filename =
  if String.is_suffix filename ~suffix:".vcd"
  then marshall_vcd t filename
  else marshall_waveterm t filename
;;
