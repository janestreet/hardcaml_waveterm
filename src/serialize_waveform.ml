open Base
open Hardcaml
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
           | Binary | Bit | Bit_or _ | Hex | Unsigned_int | Int | Index _ | Map _ -> wave
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

  (* Convert a test filename to a reasonable waveform file name *)
  let default_waveform_filename filename =
    let name = Stdlib.Filename.basename filename |> Stdlib.Filename.chop_extension in
    let valid_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$' in
    String.map name ~f:(fun c -> if valid_char c then c else '_') ^ ".hardcamlwaveform"
  ;;

  let marshall_here ~(here : [%call_pos]) (t : t) =
    if phys_equal here Lexing.dummy_pos
    then raise_s [%message "Must provide ~here:[%here] when using [marshall_here]"];
    let filename = default_waveform_filename here.pos_fname in
    marshall t filename
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

  let marshall_vcd t ~filename =
    let waves = Waveform.waves t in
    let in_ports =
      Array.filter_map waves ~f:(function
        | Empty _ -> None
        | Clock _ -> None
        | Binary (name, data) -> Some (name, data, ref (Bits.zero (Data.width data)))
        | Data (name, data, _format, _alignment) ->
          Some (name, data, ref (Bits.zero (Data.width data))))
    in
    (* replay the waveform *)
    let sim =
      Cyclesim.Private.create
        ~in_ports:
          (Array.map in_ports ~f:(fun (name, _, bits) -> name, bits) |> Array.to_list)
        ~out_ports_before_clock_edge:[]
        ~out_ports_after_clock_edge:[]
        ~reset:Fn.id
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
      let sim = Vcd.wrap file_out sim in
      let num_cycles =
        let _, data, _ = in_ports.(0) in
        Data.length data
      in
      for cycle = 0 to num_cycles - 1 do
        Array.iter in_ports ~f:(fun (_, data, port) -> port := Data.get data cycle);
        Cyclesim.cycle sim
      done)
  ;;
end
