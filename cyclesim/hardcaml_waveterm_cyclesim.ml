open Base
open Hardcaml
module Node = Cyclesim.Node
module Traced = Cyclesim.Traced
module Data = Hardcaml.Wave_data
open Hardcaml_waveterm_kernel
include Expert.Make (Data)

let lookup_node sim cycle (t : Traced.internal_signal) =
  let width = Signal.width t.signal in
  let data = Data.create width in
  let v = Cyclesim.lookup_node_or_reg sim t in
  Option.map v ~f:(fun v ->
    let d = Node.data v in
    let byte_address = Node.byte_address v in
    let set_from_bytes = Data.set_from_bytes width in
    data, fun _ -> set_from_bytes data !cycle d byte_address)
;;

let lookup_port find_port sim cycle (t : Traced.io_port) =
  let width = Signal.width t.signal in
  let data = Data.create width in
  let v = find_port sim t.name in
  data, fun _ -> Data.set data !cycle !v
;;

let lookup_in_port sim cycle t = lookup_port Cyclesim.in_port sim cycle t

let lookup_out_port sim cycle t =
  lookup_port (Cyclesim.out_port ~clock_edge:Before) sim cycle t
;;

let create_wave signal name data =
  let width = Signal.width signal in
  let wave_format = Signal.Type.get_wave_format signal in
  let wave =
    if width = 1
       &&
       match wave_format with
       | Bit | Bit_or _ -> true
       | _ -> false
    then Wave.Binary (name, data)
    else Wave.Data (name, data, wave_format, Left)
  in
  wave
;;

let is_clock = function
  | "clock" | "clk" -> true
  | _ -> false
;;

let is_reset = function
  | "reset" | "rst" -> true
  | _ -> false
;;

let trace sim cycle =
  let traced = Cyclesim.traced sim in
  let io_port lookup (t : Traced.io_port) =
    if is_clock t.name
    then Wave.Clock t.name, fun _ -> ()
    else if is_reset t.name
    then (
      let data, _ = lookup sim cycle t in
      ( create_wave t.signal t.name data
      , fun v -> Data.set data !cycle (if v then Bits.vdd else Bits.gnd) ))
    else (
      let data, update = lookup sim cycle t in
      create_wave t.signal t.name data, update)
  in
  let internal_signal (t : Traced.internal_signal) =
    Option.value_map (lookup_node sim cycle t) ~default:[] ~f:(fun (data, update) ->
      List.map t.mangled_names ~f:(fun name ->
        if is_clock name
        then Wave.Clock name, fun _ -> ()
        else create_wave t.signal name data, update))
  in
  List.concat
    [ List.map traced.input_ports ~f:(io_port lookup_in_port)
    ; List.map traced.output_ports ~f:(io_port lookup_out_port)
    ; List.map traced.internal_signals ~f:internal_signal |> List.concat
    ]
;;

module Expert = struct
  let wrap sim =
    let cycle = ref 0 in
    let traced = trace sim cycle in
    let waves =
      let waves = Array.of_list_map traced ~f:fst in
      Array.sort waves ~compare:(fun w0 w1 ->
        String.compare (Wave.get_name w0) (Wave.get_name w1));
      waves
    in
    let updates = Array.of_list_map traced ~f:snd in
    let tasks rst () =
      Array.iter ~f:(fun f -> f rst) updates;
      Int.incr cycle
    in
    let sim =
      Cyclesim.Private.modify
        sim
        [ After, Reset, tasks true; Before, At_clock_edge, tasks false ]
    in
    sim, waves
  ;;
end

module Waveform = struct
  include Waveform

  let create sim =
    let ports =
      let port type_ (port_name, s) =
        { Port.type_
        ; port_name = port_name |> Port_name.of_string
        ; width = Bits.width !s
        }
      in
      let traced_port type_ { Cyclesim.Traced.signal; mangled_names } =
        List.map mangled_names ~f:(fun name ->
          { Port.type_
          ; port_name = name |> Port_name.of_string
          ; width = Signal.width signal
          })
      in
      List.concat
        [ List.map (Cyclesim.in_ports sim) ~f:(port Input)
        ; List.map (Cyclesim.out_ports sim) ~f:(port Output)
        ; List.concat_map (Cyclesim.traced sim).internal_signals ~f:(traced_port Internal)
        ]
    in
    let sim, waves = Expert.wrap sim in
    Waveform.create_from_data ~waves:(Array.to_list waves) ~ports, sim
  ;;
end
