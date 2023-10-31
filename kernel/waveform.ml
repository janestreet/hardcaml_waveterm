open Base
open Hardcaml
open Stdio

let apply_wave_format (t : Wave.t) (format : Wave_format.t) (alignment : Text_alignment.t)
  : Wave.t
  =
  let width =
    try
      let t = Wave.get_data t in
      Data.get t 0 |> Bits.width
    with
    | _ -> 0
  in
  let rec to_basic_type w : Wave_format.t -> Wave_format.t = function
    | Bit -> Binary
    | Bit_or t -> if w = 1 then Binary else to_basic_type w t
    | (Binary | Hex | Unsigned_int | Int | Index _ | Custom _) as x -> x
  in
  let to_str = to_basic_type width format in
  match t with
  | Binary (name, data) ->
    (match format with
     | Bit | Bit_or _ -> t
     | _ ->
       (* special case - promote [Binary] to [Data] for single bits, if required. *)
       Data (name, data, to_str, alignment))
  | Data (name, data, _, _) -> Data (name, data, to_str, alignment)
  | Empty _ | Clock _ -> t
;;

type t =
  { waves : Wave.t array
  ; ports : Port.t list
  ; digest : Hardcaml.Cyclesim.Digest.t ref
  }
[@@deriving sexp_of, equal, fields ~getters]

let update_waves t waves = { t with waves }

let create_from_data ~waves ~ports =
  let waves = Array.of_list waves in
  { waves; ports; digest = ref Hardcaml.Cyclesim.Digest.none }
;;

let combine a b =
  { waves = Array.append a.waves b.waves; ports = a.ports @ b.ports; digest = a.digest }
;;

(* A simple heuristic to put the standard clock and reset related signals
   at the top of the waveform, then everything else in sorted order. *)
let default_display_rules =
  Display_rule.
    [ port_name_is "clk" ~wave_format:Bit
    ; port_name_is "clock" ~wave_format:Bit
    ; port_name_is "rst" ~wave_format:Bit
    ; port_name_is "reset" ~wave_format:Bit
    ; port_name_is "clr" ~wave_format:Bit
    ; port_name_is "clear" ~wave_format:Bit
    ; port_name_is "enable" ~wave_format:Bit
    ; default
    ]
;;

let sort_ports_and_formats t display_rules : Wave.t array =
  let display_rules = Option.value ~default:default_display_rules display_rules in
  let waves =
    Array.to_list t.waves
    |> List.map ~f:(fun wave -> Wave.get_name wave |> Port_name.of_string, wave)
    |> Map.of_alist_exn (module Port_name)
  in
  (* Construct the display order and formatting *)
  Display_rules.sort_ports_and_formats display_rules t.ports
  (* Associate ports in display order with waves in [t.waves].  We make no assumptions
     about what [hardcaml_waveterm] is actually doing and do our best to construct the
     requested display.  In fact, [t.waves] should match [t.ports]. *)
  |> List.filter_map ~f:(fun ((port : Port.t), fmt_align_opt) ->
       Map.find waves port.port_name
       |> Option.map ~f:(fun wave ->
            match fmt_align_opt, wave with
            | Some (format, alignment), _ -> apply_wave_format wave format alignment
            (* None represents default format. Don't apply default to Index and Custom *)
            | None, Data (_, _, Wave_format.Index _, _)
            | None, Data (_, _, Wave_format.Custom _, _) -> wave
            | None, _ ->
              Display_rules.run_rule Display_rule.Default port
              |> Option.map ~f:(fun (format, alignment) ->
                   apply_wave_format wave format alignment)
              |> Option.value ~default:wave))
  |> Array.of_list
;;

let create sim =
  let ports =
    let port type_ (port_name, s) =
      { Port.type_; port_name = port_name |> Port_name.of_string; width = Bits.width !s }
    in
    let traced_port type_ { Cyclesim.Traced.signal; names } =
      List.map names ~f:(fun name ->
        { Port.type_
        ; port_name = name |> Port_name.of_string
        ; width = Signal.width signal
        })
    in
    List.concat
      [ List.map (Cyclesim.in_ports sim) ~f:(port Input)
      ; List.map (Cyclesim.out_ports sim) ~f:(port Output)
      ; List.concat_map (Cyclesim.traced sim) ~f:(traced_port Internal)
      ]
  in
  let sim, waves = Sim.wrap sim in
  { waves; ports; digest = Cyclesim.digest sim }, sim
;;

type 'a with_options =
  ?display_rules:Display_rules.t
  -> ?display_width:int
  -> ?display_height:int
  -> ?display_values:bool
  -> ?wave_width:int
  -> ?wave_height:int
  -> ?signals_width:int
  -> ?start_cycle:int
  -> ?signals_alignment:Text_alignment.t
  -> 'a

let to_ctx
  ?display_rules
  ?signals_alignment
  ?(display_width = 70)
  ?(display_height = 20)
  ?(wave_width = 3)
  ?(wave_height = 1)
  ?(start_cycle = 0)
  ?(display_values = false)
  ?signals_width
  t
  =
  if display_height < 3
  then raise_s [%message "Invalid display height.  Must be >= 3." (display_height : int)];
  if display_width < 7
  then raise_s [%message "Invalid display width.  Must be >= 7." (display_width : int)];
  if wave_height < 0
  then raise_s [%message "Invalid wave height.  Must be >= 0." (wave_height : int)];
  Option.iter signals_width ~f:(fun signals_width ->
    if signals_width >= display_width
    then
      raise_s
        [%message
          "Invalid signals_width. Require signals_width < display_width."
            (signals_width : int)
            (display_width : int)]);
  Render.Static.draw
    ?signals_alignment
    ?signals_width
    ~values:display_values
    ~style:Render.Styles.black_on_white
    ~rows:display_height
    ~cols:display_width
    { cfg = { Waves.Config.default with wave_width; wave_height; start_cycle }
    ; waves = sort_ports_and_formats t display_rules
    }
;;

let to_buffer
  ?display_rules
  ?display_width
  ?display_height
  ?display_values
  ?wave_width
  ?wave_height
  ?signals_width
  ?start_cycle
  ?signals_alignment
  t
  =
  let buffer = Buffer.create 1024 in
  let ctx =
    to_ctx
      ?display_rules
      ?display_width
      ?display_height
      ?display_values
      ?wave_width
      ?wave_height
      ?signals_width
      ?start_cycle
      ?signals_alignment
      t
  in
  Write.utf8 (Buffer.add_string buffer) ctx;
  buffer
;;

let to_string
  ?display_rules
  ?display_width
  ?display_height
  ?display_values
  ?wave_width
  ?wave_height
  ?signals_width
  ?start_cycle
  ?signals_alignment
  t
  =
  to_buffer
    ?display_rules
    ?display_width
    ?display_height
    ?display_values
    ?wave_width
    ?wave_height
    ?signals_width
    ?start_cycle
    ?signals_alignment
    t
  |> Buffer.contents
;;

let print
  ?display_rules
  ?display_width
  ?display_height
  ?display_values
  ?wave_width
  ?wave_height
  ?signals_width
  ?start_cycle
  ?signals_alignment
  ?(show_digest = false)
  ?(channel = Out_channel.stdout)
  t
  =
  let ctx =
    to_ctx
      ?display_rules
      ?display_width
      ?display_height
      ?display_values
      ?wave_width
      ?wave_height
      ?signals_width
      ?start_cycle
      ?signals_alignment
      t
  in
  Write.utf8 (Out_channel.output_string channel) ctx;
  if show_digest
  then
    Out_channel.print_endline
      (Sexp.to_string_mach (Hardcaml.Cyclesim.Digest.sexp_of_t !(t.digest)))
;;
