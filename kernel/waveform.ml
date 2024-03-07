open Base
open Hardcaml
open Stdio

module type S = Waveform_intf.S

module M = Waveform_intf.M

module Make
  (Data : Data.S)
  (Wave : Wave.M(Data).S)
  (Waves : Waves.M(Data)(Wave).S)
  (Render : Render.M(Data)(Wave)(Waves).S) =
struct
  let apply_wave_format
    (t : Wave.t)
    (format : Wave_format.t option)
    (alignment : Text_alignment.t)
    : Wave.t
    =
    let width =
      try Data.get (Wave.get_data t) 0 |> Bits.width with
      | _ -> 0
    in
    let wave_format =
      match format, Wave.get_format t with
      | None, fmt -> fmt
      | Some fmt, _ -> fmt
    in
    (* Maybe switch the rendering format depending on what wave_format was chosen. *)
    match t with
    | Binary (name, data) ->
      (match wave_format with
       | Bit | Bit_or _ -> t
       | _ -> Data (name, data, wave_format, alignment))
    | Data (name, data, _, _) ->
      (match wave_format with
       | (Bit | Bit_or _) when width = 1 -> Binary (name, data)
       | _ -> Data (name, data, wave_format, alignment))
    | Empty _ | Clock _ -> t
  ;;

  type t =
    { waves : Wave.t array
    ; ports : Port.t list
    }
  [@@deriving sexp_of, equal, fields ~getters]

  let update_waves t waves = { t with waves }

  let create_from_data ~waves ~ports =
    let waves = Array.of_list waves in
    { waves; ports }
  ;;

  let combine a b = { waves = Array.append a.waves b.waves; ports = a.ports @ b.ports }

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
    then
      raise_s [%message "Invalid display height.  Must be >= 3." (display_height : int)];
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
    Write.utf8 (Out_channel.output_string channel) ctx
  ;;
end
