open Base
open Hardcaml
open Stdio
include Waveform_intf

(* A simple heuristic to put the standard clock and reset related signals at the top of
   the waveform, then everything else in sorted order. *)
let default_display_rules =
  Display_rule.
    [ port_name_is "clk" ~wave_format:Bit
    ; port_name_is "clock" ~wave_format:Bit
    ; port_name_is "rst" ~wave_format:Bit
    ; port_name_is "reset" ~wave_format:Bit
    ; port_name_is "clr" ~wave_format:Bit
    ; port_name_is "clear" ~wave_format:Bit
    ; port_name_is "enable" ~wave_format:Bit
    ; input ()
    ; output ()
    ; default
    ]
;;

let apply_wave_format
  (t : _ Wave.t)
  (format : Wave_format.t option)
  (alignment : Text_alignment.t)
  : _ Wave.t
  =
  let wave_format =
    match format, Wave.get_format t with
    | None, fmt -> fmt
    | Some fmt, _ -> fmt
  in
  (* Maybe switch the rendering format depending on what wave_format was chosen. *)
  match t with
  | Binary { name; data; style } ->
    (match wave_format with
     | Bit | Bit_or _ -> t
     | _ ->
       Data
         { name
         ; data
         ; width = 1
         ; wave_format = { default = wave_format; current = wave_format }
         ; text_alignment = alignment
         ; style
         })
  | Data { name; data; width; wave_format = _; text_alignment = _; style } ->
    (match wave_format with
     | (Bit | Bit_or _) when width = 1 -> Binary { name; data; style }
     | _ ->
       Data
         { name
         ; data
         ; width
         ; wave_format = { current = wave_format; default = wave_format }
         ; text_alignment = alignment
         ; style
         })
  | Empty _ | Divider _ | Clock _ -> t
;;

let create_wave
  (type wave_data)
  ({ name; width; wave_format; is_pseudo_clock; wave_data; _ } :
    wave_data Hardcaml.Wave_data.Wave.t)
  : wave_data Wave.t
  =
  if is_pseudo_clock
  then Clock { name; style = { style = Style.default } }
  else if width = 1
          &&
          match wave_format with
          | Bit | Bit_or _ -> true
          | _ -> false
  then Binary { name; data = wave_data; style = { style = Style.default } }
  else
    Data
      { name
      ; data = wave_data
      ; width
      ; wave_format = { current = wave_format; default = wave_format }
      ; text_alignment = Left
      ; style = { style = Style.default }
      }
;;

let sort_ports_and_formats
  (type wave_data)
  (t : wave_data Wave_data.Wave.t array)
  display_rules
  : wave_data Wave.t array
  =
  let display_rules = Option.value ~default:default_display_rules display_rules in
  let waves =
    Array.fold
      t
      ~init:(Map.empty (module Port_name))
      ~f:(fun map (wave : _ Wave_data.Wave.t) ->
        match Map.add map ~key:(Port_name.of_string wave.name) ~data:wave with
        | `Ok map -> map
        | `Duplicate -> map)
  in
  (* Construct the display order and formatting *)
  let ports =
    Array.map t ~f:(fun wave ->
      { Port.port_name = Port_name.of_string wave.name
      ; type_ = wave.typ
      ; width = wave.width
      })
    |> Array.to_list
  in
  Display_rules.sort_ports_and_formats display_rules ports
  (* Associate ports in display order with waves in [t.waves]. We make no assumptions
     about what [hardcaml_waveterm] is actually doing and do our best to construct the
     requested display. In fact, [t.waves] should match [t.ports]. *)
  |> List.filter_map ~f:(function
    | Divider name -> Some (Wave.Divider { name; style = { style = Style.default } })
    | (Port { port; format = fmt_align_opt } : Display_rules.matched_rule) ->
      Map.find waves port.port_name
      |> Option.map ~f:(fun wave ->
        let wave = create_wave wave in
        match fmt_align_opt, wave with
        | Some { wave_format; alignment }, _ ->
          apply_wave_format wave wave_format alignment
        (* None represents default format, or format applied to a signal. *)
        | None, Data _ -> wave
        | None, _ ->
          Display_rules.run_rule Display_rule.Default port
          |> Option.map ~f:(fun { wave_format; alignment } ->
            apply_wave_format wave wave_format alignment)
          |> Option.value ~default:wave))
  |> Array.of_list
;;

module type Context = sig
  type t [@@deriving sexp_of, equal ~localize]

  val to_ctx
    :  ?display_rules:Display_rules.t
    -> ?signals_alignment:Text_alignment.t
    -> ?display_width:int
    -> ?display_height:int
    -> ?wave_width:int
    -> ?start_cycle:int
    -> ?display_values:bool
    -> ?signals_width:int
    -> t
    -> Draw.ctx

  val look_for_nth_instance_of_condition_in_waveform
    :  ?pos:int
    -> ?span:int
    -> ?negate:bool
    -> n:int
    -> conditions:Wave_condition.t list
    -> t
    -> int option
end

module Make_context (Data : Hardcaml.Wave_data.S) = struct
  module Render = Render.Make (Data)

  type t = Data.t Hardcaml.Wave_data.Wave.t array [@@deriving sexp_of, equal ~localize]

  let to_ctx
    ?display_rules
    ?signals_alignment
    ?(display_width = 70)
    ?display_height
    ?(wave_width = 3)
    ?(start_cycle = 0)
    ?(display_values = false)
    ?signals_width
    t
    =
    if display_width < 7
    then raise_s [%message "Invalid display width.  Must be >= 7." (display_width : int)];
    Option.iter signals_width ~f:(fun signals_width ->
      if signals_width >= display_width
      then
        raise_s
          [%message
            "Invalid signals_width. Require signals_width < display_width."
              (signals_width : int)
              (display_width : int)]);
    let waves =
      { Waves.cfg = { Waves.Config.default with wave_width; start_cycle }
      ; waves = sort_ports_and_formats t display_rules
      }
    in
    let display_height =
      match display_height with
      | Some display_height ->
        if display_height < 3
        then
          raise_s
            [%message "Invalid display height.  Must be >= 3." (display_height : int)];
        display_height
      | None ->
        Int.min
          256
          (2
           + Array.fold waves.waves ~init:0 ~f:(fun acc w ->
             acc + Wave.get_height_in_chars w))
    in
    Render.Static.draw
      ?signals_alignment
      ?signals_width
      ~values:display_values
      ~style:Window_styles.black_on_white
      ~rows:display_height
      ~cols:display_width
      waves
  ;;

  let look_for_nth_instance_of_condition_in_waveform
    ?(pos = 0)
    ?span
    ?(negate = false)
    ~n
    ~(conditions : Wave_condition.t list)
    (waves : Data.t Hardcaml.Wave_data.Wave.t array)
    =
    assert (List.length conditions > 0);
    assert (pos >= 0);
    (* Pick out the signal (specifically, its set of events) that we will use for each
       condition. *)
    let events_per_condition =
      List.map conditions ~f:(fun { how_to_find; condition = _ } ->
        let matches_name name =
          match how_to_find with
          | Wave_condition.How_to_find.Suffix suffix -> String.is_suffix name ~suffix
          | Regex re -> Re.execp (Display_rule.Regexp.compile re) name
        in
        let potential_matches =
          Array.filter_map waves ~f:(fun wave ->
            if (not wave.is_pseudo_clock) && matches_name wave.name
            then Some (wave.name, wave.wave_data)
            else None)
        in
        let how_to_find_str =
          match how_to_find with
          | Suffix suffix -> [%string "suffix \"%{suffix}\""]
          | Regex _ -> "regex"
        in
        match Array.length potential_matches with
        | 0 ->
          failwith
            [%string
              "Your %{how_to_find_str} didn't match any wave in the waveform! Please try \
               again."]
        | 1 -> snd potential_matches.(0)
        | _ ->
          raise_s
            [%message
              "Your pattern matched multiple signals in the waveform! Please be more \
               specific."
                (how_to_find_str : string)
                ~matches:(Array.map ~f:fst potential_matches : string array)])
    in
    let num_events =
      match
        List.map events_per_condition ~f:Data.length |> List.all_equal ~equal:Int.equal
      with
      | None ->
        raise_s
          [%message
            "Expect all signals to have the same number of events! If this is no longer \
             true, this function will have to be rewritten based on the new encoding \
             scheme."]
      | Some v -> v
    in
    let end_exclusive =
      match span with
      | None -> num_events
      | Some span -> Int.min (pos + span) num_events
    in
    (* Iterate through the events in the given range and find the nth point where all the
       conditions are met. *)
    With_return.with_return (fun { return } ->
      let num_found = ref 0 in
      for i = pos to end_exclusive - 1 do
        let conditions_met =
          List.for_all2_exn
            conditions
            events_per_condition
            ~f:(fun { condition; how_to_find = _ } events ->
              let ev = Data.get events i in
              condition ev)
        in
        let matched = if negate then not conditions_met else conditions_met in
        if matched
        then (
          Int.incr num_found;
          if !num_found = n then return (Some i))
      done;
      None)
  ;;
end

module Make_api (Context : Context) = struct
  include Context

  let to_buffer
    ?display_rules
    ?display_width
    ?display_height
    ?display_values
    ?wave_width
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
        ?signals_width
        ?start_cycle
        ?signals_alignment
        t
    in
    Write.utf8 (fun s -> Buffer.add_string buffer s) ctx;
    buffer
  ;;

  let to_string
    ?display_rules
    ?display_width
    ?display_height
    ?display_values
    ?wave_width
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
        ?signals_width
        ?start_cycle
        ?signals_alignment
        t
    in
    Write.utf8 (Out_channel.output_string channel) ctx
  ;;

  let look_for_first_instance_of_condition_in_wave ~f ~wave_name t =
    look_for_nth_instance_of_condition_in_waveform
      t
      ~n:1
      ~conditions:[ { how_to_find = Suffix wave_name; condition = f } ]
  ;;

  let look_for_wave_first_vdd_cycle ~how_to_find t =
    look_for_nth_instance_of_condition_in_waveform
      ~n:1
      ~conditions:[ { condition = Bits.to_bool; how_to_find } ]
      t
  ;;
end

module type S_with_context = sig
  include Context
  include S with type t := t
end

module Make (Data : Hardcaml.Wave_data.S) = struct
  module Context = Make_context (Data)
  module Api = Make_api (Context)
  include Context
  include Api
end

module By_cycle :
  S_with_context
  with type t = Hardcaml.Wave_data_in_cycles.t Hardcaml.Wave_data.Wave.t array =
  Make (Hardcaml.Wave_data_in_cycles)

module By_event = Make (Hardcaml.Wave_data_in_events.Bits)

include Make_api (struct
    type t = Hardcaml.Wave_data.t [@@deriving sexp_of, equal ~localize]

    let to_ctx
      ?display_rules
      ?signals_alignment
      ?display_width
      ?display_height
      ?wave_width
      ?start_cycle
      ?display_values
      ?signals_width
      t
      =
      match t with
      | Hardcaml.Wave_data.By_cycle cycles ->
        By_cycle.to_ctx
          ?display_rules
          ?signals_alignment
          ?display_width
          ?display_height
          ?wave_width
          ?start_cycle
          ?display_values
          ?signals_width
          cycles
      | By_event events ->
        By_event.to_ctx
          ?display_rules
          ?signals_alignment
          ?display_width
          ?display_height
          ?wave_width
          ?start_cycle
          ?display_values
          ?signals_width
          events
    ;;

    let look_for_nth_instance_of_condition_in_waveform
      ?pos
      ?span
      ?negate
      ~n
      ~(conditions : Wave_condition.t list)
      waves
      =
      match waves with
      | Hardcaml.Wave_data.By_cycle cycles ->
        By_cycle.look_for_nth_instance_of_condition_in_waveform
          ?pos
          ?span
          ?negate
          ~n
          ~conditions
          cycles
      | By_event events ->
        By_event.look_for_nth_instance_of_condition_in_waveform
          ?pos
          ?span
          ?negate
          ~n
          ~conditions
          events
    ;;
  end)
