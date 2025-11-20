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
    | Binary { name; data; style } ->
      (match wave_format with
       | Bit | Bit_or _ -> t
       | _ ->
         Data
           { name
           ; data
           ; wave_format = { default = wave_format; current = wave_format }
           ; text_alignment = alignment
           ; style
           })
    | Data { name; data; wave_format = _; text_alignment = _; style } ->
      (match wave_format with
       | (Bit | Bit_or _) when width = 1 -> Binary { name; data; style }
       | _ ->
         Data
           { name
           ; data
           ; wave_format = { current = wave_format; default = wave_format }
           ; text_alignment = alignment
           ; style
           })
    | Empty _ | Clock _ -> t
  ;;

  type t =
    { waves : Wave.t array
    ; ports : Port.t list
    }
  [@@deriving sexp_of, equal ~localize, fields ~getters]

  let update_waves t waves = { t with waves }

  let create_from_data ~waves ~ports =
    let waves = Array.of_list waves in
    { waves; ports }
  ;;

  let combine a b = { waves = Array.append a.waves b.waves; ports = a.ports @ b.ports }

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
    (* Associate ports in display order with waves in [t.waves]. We make no assumptions
       about what [hardcaml_waveterm] is actually doing and do our best to construct the
       requested display. In fact, [t.waves] should match [t.ports]. *)
    |> List.filter_map ~f:(fun ((port : Port.t), fmt_align_opt) ->
      Map.find waves port.port_name
      |> Option.map ~f:(fun wave ->
        match fmt_align_opt, wave with
        | Some (format, alignment), _ -> apply_wave_format wave format alignment
        (* None represents default format, or format applied to a signal. *)
        | None, Data _ -> wave
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
    -> ?signals_width:int
    -> ?start_cycle:int
    -> ?signals_alignment:Text_alignment.t
    -> 'a

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
    Write.utf8 (Buffer.add_string buffer) ctx;
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

  let look_for_nth_instance_of_condition_in_waveform
    ~n
    ~(conditions : Wave_condition.t list)
    t
    =
    assert (List.length conditions > 0);
    let waves = t.waves in
    (* Pick out the signal (specifically, its set of events) that we will use for each
       condition. *)
    let events_per_condition =
      List.map conditions ~f:(fun { how_to_find; condition = _ } ->
        let matches_name name =
          match how_to_find with
          | Wave_condition.How_to_find.Suffix suffix -> String.is_suffix name ~suffix
          | Regex re -> Re.execp re name
        in
        let potential_matches =
          Array.filter_map waves ~f:(fun wave ->
            match wave with
            | (Binary { name; data = events; style = _ } | Data { name; data = events; _ })
              when matches_name name -> Some (name, events)
            | Binary _ | Empty _ | Clock _ | Data _ -> None)
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
    (* Iterate through all of the events and find the first point where all the conditions
       are met. *)
    With_return.with_return (fun { return } ->
      let num_found = ref 0 in
      for i = 0 to num_events - 1 do
        let conditions_met =
          List.fold2_exn
            ~init:true
            conditions
            events_per_condition
            ~f:(fun acc { condition; how_to_find = _ } events ->
              let ev = Data.get events i in
              acc && condition ev)
        in
        if conditions_met
        then (
          Int.incr num_found;
          if !num_found = n then return (Some i))
      done;
      None)
  ;;
end
