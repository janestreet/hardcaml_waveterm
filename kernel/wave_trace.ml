open Base
open Hardcaml

module type Target = sig
  type ctx
  type style

  (** Both primitives must clip to [bounds] themselves; the engine relies on it. *)
  val draw_glyph
    :  ctx:ctx
    -> style:style
    -> bounds:Rect.t
    -> r:int
    -> c:int
    -> Glyph.t
    -> unit

  val draw_int : ctx:ctx -> style:style -> bounds:Rect.t -> r:int -> c:int -> int -> unit
end

module Make (Data : Hardcaml.Wave_data.S) (Target : Target) = struct
  let ones data = Bits.ones (Data.width data)

  (* Before the first event, both planes are all ones for X, or all zeros for 0. *)
  let bits_before_first_event data ~four_state ~default =
    if four_state then ones data else default
  ;;

  (* Two-state data has no mask plane. Reuse the per-wave zero mask for every event. *)
  let mask_at_index data index ~four_state ~default =
    if four_state then Data.get_mask_at_index data index else default
  ;;

  (* Read the value in effect at [time] via the event store, rather than indexing a
     per-time array. An event takes effect at its timestamp and holds until the next. *)
  let value_at_time data time ~four_state ~default =
    match Data.find_insertion_index data time with
    | None -> bits_before_first_event data ~four_state ~default
    | Some index -> Data.get_data_at_index data index
  ;;

  let get_data_bounds_clipped data i ~four_state ~default =
    let length = Data.length data in
    if length = 0
    then bits_before_first_event data ~four_state ~default
    else if i < 0
    then value_at_time data ~four_state ~default 0
    else if i >= length
    then value_at_time data ~four_state ~default (length - 1)
    else value_at_time data ~four_state ~default i
  ;;

  let mask_at_time data time ~four_state ~default =
    match Data.find_insertion_index data time with
    | None -> bits_before_first_event data ~four_state ~default
    | Some index -> mask_at_index data index ~four_state ~default
  ;;

  let get_mask_bounds_clipped data i ~four_state ~default =
    let length = Data.length data in
    if not four_state
    then default
    else if length = 0
    then ones data
    else if i < 0
    then mask_at_time data ~four_state ~default 0
    else if i >= length
    then mask_at_time data ~four_state ~default (length - 1)
    else mask_at_time data ~four_state ~default i
  ;;

  (* Value equality for transition detection. 2-state values never consult the mask. *)
  let planes_equal ~four_state bits0 mask0 bits1 mask1 =
    Bits.equal bits0 bits1 && ((not four_state) || Bits.equal mask0 mask1)
  ;;

  module Binary_rendering = struct
    let low ~ctx ~style ~bounds ~w ~c =
      for i = 0 to w - 1 do
        Target.draw_glyph ~ctx ~style ~bounds ~r:1 ~c:(c + i) Horizontal
      done
    ;;

    let low_high ~ctx ~style ~bounds ~w ~c =
      Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c Top_left;
      Target.draw_glyph ~ctx ~style ~bounds ~r:1 ~c Bottom_right;
      for i = 1 to w - 1 do
        Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c:(c + i) Horizontal
      done
    ;;

    let high_low ~ctx ~style ~bounds ~w ~c =
      Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c Top_right;
      Target.draw_glyph ~ctx ~style ~bounds ~r:1 ~c Bottom_left;
      for i = 1 to w - 1 do
        Target.draw_glyph ~ctx ~style ~bounds ~r:1 ~c:(c + i) Horizontal
      done
    ;;

    let high ~ctx ~style ~bounds ~w ~c =
      for i = 0 to w - 1 do
        Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c:(c + i) Horizontal
      done
    ;;

    let fuzz ~ctx ~style ~bounds ~c =
      Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c Bottom_half;
      Target.draw_glyph ~ctx ~style ~bounds ~r:1 ~c Top_half
    ;;

    (* Mark an impure region by writing X or Z characters onto the line: X ([level] is the
       high line) and Z (the low line). No-op when [impure] is false or [w] is zero. *)
    let annotate_x_or_z ~ctx ~style ~bounds ~level ~impure ~c ~w =
      if impure
      then (
        let ch = if level then 'X' else 'Z' in
        let r = if level then 0 else 1 in
        for c = c to c + w - 1 do
          Target.draw_int ~ctx ~style ~bounds ~r ~c (Char.to_int ch)
        done)
    ;;
  end

  module To_render = struct
    (* Values are held as raw [Four_state_value] planes. On 2-state data the masks are a
       shared all-zero value and are never consulted for transition detection. *)
    type 'a t =
      { at_most_one_transition : bool
      ; first : 'a
      ; first_mask : 'a
      ; last : 'a
      ; last_mask : 'a
      }

    let bits_to_bool t =
      { at_most_one_transition = t.at_most_one_transition
      ; first = Bits.to_bool t.first
      ; first_mask = Bits.to_bool t.first_mask
      ; last = Bits.to_bool t.last
      ; last_mask = Bits.to_bool t.last_mask
      }
    ;;
  end

  (* A position within a wave's event store. As we render left-to-right we advance this
     cursor rather than re-scanning the events from the start for each character. *)
  type cursor =
    { next_event_index : int
    ; current_data : Bits.t
    ; current_mask : Bits.t
    }

  let create_cursor data ~four_state ~default ~time =
    let current_event_index = Data.find_insertion_index data time in
    let current_data, current_mask =
      match current_event_index with
      | None ->
        let initial = bits_before_first_event data ~four_state ~default in
        initial, initial
      | Some index ->
        Data.get_data_at_index data index, mask_at_index data index ~four_state ~default
    in
    let next_event_index =
      match current_event_index with
      | None -> 0
      | Some index -> index + 1
    in
    { next_event_index; current_data; current_mask }
  ;;

  let advance_through data cursor ~four_state ~default ~time =
    let rec loop next_event_index current_data current_mask =
      (* An event takes effect at its timestamp and holds until the next event. *)
      if next_event_index >= Data.num_events data
         || Data.get_time_at_index data next_event_index > time
      then { next_event_index; current_data; current_mask }
      else
        loop
          (next_event_index + 1)
          (Data.get_data_at_index data next_event_index)
          (mask_at_index data next_event_index ~four_state ~default)
    in
    loop cursor.next_event_index cursor.current_data cursor.current_mask
  ;;

  let summarize_range data cursor ~four_state ~default ~start_time ~stop_time ~prev_data =
    let start_cursor =
      advance_through data cursor ~four_state ~default ~time:start_time
    in
    let first = start_cursor.current_data in
    let first_mask = start_cursor.current_mask in
    (* Ranges are half-open, so events at [stop_time] belong to the next character. *)
    let last_event_index = Data.find_insertion_index data (stop_time - 1) in
    (* Jump to the final event instead of walking every event in the rendered character. *)
    let next_cursor =
      match last_event_index with
      | None -> start_cursor
      | Some index ->
        { next_event_index = index + 1
        ; current_data = Data.get_data_at_index data index
        ; current_mask = mask_at_index data index ~four_state ~default
        }
    in
    let last = next_cursor.current_data in
    let last_mask = next_cursor.current_mask in
    let last_event_index = Option.value last_event_index ~default:(-1) in
    let transition_count =
      match prev_data with
      | None -> 0
      | Some (bits, mask) ->
        if planes_equal ~four_state bits mask first first_mask then 0 else 1
    in
    let rec count_transitions current_data current_mask next_event_index transition_count =
      if transition_count > 1 || next_event_index > last_event_index
      then transition_count
      else (
        let next_data = Data.get_data_at_index data next_event_index in
        let next_mask = mask_at_index data next_event_index ~four_state ~default in
        let transition_count =
          if planes_equal ~four_state current_data current_mask next_data next_mask
          then transition_count
          else transition_count + 1
        in
        count_transitions next_data next_mask (next_event_index + 1) transition_count)
    in
    let transition_count =
      count_transitions
        start_cursor.current_data
        start_cursor.current_mask
        start_cursor.next_event_index
        transition_count
    in
    ( { To_render.at_most_one_transition = transition_count <= 1
      ; first
      ; first_mask
      ; last
      ; last_mask
      }
    , next_cursor )
  ;;

  module For_testing = struct
    let get_transition_data ~prev_data ~data ~time_steps_per_char ~off =
      let four_state = Data.is_four_state data in
      let default = Bits.zero (Data.width data) in
      let cursor = create_cursor data ~four_state ~default ~time:off in
      summarize_range
        data
        cursor
        ~four_state
        ~default
        ~start_time:off
        ~stop_time:(off + time_steps_per_char)
        ~prev_data
      |> fst
    ;;
  end

  let draw_binary_data_chars_per_time_step
    ~ctx
    ~style
    ~(bounds : Rect.t)
    ~w
    ~data
    ~four_state
    ~default
    ~off
    =
    (* [w] represents the number of chars per time step and must be 1 or more *)
    assert (w > 0);
    let rec f prev_data c time =
      if c >= bounds.w || time >= Data.length data
      then ()
      else (
        let cur_data =
          Bits.to_bool (get_data_bounds_clipped data time ~four_state ~default)
        in
        let prev_data_bool = Bits.to_bool prev_data in
        (match prev_data_bool, cur_data with
         | false, false -> Binary_rendering.low ~ctx ~style ~bounds ~w ~c
         | true, false -> Binary_rendering.high_low ~ctx ~style ~bounds ~w ~c
         | false, true -> Binary_rendering.low_high ~ctx ~style ~bounds ~w ~c
         | true, true -> Binary_rendering.high ~ctx ~style ~bounds ~w ~c);
        if four_state
        then (
          let cur_mask =
            Bits.to_bool (get_mask_bounds_clipped data time ~four_state ~default)
          in
          let has_edge = Bool.( <> ) prev_data_bool cur_data in
          (* The same impure value carries on into the next drawn time step, which is
             drawn without an edge and so is annotated across all of its chars. *)
          let annotated_by_next_time_step () =
            time + 1 < Data.length data
            && c + w < bounds.w
            && Bool.( = )
                 (Bits.to_bool
                    (get_data_bounds_clipped data (time + 1) ~four_state ~default))
                 cur_data
            && Bool.( = )
                 (Bits.to_bool
                    (get_mask_bounds_clipped data (time + 1) ~four_state ~default))
                 cur_mask
          in
          (* Leave the char holding the transition edge to the edge glyph, unless that
             would leave the impure region without a letter anywhere: a time step with
             only one visible char has no room for both, and then the letter wins. *)
          let skip_edge_char =
            has_edge
            && ((w > 1 && c + 1 < bounds.w) || (cur_mask && annotated_by_next_time_step ())
               )
          in
          Binary_rendering.annotate_x_or_z
            ~ctx
            ~style
            ~bounds
            ~level:cur_data
            ~impure:cur_mask
            ~c:(if skip_edge_char then c + 1 else c)
            ~w:(if skip_edge_char then w - 1 else w));
        f (Bits.of_bool cur_data) (c + w) (time + 1))
    in
    f (get_data_bounds_clipped data off ~four_state ~default) 0 off
  ;;

  let draw_binary_data_time_steps_per_char
    ~ctx
    ~style
    ~(bounds : Rect.t)
    ~w
    ~data
    ~four_state
    ~default
    ~off
    =
    (* [w] represents the number of time steps per char and must be greater than 0 *)
    assert (w > 0);
    let rec f prev_data cursor c time =
      if c >= bounds.w || time >= Data.length data
      then ()
      else (
        let cur_data, cursor =
          summarize_range
            data
            cursor
            ~four_state
            ~default
            ~start_time:time
            ~stop_time:(time + w)
            ~prev_data:
              (Option.map prev_data ~f:(fun (bits, mask) ->
                 Bits.of_bool bits, if four_state then Bits.of_bool mask else default))
        in
        let cur_data = To_render.bits_to_bool cur_data in
        let prev_data_bool =
          match prev_data with
          | None -> cur_data.first
          | Some (bits, _) -> bits
        in
        (match prev_data_bool, cur_data with
         | _, { at_most_one_transition = false; _ } ->
           Binary_rendering.fuzz ~ctx ~style ~bounds ~c
         | false, { last = false; _ } -> Binary_rendering.low ~ctx ~style ~bounds ~w:1 ~c
         | true, { last = false; _ } ->
           Binary_rendering.high_low ~ctx ~style ~bounds ~w:1 ~c
         | false, { last = true; _ } ->
           Binary_rendering.low_high ~ctx ~style ~bounds ~w:1 ~c
         | true, { last = true; _ } -> Binary_rendering.high ~ctx ~style ~bounds ~w:1 ~c);
        (* Annotate every char that isn't fuzz and whose time range holds an impure value.
           A char with at most one transition spans at most two distinct values ([first]
           and [last]), so "impure somewhere in the range" is exactly
           [first_mask || last_mask]. Fuzz chars are deliberately left unannotated: rapid
           activity reads as fuzz whether or not the values within it are impure. *)
        if four_state && cur_data.at_most_one_transition
        then (
          let impure = cur_data.first_mask || cur_data.last_mask in
          (* The letter goes on the line the char holds while impure. X wins over Z when
             the char holds both, matching [Four_state_value.Impurity]. *)
          let level =
            (cur_data.first_mask && cur_data.first)
            || (cur_data.last_mask && cur_data.last)
          in
          (* Defer only if the next visible char can letter the same impure region: fuzz
             is never lettered, and an X there would hide a deferred Z. *)
          let annotated_by_next_char () =
            cur_data.last_mask
            && (Bool.( = ) cur_data.first cur_data.last || not cur_data.first_mask)
            && time + w < Data.length data
            && c + 1 < bounds.w
            &&
            let next_data, _ =
              summarize_range
                data
                cursor
                ~four_state
                ~default
                ~start_time:(time + w)
                ~stop_time:(time + (2 * w))
                ~prev_data:
                  (Some (Bits.of_bool cur_data.last, Bits.of_bool cur_data.last_mask))
            in
            let next_data = To_render.bits_to_bool next_data in
            let next_level =
              (next_data.first_mask && next_data.first)
              || (next_data.last_mask && next_data.last)
            in
            Bool.( = ) next_data.first cur_data.last
            && Bool.( = ) next_data.first_mask cur_data.last_mask
            && next_data.at_most_one_transition
            && Bool.( = ) next_level cur_data.last
          in
          (* A char is a single character wide, so its letter replaces half of any
             transition edge glyph drawn there. Keep the glyph when the region is lettered
             elsewhere; otherwise the letter wins. *)
          let has_edge = Bool.( <> ) prev_data_bool cur_data.last in
          Binary_rendering.annotate_x_or_z
            ~ctx
            ~style
            ~bounds
            ~level
            ~impure:(impure && not (has_edge && annotated_by_next_char ()))
            ~c
            ~w:1);
        f (Some (cur_data.last, cur_data.last_mask)) cursor (c + 1) (time + w))
    in
    f None (create_cursor data ~four_state ~default ~time:off) 0 off
  ;;

  let draw_binary_data data ~ctx ~style ~bounds ~wave_scale ~off =
    let four_state = Data.is_four_state data in
    let default = Bits.zero (Data.width data) in
    let draw, w =
      match wave_scale with
      | `Chars_per_time_step w -> draw_binary_data_chars_per_time_step, w
      | `Time_steps_per_char w -> draw_binary_data_time_steps_per_char, w
    in
    draw ~ctx ~style ~bounds ~w ~data ~four_state ~default ~off
  ;;

  module Data_rendering = struct
    let transition ~ctx ~style ~bounds ~w ~c =
      Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c T_down;
      Target.draw_glyph ~ctx ~style ~bounds ~r:1 ~c Vertical;
      Target.draw_glyph ~ctx ~style ~bounds ~r:2 ~c T_up;
      for c = c + 1 to c + w - 1 do
        Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c Horizontal;
        Target.draw_glyph ~ctx ~style ~bounds ~r:2 ~c Horizontal
      done
    ;;

    let extend ~ctx ~style ~bounds ~w ~c =
      for c = c to c + w - 1 do
        Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c Horizontal;
        Target.draw_glyph ~ctx ~style ~bounds ~r:2 ~c Horizontal
      done
    ;;

    let fuzz ~ctx ~style ~bounds ~c =
      Target.draw_glyph ~ctx ~style ~bounds ~r:0 ~c Bottom_half;
      Target.draw_glyph ~ctx ~style ~bounds ~r:1 ~c Full;
      Target.draw_glyph ~ctx ~style ~bounds ~r:2 ~c Top_half
    ;;
  end

  let draw_text_data
    ~ctx
    ~style
    ~bounds
    ~(alignment : Text_alignment.t)
    ~r
    ~c
    ~max_length
    data
    =
    let puti i scalar = Target.draw_int ~ctx ~style ~bounds ~r ~c:(c + i) scalar in
    let dot = Char.to_int '.' in
    (* Codepoint cells: truncate between characters, replacing malformed UTF-8 with U+FFFD
       just as the canvas does. *)
    let scalars = Utf8.to_scalar_array data in
    let length = Array.length scalars in
    if length <= max_length
    then Array.iteri scalars ~f:puti
    else (
      match alignment with
      | Left ->
        for i = 0 to max_length - 1 do
          puti i (if i = max_length - 1 then dot else scalars.(i))
        done
      | Right ->
        for i = 0 to max_length - 1 do
          puti i (if i = 0 then dot else scalars.(length - 1 - (max_length - 1 - i)))
        done)
  ;;

  let draw_data_chars_per_time_step
    ~ctx
    ~style
    ~bounds
    ~to_str
    ~alignment
    ~w
    ~data
    ~four_state
    ~default
    ~off
    =
    (* [w] represents the number of chars per time step and must be 1 or more *)
    assert (w > 0);
    (* The zero mask for two-state data is shared across the wave, not allocated per
       value, and is never compared for transition detection. *)
    let get_mask time = get_mask_bounds_clipped data time ~four_state ~default in
    let rec f prev_data_space prev_data prev_mask c time =
      let draw_previous_text_data () =
        draw_text_data
          ~ctx
          ~style
          ~bounds
          ~alignment
          ~r:1
          ~c:(c - prev_data_space)
          ~max_length:prev_data_space
          (to_str ~bits:prev_data ~mask:prev_mask)
      in
      if c >= bounds.w || time >= Data.length data
      then draw_previous_text_data ()
      else (
        let cur_data = get_data_bounds_clipped data time ~four_state ~default in
        let cur_mask = get_mask time in
        if planes_equal ~four_state prev_data prev_mask cur_data cur_mask
        then (
          Data_rendering.extend ~ctx ~style ~bounds ~w ~c;
          let prev_data_space_except_at_start =
            prev_data_space + w - if c = 0 then 1 else 0
          in
          f prev_data_space_except_at_start cur_data cur_mask (c + w) (time + 1))
        else (
          draw_previous_text_data ();
          Data_rendering.transition ~ctx ~style ~bounds ~w ~c;
          f (w - 1) cur_data cur_mask (c + w) (time + 1)))
    in
    f 0 (get_data_bounds_clipped data off ~four_state ~default) (get_mask off) 0 off
  ;;

  let draw_data_time_steps_per_char
    ~ctx
    ~style
    ~bounds
    ~to_str
    ~alignment
    ~w
    ~data
    ~four_state
    ~default
    ~off
    =
    (* [w] represents the number of time steps per char and must be 1 or more *)
    assert (w > 0);
    let rec f prev_data_space prev_data cursor c time =
      let draw_previous_text_data () =
        Option.iter prev_data ~f:(fun (prev_data : _ To_render.t) ->
          draw_text_data
            ~ctx
            ~style
            ~bounds
            ~alignment
            ~r:1
            ~c:(c - prev_data_space)
            ~max_length:prev_data_space
            (to_str ~bits:prev_data.last ~mask:prev_data.last_mask))
      in
      if c >= bounds.w || time >= Data.length data
      then draw_previous_text_data ()
      else (
        let cur_data, cursor =
          summarize_range
            data
            cursor
            ~four_state
            ~default
            ~start_time:time
            ~stop_time:(time + w)
            ~prev_data:
              (Option.map prev_data ~f:(fun (p : _ To_render.t) -> p.last, p.last_mask))
        in
        match prev_data, cur_data with
        | _, { at_most_one_transition = false; _ } ->
          (* Multiple transitions internal to the character, fuzz *)
          draw_previous_text_data ();
          Data_rendering.fuzz ~ctx ~style ~bounds ~c;
          f 0 None cursor (c + 1) (time + w)
        | _, { first; first_mask; last; last_mask; _ }
          when not (planes_equal ~four_state first first_mask last last_mask) ->
          (* One transition internal to the character, draw a transition *)
          draw_previous_text_data ();
          Data_rendering.transition ~ctx ~style ~bounds ~w:1 ~c;
          f 0 (Some cur_data) cursor (c + 1) (time + w)
        | (None | Some { at_most_one_transition = false; _ }), _ ->
          (* Previous character was fuzzed or doesn't exist, don't draw a transition in
             this case. *)
          Data_rendering.extend ~ctx ~style ~bounds ~w:1 ~c;
          let prev_data_space_except_at_start = if c = 0 then 0 else 1 in
          f prev_data_space_except_at_start (Some cur_data) cursor (c + 1) (time + w)
        | ( Some { last = previous_last; last_mask = previous_last_mask; _ }
          , { last; last_mask; _ } )
          when planes_equal ~four_state previous_last previous_last_mask last last_mask ->
          (* New data is the same as the old, extend *)
          Data_rendering.extend ~ctx ~style ~bounds ~w:1 ~c;
          f (prev_data_space + 1) (Some cur_data) cursor (c + 1) (time + w)
        | Some _, _ ->
          (* New data is the different than the old, draw a transition *)
          draw_previous_text_data ();
          Data_rendering.transition ~ctx ~style ~bounds ~w:1 ~c;
          f 0 (Some cur_data) cursor (c + 1) (time + w))
    in
    f 0 None (create_cursor data ~four_state ~default ~time:off) 0 off
  ;;

  let draw_data data ~ctx ~style ~bounds ~to_str ~alignment ~wave_scale ~off =
    let four_state = Data.is_four_state data in
    let default = Bits.zero (Data.width data) in
    let draw, w =
      match wave_scale with
      | `Chars_per_time_step w -> draw_data_chars_per_time_step, w
      | `Time_steps_per_char w -> draw_data_time_steps_per_char, w
    in
    draw ~ctx ~style ~bounds ~to_str ~alignment ~w ~data ~four_state ~default ~off
  ;;
end
