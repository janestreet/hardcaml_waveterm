open Base
open Hardcaml

module type S = Render_intf.S

module M = Render_intf.M

module Make (Data : Hardcaml.Wave_data.S) = struct
  open Draw
  open Wave

  (* The user specifies the wave scale with an integer - here we encode the integer into a
     rendering representation.

     {v
       -2 -> 3 time steps encoded into 1 char
       -1 -> 2 time steps encoded into 1 char
        0 -> 1 time step encoded into 1 char
        1 -> 1 time step encoded into 2 chars
        2 -> 1 time step encoded into 3 chars
     v} *)
  let wave_scale_of_code wave_scale_code =
    if wave_scale_code < 0
    then `Time_steps_per_char (1 - wave_scale_code)
    else `Chars_per_time_step (wave_scale_code + 1)
  ;;

  let wave_scale (state : _ Waves.t) = wave_scale_of_code state.cfg.wave_scale

  let get_max_signal_width_in_chars (state : _ Waves.t) =
    Array.fold state.waves ~init:0 ~f:(fun m s -> max m (String.length (get_name s)))
  ;;

  let get_max_value_width_in_chars (state : _ Waves.t) =
    let fold f a d =
      let len = Data.length d in
      let rec g a i = if i = len then a else g (f a (Data.get d i)) (i + 1) in
      g a 0
    in
    Array.fold state.waves ~init:0 ~f:(fun m w ->
      try
        let data = Wave.get_data w in
        let to_str = Wave.get_to_str w in
        let max m s = max m (String.length (to_str s)) in
        fold max m data
      with
      | _ -> m)
  ;;

  let get_estimated_max_value_width (state : _ Waves.t) =
    let unsigned_width =
      let table =
        Array.init 64 ~f:(fun i ->
          if i = 0
          then 1
          else Bits.ones i |> Bits.to_int64_trunc |> Int64.to_string |> String.length)
      in
      fun width -> table.(min 63 width)
    in
    let signed_width =
      let table =
        Array.init 65 ~f:(fun i ->
          if i = 0
          then 1
          else
            Bits.one i
            |> Bits.reverse
            |> Bits.to_int64_trunc
            |> Int64.to_string
            |> String.length)
      in
      fun width -> table.(min 64 width)
    in
    Array.fold state.waves ~init:0 ~f:(fun max_width wave ->
      let bits =
        try Bits.width (Data.get (Wave.get_data wave) 0) with
        | _ -> 0
      in
      let rec get_width fmt =
        match (fmt : Wave_format.t) with
        | Binary -> bits
        | Bit -> 1
        | Bit_or t -> if bits = 0 then 1 else get_width t
        | Hex -> (bits + 3) / 4
        | Unsigned_int -> unsigned_width bits
        | Int -> signed_width bits
        | Custom _ -> 8 (* could add a width hint *)
        | Index s -> List.fold s ~init:0 ~f:(fun mx str -> max mx (String.length str))
        | Map m -> List.fold m ~init:0 ~f:(fun mx (_, str) -> max mx (String.length str))
      in
      max max_width (get_width (Wave.get_format wave)))
  ;;

  let total_time_steps_in_waveform (state : _ Waves.t) =
    Array.fold state.waves ~init:0 ~f:(fun m d ->
      max
        m
        (try Data.length (Wave.get_data d) with
         | _ -> 0))
  ;;

  let total_signals_in_waveform (state : _ Waves.t) = Array.length state.waves

  let get_max_wave_scale_in_chars (state : _ Waves.t) =
    let total_time_steps = total_time_steps_in_waveform state in
    match wave_scale state with
    | `Time_steps_per_char time_steps_per_char ->
      Int.round_up total_time_steps ~to_multiple_of:time_steps_per_char
      / time_steps_per_char
    | `Chars_per_time_step chars_per_time_step -> total_time_steps * chars_per_time_step
  ;;

  let get_max_wave_height_in_chars (state : _ Waves.t) start_signal =
    let rec f acc i =
      if i < Array.length state.waves
      then (
        let wah = Wave.get_height_in_chars state.waves.(i) in
        f (acc + wah) (i + 1))
      else acc
    in
    f 0 start_signal
  ;;

  let get_max_bounds state =
    let swidth = get_max_signal_width_in_chars state in
    let vwidth = get_max_value_width_in_chars state in
    let wwidth = get_max_wave_scale_in_chars state in
    let wheight = get_max_wave_height_in_chars state state.cfg.start_signal in
    let z = { Rect.r = 0; c = 0; h = wheight; w = 0 } in
    { Window_bounds.signals = { z with w = swidth }
    ; values = { z with w = vwidth }
    ; waves = { z with w = wwidth }
    ; status = z
    }
  ;;

  let draw_divider ~ctx ~style ~(bounds : Rect.t) ~wave_scale:_ ~cnt =
    let r = (bounds.h - 1) / 2 in
    for c = 0 to cnt - 1 do
      draw_glyph ~ctx ~style ~bounds ~r ~c Horizontal_dash
    done
  ;;

  module Trace =
    Wave_trace.Make
      (Data)
      (struct
        type ctx = Draw.ctx
        type style = Style.t

        let draw_glyph = Draw.draw_glyph
        let draw_int = Draw.draw_int
      end)

  module To_render = Trace.To_render
  module For_testing = Trace.For_testing

  let draw_binary_data ~ctx ~style ~bounds ~wave_scale ~data ~off =
    Trace.draw_binary_data data ~ctx ~style ~bounds ~wave_scale ~off
  ;;

  let draw_four_state_data ~ctx ~style ~bounds ~to_str ~alignment ~wave_scale ~data ~off =
    (* Values are only built when text is actually formatted, not per event. *)
    let to_str ~bits ~mask = to_str (Four_state_logic.create_exn ~bits ~mask) in
    Trace.draw_data data ~ctx ~style ~bounds ~to_str ~alignment ~wave_scale ~off
  ;;

  let draw_data ~ctx ~style ~bounds ~to_str ~alignment ~wave_scale ~data ~off =
    (* The mask is never inspected, so 2-state data pays nothing for mask handling and
       never builds a [Four_state_value.t]. *)
    let to_str ~bits ~mask:_ = to_str bits in
    Trace.draw_data data ~ctx ~style ~bounds ~to_str ~alignment ~wave_scale ~off
  ;;

  let rec draw_iter i (bounds : Rect.t) (state : _ Waves.t) f =
    if i < Array.length state.waves && bounds.h > 0
    then (
      let wah = Wave.get_height_in_chars state.waves.(i) in
      (* render the wave - constrain the bounds to the size of the wave *)
      f i { bounds with h = min bounds.h wah } state.waves.(i);
      draw_iter (i + 1) { bounds with r = bounds.r + wah; h = bounds.h - wah } state f)
  ;;

  type 'a draw_item =
    ?style:Style.t -> ctx:Draw.ctx -> bounds:Rect.t -> Data.t Waves.t -> 'a

  let with_border
    ~(draw : 'a draw_item)
    ~label
    ?border
    ?(style = Style.default)
    ~ctx
    ~bounds
    state
    =
    let r = draw ~style ~ctx ~bounds state in
    match border with
    | Some border when bounds.w > 0 && bounds.h > 0 ->
      Draw.draw_box
        ~ctx
        ~style:border
        ~bounds:(Window_bounds.expand_for_border bounds)
        label;
      r
    | _ -> r
  ;;

  let draw_cursor ~ctx ~(bounds : Rect.t) ~wave_cursor ~primary ~(state : _ Waves.t) =
    let c =
      let time_offset = wave_cursor - state.cfg.start_time in
      match wave_scale state with
      | `Chars_per_time_step w -> time_offset * w
      | `Time_steps_per_char w -> time_offset / w
    in
    let style =
      if primary then Some { Style.fg = Black; bg = Yellow; bold = false } else None
    in
    for r = 0 to bounds.h - 1 do
      (* assume clipped when drawn *)
      inv ?style ~ctx ~bounds ~r ~c ()
    done
  ;;

  let draw_highlight ~ctx ~(bounds : Rect.t) ~r =
    for c = 0 to bounds.w - 1 do
      inv ~ctx ~bounds ~r ~c ()
    done
  ;;

  let draw_wave
    ?(style = Style.default)
    ?(wave_cursors = [])
    ~selected_wave_index
    ~ctx
    ~bounds
    (state : _ Waves.t)
    =
    let wave_scale = wave_scale state in
    fill ~ctx ~bounds ~style ' ';
    let maybe_fill ~bounds ~style:(local_style : Style.t) =
      (* If the local signal style differs, then we need to fill it first *)
      if not (Style.equal_colour style.fg local_style.fg)
      then fill ~ctx ~bounds ~style:local_style ' '
    in
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let off = state.cfg.start_time in
      (match wave with
       | Empty _ -> ()
       | Divider { name = _; style } ->
         maybe_fill ~bounds ~style:style.style;
         draw_divider ~ctx ~style:style.style ~bounds ~wave_scale ~cnt:bounds.w
       | Binary { name = _; data; style } ->
         maybe_fill ~bounds ~style:style.style;
         let off = min (Data.length data - 1) off in
         draw_binary_data ~ctx ~style:style.style ~bounds ~wave_scale ~data ~off
       | Data { name = _; data; width = _; wave_format = _; text_alignment; style } ->
         maybe_fill ~bounds ~style:style.style;
         let off = min (Data.length data - 1) off in
         (* [Wave_format]'s formatters build lookup structures when unstaged, so build
            only the one this wave needs. *)
         if Data.is_four_state data
         then
           draw_four_state_data
             ~ctx
             ~style:style.style
             ~bounds
             ~alignment:text_alignment
             ~to_str:(Wave.get_to_str_four_state wave)
             ~wave_scale
             ~data
             ~off
         else
           draw_data
             ~ctx
             ~style:style.style
             ~bounds
             ~alignment:text_alignment
             ~to_str:(Wave.get_to_str wave)
             ~wave_scale
             ~data
             ~off);
      let is_selected =
        match selected_wave_index with
        | None -> false
        | Some selected_wave_index -> i = selected_wave_index
      in
      if is_selected
      then
        for r = 0 to Wave.get_height_in_chars wave - 1 do
          draw_highlight ~ctx ~bounds ~r
        done);
    List.iter wave_cursors ~f:(function
      | `Primary wave_cursor -> draw_cursor ~primary:true ~ctx ~bounds ~wave_cursor ~state
      | `Secondary wave_cursor ->
        draw_cursor ~primary:false ~ctx ~bounds ~wave_cursor ~state)
  ;;

  let ssub s a b = String.sub s ~pos:a ~len:b

  let draw_scroll_string ~ctx ~style ~(bounds : Rect.t) ~r ~c str =
    let len = String.length str in
    let w = bounds.w in
    if len <= w
    then draw_string ~ctx ~style ~bounds ~r ~c:0 str
    else (
      let c = min c (len - w) in
      let str =
        try ssub str c w with
        | _ -> ""
      in
      draw_string ~ctx ~style ~bounds ~r ~c:0 str)
  ;;

  let draw_scroll_string_right ~ctx ~style ~(bounds : Rect.t) ~r ~c str =
    let len = String.length str in
    let w = bounds.w in
    let sub_right s o l =
      try ssub s (len - l - o) l with
      | _ -> ""
    in
    let draw_string_right ~ctx ~style ~bounds ~r str =
      let c = w - String.length str in
      draw_string ~ctx ~style ~bounds ~r ~c str
    in
    if len <= w
    then draw_string_right ~ctx ~style ~bounds ~r str
    else (
      let c = min c (len - w) in
      draw_string_right ~ctx ~style ~bounds ~r (sub_right str c w))
  ;;

  let draw_signals
    ?(alignment = Text_alignment.Left)
    ?(style = Style.default)
    ~selected_wave_index
    ~ctx
    ~bounds
    (state : _ Waves.t)
    =
    fill ~ctx ~bounds ~style ' ';
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let wah = Wave.get_height_in_chars wave in
      let r = (wah - 1) / 2 in
      (* Extend the divider line through the signals window, underneath the name. *)
      (match wave with
       | Divider _ ->
         for c = 0 to bounds.w - 1 do
           draw_glyph ~ctx ~style ~bounds ~r ~c Horizontal_dash
         done
       | Empty _ | Binary _ | Data _ -> ());
      (match alignment with
       | Left ->
         draw_scroll_string
           ~ctx
           ~style
           ~bounds
           ~r
           ~c:state.cfg.signal_scroll
           (Wave.get_name wave)
       | Right ->
         draw_scroll_string_right
           ~ctx
           ~style
           ~bounds
           ~r
           ~c:state.cfg.signal_scroll
           (Wave.get_name wave));
      match selected_wave_index with
      | Some selected_wave_index when i = selected_wave_index ->
        draw_highlight ~ctx ~bounds ~r
      | None | Some _ -> ())
  ;;

  let rec add_value_prefix (wave_format : Wave_format.t) bits str =
    let width = Bits.width bits in
    if width = 1
    then str
    else (
      match wave_format with
      | Binary -> [%string "%{width#Int}'b%{str}"]
      | Bit -> str
      | Bit_or t -> add_value_prefix t bits str
      | Hex -> [%string "%{width#Int}'h%{str}"]
      | Unsigned_int -> [%string "%{width#Int}'d%{str}"]
      | Int -> [%string "%{width#Int}'D%{str}"]
      | Custom _ | Index _ | Map _ -> str)
  ;;

  let draw_values
    ?(style = Style.default)
    ?wave_cursor
    ~selected_wave_index
    ~ctx
    ~bounds
    (state : _ Waves.t)
    =
    fill ~ctx ~bounds ~style ' ';
    let off =
      match wave_cursor with
      | Some wave_cursor when wave_cursor >= 0 -> wave_cursor
      | None | Some _ -> state.cfg.start_time
    in
    let max_string_length = ref 0 in
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let wah = Wave.get_height_in_chars wave in
      let r = (wah - 1) / 2 in
      (match wave with
       | Empty _ -> ()
       | Divider _ ->
         (* Extend the divider line through the values window. *)
         for c = 0 to bounds.w - 1 do
           draw_glyph ~ctx ~style ~bounds ~r ~c Horizontal_dash
         done
       | Binary { name = _; data; style = _ } ->
         let four_state = Data.is_four_state data in
         let default = Bits.zero (Data.width data) in
         let d = Trace.get_data_bounds_clipped data off ~four_state ~default in
         let str =
           if four_state
           then
             Four_state_logic.create_exn
               ~bits:d
               ~mask:(Trace.get_mask_bounds_clipped data off ~four_state ~default)
             |> Four_state_logic.to_string
           else Bits.to_bstr d
         in
         max_string_length := max !max_string_length (String.length str);
         draw_scroll_string_right ~ctx ~style ~bounds ~r ~c:state.cfg.value_scroll str
       | Data { name = _; data; width = _; wave_format; text_alignment = _; style = _ } ->
         let four_state = Data.is_four_state data in
         let default = Bits.zero (Data.width data) in
         let d = Trace.get_data_bounds_clipped data off ~four_state ~default in
         let str =
           if four_state
           then
             Four_state_logic.create_exn
               ~bits:d
               ~mask:(Trace.get_mask_bounds_clipped data off ~four_state ~default)
             |> Wave.get_to_str_four_state wave
           else Wave.get_to_str wave d
         in
         let str = add_value_prefix wave_format.current d str in
         max_string_length := max !max_string_length (String.length str);
         draw_scroll_string_right ~ctx ~style ~bounds ~r ~c:state.cfg.value_scroll str);
      let is_selected =
        match selected_wave_index with
        | None -> false
        | Some selected_wave_index -> i = selected_wave_index
      in
      if is_selected then draw_highlight ~ctx ~bounds ~r);
    !max_string_length
  ;;

  let draw_status ?(style = Style.default) ?wave_cursor ~ctx ~bounds (state : _ Waves.t) =
    fill ~ctx ~bounds ~style ' ';
    draw_string
      ~ctx
      ~style
      ~bounds
      ~r:0
      ~c:0
      (Printf.sprintf
         "time=%i cursor=%s w=%i sc=%i vs=%i"
         state.cfg.start_time
         (Option.value_map wave_cursor ~default:"?" ~f:Int.to_string)
         state.cfg.wave_scale
         state.cfg.signal_scroll
         state.cfg.value_scroll)
  ;;

  let draw_ui
    ?signals_alignment
    ?(style = Window_styles.default Style.default)
    ?(bounds : Window_bounds.t option)
    ~ctx
    (state : _ Waves.t)
    =
    let bounds =
      match bounds with
      | None -> Window_bounds.fit_to_window (get_bounds ctx)
      | Some b -> b
    in
    with_border
      ~draw:(draw_signals ?alignment:signals_alignment ~selected_wave_index:None)
      ~label:"Signals"
      ~style:style.signals
      ?border:style.border
      ~ctx
      ~bounds:bounds.signals
      state;
    ignore
      (with_border
         ~draw:(draw_values ?wave_cursor:None ~selected_wave_index:None)
         ~label:"Values"
         ~style:style.values
         ?border:style.border
         ~ctx
         ~bounds:bounds.values
         state
       : int);
    with_border
      ~draw:(draw_wave ?wave_cursors:None ~selected_wave_index:None)
      ~label:"Waves"
      ~style:style.waves
      ?border:style.border
      ~ctx
      ~bounds:bounds.waves
      state;
    with_border
      ~draw:(draw_status ?wave_cursor:None)
      ~label:"Status"
      ~style:style.status
      ?border:style.border
      ~ctx
      ~bounds:bounds.status
      state
  ;;

  let draw_help ?(style = Style.default) ~ctx ~(bounds : Rect.t) help ~offset =
    let help_length = List.length help in
    Draw.fill ~ctx ~style ~bounds ' ';
    List.iteri help ~f:(fun i { Key_help.key; descr } ->
      (* If there are elements before or after the displayed values, draw an arrow *)
      if i = offset && offset <> 0
      then draw_glyph ~ctx ~bounds ~style ~r:0 ~c:22 Up_arrow
      else if i - offset = bounds.h - 1 && i < help_length - 1
      then draw_glyph ~ctx ~bounds ~style ~r:(bounds.h - 1) ~c:22 Down_arrow
      else if i >= offset
      then
        (* Print the help string *)
        draw_string
          ~ctx
          ~bounds
          ~style
          ~r:(i - offset)
          ~c:0
          [%string "%{key#String:20}    %{descr}"])
  ;;

  type pick =
    | Wave of
        { time : int
        ; signal_index : int option
        }
    | Value of { signal_index : int option }
    | Signal of { signal_index : int option }
    | Status
    | No_pick

  let pick ~(bounds : Window_bounds.t) ~r ~c (state : _ Waves.t) =
    let in_rect (b : Rect.t) = r >= b.r && c >= b.c && r < b.r + b.h && c < b.c + b.w in
    let get_signal_offset (b : Rect.t) =
      let r = r - b.r in
      let rec f row i =
        if i >= Array.length state.waves
        then None
        else (
          let wah = Wave.get_height_in_chars state.waves.(i) in
          if r >= row && r < row + wah then Some i else f (row + wah) (i + 1))
      in
      f 0 state.cfg.start_signal
    in
    let get_wave_offset (bounds : Rect.t) =
      let c = c - bounds.c in
      match wave_scale state with
      | `Chars_per_time_step w -> state.cfg.start_time + (c / w)
      | `Time_steps_per_char w -> state.cfg.start_time + (c * w)
    in
    if in_rect bounds.waves
    then
      Wave
        { time = get_wave_offset bounds.waves
        ; signal_index = get_signal_offset bounds.waves
        }
    else if in_rect bounds.values
    then Value { signal_index = get_signal_offset bounds.values }
    else if in_rect bounds.signals
    then Signal { signal_index = get_signal_offset bounds.signals }
    else if in_rect bounds.status
    then Status
    else No_pick
  ;;

  module Static = struct
    let border_ext = function
      | None -> 0
      | Some _ -> 2
    ;;

    let get_max_height border (state : _ Waves.t) =
      border_ext border + get_max_wave_height_in_chars state state.cfg.start_signal
    ;;

    let draw
      ?signals_alignment
      ?signals
      ?values
      ?waves
      ?(style = Window_styles.default Style.default)
      ?rows
      ?cols
      ?signals_width
      state
      =
      (* inferred width and height *)
      let cols =
        match cols with
        | None -> 80
        | Some x -> x
      in
      let rows =
        match rows with
        | None -> get_max_height style.border state
        | Some x -> x
      in
      (* do drawing *)
      let ctx = Draw.init ~rows ~cols in
      let bounds =
        Window_bounds.fit_to_window
          ?signals_width
          ?signals
          ?values
          ?waves
          { r = 0; c = 0; h = rows; w = cols }
      in
      draw_ui ?signals_alignment ~style ~ctx ~bounds state;
      (* return context *)
      ctx
    ;;

    let draw_full ?signals_alignment ?(style = Window_styles.default Style.default) state =
      let bounds = get_max_bounds state in
      let ext = border_ext style.border in
      let get_ctx (b : Rect.t) =
        let b = { b with w = b.w + ext; h = b.h + ext } in
        let ctx = Draw.init ~rows:b.h ~cols:b.w in
        let b = if ext = 0 then b else Window_bounds.shrink_for_border b in
        b, ctx
      in
      let b, sctx = get_ctx bounds.signals in
      with_border
        ~draw:(draw_signals ?alignment:signals_alignment ~selected_wave_index:None)
        ?border:style.border
        ~label:"Signals"
        ~style:style.signals
        ~ctx:sctx
        ~bounds:b
        state;
      let b, vctx = get_ctx bounds.values in
      ignore
      @@ with_border
           ~draw:(draw_values ?wave_cursor:None ~selected_wave_index:None)
           ?border:style.border
           ~label:"Values"
           ~style:style.values
           ~ctx:vctx
           ~bounds:b
           state;
      let b, wctx = get_ctx bounds.waves in
      with_border
        ~draw:(draw_wave ?wave_cursors:None ~selected_wave_index:None)
        ?border:style.border
        ~label:"Waves"
        ~style:style.waves
        ~ctx:wctx
        ~bounds:b
        state;
      sctx, vctx, wctx
    ;;
  end
end
