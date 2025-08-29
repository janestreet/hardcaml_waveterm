open Base
open Hardcaml

module type S = Render_intf.S

module M = Render_intf.M

module Make (Data : Data.S) (Wave : Wave.M(Data).S) (Waves : Waves.M(Data)(Wave).S) =
struct
  open Draw
  open Wave

  (* The user specifies the wave width with an integer - here we encode the integer into
       a rendering representation.

       {v
       -3 -> 3 cycles encoded into 1 char
       -2 -> 2 cycles encoded into 1 char
       -1 -> 1 cycle encoded into 1 char
        0 -> 1 cyle encoded into 2 chars
        1 -> 1 cycle encoded into 4 char
        2 -> 1 cycle encoded into 6 chars
       v}

       Note that when we encode 1 cycle into multiple chars, we require an even number of
       chars - this is because to render the clock properly it must be even. *)
  let wave_width_of_code wave_width_code =
    if wave_width_code = -1
    then `Chars_per_cycle 1
    else if wave_width_code < 0
    then `Cycles_per_char (-wave_width_code)
    else `Chars_per_cycle ((wave_width_code + 1) * 2)
  ;;

  let wave_width (state : Waves.t) =
    let wave_width_code = state.cfg.wave_width in
    wave_width_of_code wave_width_code
  ;;

  let get_max_signal_width_in_chars (state : Waves.t) =
    Array.fold state.waves ~init:0 ~f:(fun m s -> max m (String.length (get_name s)))
  ;;

  let get_max_value_width_in_chars (state : Waves.t) =
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

  let get_estimated_max_value_width (state : Waves.t) =
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

  let total_cycles_in_waveform (state : Waves.t) =
    Array.fold state.waves ~init:0 ~f:(fun m d ->
      max
        m
        (try Data.length (Wave.get_data d) with
         | _ -> 0))
  ;;

  let total_signals_in_waveform (state : Waves.t) = Array.length state.waves

  let get_max_wave_width_in_chars (state : Waves.t) =
    let total_cycles = total_cycles_in_waveform state in
    match wave_width state with
    | `Cycles_per_char cycles_per_char ->
      Int.round_up total_cycles ~to_multiple_of:cycles_per_char / cycles_per_char
    | `Chars_per_cycle chars_per_cycle -> total_cycles * chars_per_cycle
  ;;

  let get_max_wave_height_in_chars (state : Waves.t) start_signal =
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
    let wwidth = get_max_wave_width_in_chars state in
    let wheight = get_max_wave_height_in_chars state state.cfg.start_signal in
    let z = { Rect.r = 0; c = 0; h = wheight; w = 0 } in
    { Window_bounds.signals = { z with w = swidth }
    ; values = { z with w = vwidth }
    ; waves = { z with w = wwidth }
    ; status = z
    }
  ;;

  let draw_clock_cycles_per_char ~ctx ~style ~bounds ~cnt =
    for c = 0 to cnt - 1 do
      draw_piece ~ctx ~style ~bounds ~r:0 ~c BH;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c TH
    done
  ;;

  let draw_clock_chars_per_cycle ~ctx ~style ~bounds ~w ~cnt =
    let cnt = Int.round_up cnt ~to_multiple_of:w / w in
    for c = 0 to cnt - 1 do
      let c = c * w in
      let half_w = w / 2 in
      draw_piece ~ctx ~style ~bounds ~r:0 ~c BR;
      for i = 1 to half_w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + i) H
      done;
      draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + half_w) BL;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c:(c + half_w) TR;
      for i = 1 to half_w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:1 ~c:(c + half_w + i) H
      done;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c:(c + w) TL
    done
  ;;

  let draw_clock_cycles ~ctx ~style ~bounds ~wave_width ~cnt =
    match wave_width with
    | `Chars_per_cycle w when w = 1 -> draw_clock_cycles_per_char ~ctx ~style ~bounds ~cnt
    | `Chars_per_cycle w -> draw_clock_chars_per_cycle ~ctx ~style ~bounds ~w ~cnt
    | `Cycles_per_char _w -> draw_clock_cycles_per_char ~ctx ~style ~bounds ~cnt
  ;;

  let get_data_bounds_clipped data i =
    let length = Data.length data in
    if length = 0
    then Bits.gnd
    else if i < 0
    then Data.get data 0
    else if i >= length
    then Data.get data (length - 1)
    else Data.get data i
  ;;

  module Binary_rendering = struct
    let low ~ctx ~style ~bounds ~w ~c =
      for i = 0 to w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:1 ~c:(c + i) H
      done
    ;;

    let low_high ~ctx ~style ~bounds ~w ~c =
      draw_piece ~ctx ~style ~bounds ~r:0 ~c BR;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c TL;
      for i = 1 to w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + i) H
      done
    ;;

    let high_low ~ctx ~style ~bounds ~w ~c =
      draw_piece ~ctx ~style ~bounds ~r:0 ~c BL;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c TR;
      for i = 1 to w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:1 ~c:(c + i) H
      done
    ;;

    let high ~ctx ~style ~bounds ~w ~c =
      for i = 0 to w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + i) H
      done
    ;;

    let fuzz ~ctx ~style ~bounds ~c =
      draw_piece ~ctx ~style ~bounds ~r:0 ~c BH;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c TH
    ;;
  end

  let get_transition_data ~data ~chars_per_cycle ~off =
    let rec f first_value i =
      if i = chars_per_cycle
      then true
      else if Bits.equal first_value (get_data_bounds_clipped data (off + i))
      then f first_value (i + 1)
      else false
    in
    let first_value = get_data_bounds_clipped data off in
    if f first_value 1 then Some first_value else None
  ;;

  let draw_binary_data_chars_per_cycle ~ctx ~style ~(bounds : Rect.t) ~w ~data ~off =
    (* [w] represents the number of chars per cycle and must be 1 or more *)
    assert (w > 0);
    let rec f prev_data c cycle =
      if c >= bounds.w || cycle >= Data.length data
      then ()
      else (
        let cur_data = get_data_bounds_clipped data cycle in
        (match Bits.to_bool prev_data, Bits.to_bool cur_data with
         | false, false -> Binary_rendering.low ~ctx ~style ~bounds ~w ~c
         | true, false -> Binary_rendering.high_low ~ctx ~style ~bounds ~w ~c
         | false, true -> Binary_rendering.low_high ~ctx ~style ~bounds ~w ~c
         | true, true -> Binary_rendering.high ~ctx ~style ~bounds ~w ~c);
        f cur_data (c + w) (cycle + 1))
    in
    f (get_data_bounds_clipped data off) 0 off
  ;;

  let draw_binary_data_cycles_per_char ~ctx ~style ~(bounds : Rect.t) ~w ~data ~off =
    (* [w] represents the number of cycles per char and must be greater than 0 *)
    assert (w > 0);
    let rec f prev_data c cycle =
      if c >= bounds.w || cycle >= Data.length data
      then ()
      else (
        let cur_data =
          get_transition_data ~data ~chars_per_cycle:w ~off:cycle
          |> Option.map ~f:Bits.to_bool
        in
        (match prev_data, cur_data with
         | _, None -> Binary_rendering.fuzz ~ctx ~style ~bounds ~c
         | (Some false | None), Some false ->
           Binary_rendering.low ~ctx ~style ~bounds ~w:1 ~c
         | Some true, Some false -> Binary_rendering.high_low ~ctx ~style ~bounds ~w:1 ~c
         | Some false, Some true -> Binary_rendering.low_high ~ctx ~style ~bounds ~w:1 ~c
         | (Some true | None), Some true ->
           Binary_rendering.high ~ctx ~style ~bounds ~w:1 ~c);
        f cur_data (c + 1) (cycle + w))
    in
    f None 0 off
  ;;

  let draw_binary_data ~ctx ~style ~bounds ~wave_width ~data ~off =
    match wave_width with
    | `Chars_per_cycle w ->
      draw_binary_data_chars_per_cycle ~ctx ~style ~bounds ~w ~data ~off
    | `Cycles_per_char w ->
      draw_binary_data_cycles_per_char ~ctx ~style ~bounds ~w ~data ~off
  ;;

  module Data_rendering = struct
    let transition ~ctx ~style ~bounds ~w ~c =
      draw_piece ~ctx ~style ~bounds ~r:0 ~c T;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c V;
      draw_piece ~ctx ~style ~bounds ~r:2 ~c Tu;
      for c = c + 1 to c + w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:0 ~c H;
        draw_piece ~ctx ~style ~bounds ~r:2 ~c H
      done
    ;;

    let extend ~ctx ~style ~bounds ~w ~c =
      for c = c to c + w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:0 ~c H;
        draw_piece ~ctx ~style ~bounds ~r:2 ~c H
      done
    ;;

    let fuzz ~ctx ~style ~bounds ~c =
      draw_piece ~ctx ~style ~bounds ~r:0 ~c BH;
      draw_piece ~ctx ~style ~bounds ~r:1 ~c F;
      draw_piece ~ctx ~style ~bounds ~r:2 ~c TH
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
    let putc i ch = draw_char ~ctx ~style ~bounds ~r ~c:(c + i) ch in
    let length = String.length data in
    if length <= max_length
    then
      for i = 0 to length - 1 do
        putc i data.[i]
      done
    else (
      match alignment with
      | Left ->
        for i = 0 to max_length - 1 do
          putc i (if i = max_length - 1 then '.' else data.[i])
        done
      | Right ->
        for i = 0 to max_length - 1 do
          putc i (if i = 0 then '.' else data.[length - 1 - (max_length - 1 - i)])
        done)
  ;;

  let draw_data_chars_per_cycle ~ctx ~style ~bounds ~to_str ~alignment ~w ~data ~off =
    (* [w] represents the number of chars per cycle and must be 1 or more *)
    assert (w > 0);
    let rec f prev_data_space prev_data c cycle =
      let draw_previous_text_data () =
        draw_text_data
          ~ctx
          ~style
          ~bounds
          ~alignment
          ~r:1
          ~c:(c - prev_data_space)
          ~max_length:prev_data_space
          (to_str prev_data)
      in
      if c >= bounds.w || cycle >= Data.length data
      then draw_previous_text_data ()
      else (
        let cur_data = get_data_bounds_clipped data cycle in
        if Bits.equal prev_data cur_data
        then (
          Data_rendering.extend ~ctx ~style ~bounds ~w ~c;
          let prev_data_space_except_at_start =
            prev_data_space + w - if c = 0 then 1 else 0
          in
          f prev_data_space_except_at_start cur_data (c + w) (cycle + 1))
        else (
          draw_previous_text_data ();
          Data_rendering.transition ~ctx ~style ~bounds ~w ~c;
          f (w - 1) cur_data (c + w) (cycle + 1)))
    in
    f 0 (get_data_bounds_clipped data off) 0 off
  ;;

  let draw_data_cycles_per_char ~ctx ~style ~bounds ~to_str ~alignment ~w ~data ~off =
    (* [w] represents the number of cycles per char and must be 1 or more *)
    assert (w > 0);
    let rec f prev_data_space prev_data c cycle =
      let draw_previous_text_data () =
        Option.iter prev_data ~f:(fun prev_data ->
          draw_text_data
            ~ctx
            ~style
            ~bounds
            ~alignment
            ~r:1
            ~c:(c - prev_data_space)
            ~max_length:prev_data_space
            (to_str prev_data))
      in
      if c >= bounds.w || cycle >= Data.length data
      then draw_previous_text_data ()
      else (
        let cur_data = get_transition_data ~data ~chars_per_cycle:w ~off:cycle in
        match prev_data, cur_data with
        | None, None ->
          Data_rendering.fuzz ~ctx ~style ~bounds ~c;
          f 0 None (c + 1) (cycle + w)
        | Some _, None ->
          draw_previous_text_data ();
          Data_rendering.fuzz ~ctx ~style ~bounds ~c;
          f 0 None (c + 1) (cycle + w)
        | None, Some cur_data ->
          Data_rendering.extend ~ctx ~style ~bounds ~w:1 ~c;
          let prev_data_space_except_at_start = if c = 0 then 0 else 1 in
          f prev_data_space_except_at_start (Some cur_data) (c + 1) (cycle + w)
        | Some prev_data, Some cur_data when Bits.equal prev_data cur_data ->
          Data_rendering.extend ~ctx ~style ~bounds ~w:1 ~c;
          f (prev_data_space + 1) (Some cur_data) (c + 1) (cycle + w)
        | Some _, Some cur_data ->
          draw_previous_text_data ();
          Data_rendering.transition ~ctx ~style ~bounds ~w:1 ~c;
          f 0 (Some cur_data) (c + 1) (cycle + w))
    in
    f 0 None 0 off
  ;;

  let draw_data ~ctx ~style ~bounds ~to_str ~alignment ~wave_width ~data ~off =
    match wave_width with
    | `Chars_per_cycle w ->
      draw_data_chars_per_cycle ~ctx ~style ~bounds ~to_str ~alignment ~w ~data ~off
    | `Cycles_per_char w ->
      draw_data_cycles_per_char ~ctx ~style ~bounds ~to_str ~alignment ~w ~data ~off
  ;;

  let rec draw_iter i (bounds : Rect.t) (state : Waves.t) f =
    if i < Array.length state.waves && bounds.h > 0
    then (
      let wah = Wave.get_height_in_chars state.waves.(i) in
      (* render the wave - constrain the bounds to the size of the wave *)
      f i { bounds with h = min bounds.h wah } state.waves.(i);
      draw_iter (i + 1) { bounds with r = bounds.r + wah; h = bounds.h - wah } state f)
  ;;

  type 'a draw_item = ?style:Style.t -> ctx:Draw.ctx -> bounds:Rect.t -> Waves.t -> 'a

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

  let draw_cursor ~ctx ~(bounds : Rect.t) ~wave_cursor ~primary ~(state : Waves.t) =
    let c =
      let cycle_offset = wave_cursor - state.cfg.start_cycle in
      match wave_width state with
      | `Chars_per_cycle w -> cycle_offset * w
      | `Cycles_per_char w -> cycle_offset / w
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
    (state : Waves.t)
    =
    let wave_width = wave_width state in
    fill ~ctx ~bounds ~style ' ';
    let maybe_fill ~bounds ~style:(local_style : Style.t) =
      (* If the local signal style differs, then we need to fill it first *)
      if not (Style.equal_colour style.fg local_style.fg)
      then fill ~ctx ~bounds ~style:local_style ' '
    in
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let off = state.cfg.start_cycle in
      (match wave with
       | Empty _ -> ()
       | Clock { name = _; style } ->
         maybe_fill ~bounds ~style:style.style;
         draw_clock_cycles ~ctx ~style:style.style ~bounds ~wave_width ~cnt:bounds.w
       | Binary { name = _; data; style } ->
         maybe_fill ~bounds ~style:style.style;
         let off = min (Data.length data - 1) off in
         draw_binary_data ~ctx ~style:style.style ~bounds ~wave_width ~data ~off
       | Data { name = _; data; wave_format = _; text_alignment; style } ->
         maybe_fill ~bounds ~style:style.style;
         let off = min (Data.length data - 1) off in
         draw_data
           ~ctx
           ~style:style.style
           ~bounds
           ~alignment:text_alignment
           ~to_str:(Wave.get_to_str wave)
           ~wave_width
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
    (state : Waves.t)
    =
    fill ~ctx ~bounds ~style ' ';
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let wah = Wave.get_height_in_chars wave in
      let r = (wah - 1) / 2 in
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
    (state : Waves.t)
    =
    fill ~ctx ~bounds ~style ' ';
    let off =
      match wave_cursor with
      | Some wave_cursor when wave_cursor >= 0 -> wave_cursor
      | None | Some _ -> state.cfg.start_cycle
    in
    let max_string_length = ref 0 in
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let wah = Wave.get_height_in_chars wave in
      let r = (wah - 1) / 2 in
      (match wave with
       | Empty _ | Clock _ -> ()
       | Binary { name = _; data; style = _ } ->
         let d = get_data_bounds_clipped data off in
         let str = Bits.to_bstr d in
         max_string_length := max !max_string_length (String.length str);
         draw_scroll_string_right ~ctx ~style ~bounds ~r ~c:state.cfg.value_scroll str
       | Data { name = _; data; wave_format; text_alignment = _; style = _ } ->
         let d = get_data_bounds_clipped data off in
         let to_str = Wave.get_to_str wave in
         let str = add_value_prefix wave_format.current d (to_str d) in
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

  let draw_status ?(style = Style.default) ?wave_cursor ~ctx ~bounds (state : Waves.t) =
    fill ~ctx ~bounds ~style ' ';
    draw_string
      ~ctx
      ~style
      ~bounds
      ~r:0
      ~c:0
      (Printf.sprintf
         "cycle=%i cursor=%s w=%i sc=%i vs=%i"
         state.cfg.start_cycle
         (Option.value_map wave_cursor ~default:"?" ~f:Int.to_string)
         state.cfg.wave_width
         state.cfg.signal_scroll
         state.cfg.value_scroll)
  ;;

  let draw_ui
    ?signals_alignment
    ?(style = Window_styles.default Style.default)
    ?(bounds : Window_bounds.t option)
    ~ctx
    (state : Waves.t)
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
      then draw_piece ~ctx ~bounds ~style ~r:0 ~c:22 Up_arrow
      else if i - offset = bounds.h - 1 && i < help_length - 1
      then draw_piece ~ctx ~bounds ~style ~r:(bounds.h - 1) ~c:22 Down_arrow
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
        { cycle : int
        ; signal_index : int option
        }
    | Value of { signal_index : int option }
    | Signal of { signal_index : int option }
    | Status
    | No_pick

  let pick ~(bounds : Window_bounds.t) ~r ~c (state : Waves.t) =
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
      match wave_width state with
      | `Chars_per_cycle w -> state.cfg.start_cycle + (c / w)
      | `Cycles_per_char w -> state.cfg.start_cycle + (c * w)
    in
    if in_rect bounds.waves
    then
      Wave
        { cycle = get_wave_offset bounds.waves
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

    let get_max_height border (state : Waves.t) =
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
