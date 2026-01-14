open Base
open Hardcaml_waveterm_kernel
module M = Waveform_window_intf.M

module With_bounds = struct
  type 'a t =
    { bounds : Rect.t
    ; window : 'a
    }
  [@@deriving sexp_of]
end

module Make
    (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
    (M : Hardcaml_waveterm_kernel.Expert.M(Data).S) =
struct
  open M
  module Hierarchy = Hierarchy.Make (Data) (M)
  module Signals_window = Signals_window.Make (Data) (M) (Hierarchy)
  module Values_window = Values_window.Make (Data) (M) (Hierarchy)
  module Waves_window = Waves_window.Make (Data) (M) (Hierarchy)
  module Ui_state = Ui_state.Make (Data) (M) (Hierarchy)

  type t =
    { hierarchy : Hierarchy.t
    ; signals_window : Signals_window.t With_bounds.t
    ; values_window : Values_window.t With_bounds.t
    ; waves_window : Waves_window.t With_bounds.t
    ; scroll_signals : Scroll.HScrollbar.t
    ; scroll_values : Scroll.HScrollbar.t
    ; scroll_waves : Scroll.HScrollbar.t
    ; scroll_vert : Scroll.VScrollbar.t
    ; max_signal_offset : int
    ; max_cycle_offset : int
    ; ui_state_file : string
    ; key_table : (Key_actions.Key.t, t -> Event_response.t) Hashtbl.t
    }
  [@@deriving sexp_of, fields ~getters]

  let default_waves_style = Waves_window.style
  let get_signal_offset (t : t) = t.hierarchy.cfg.start_signal

  let set_signal_offset (t : t) offset =
    t.hierarchy.cfg.start_signal <- max 0 (min (t.max_signal_offset - 1) offset);
    Scroll.Scrollbar.set_offset t.scroll_vert offset
  ;;

  let get_cycle_offset (t : t) = t.hierarchy.cfg.start_cycle

  let set_cycle_offset (t : t) offset =
    t.hierarchy.cfg.start_cycle <- max 0 (min (t.max_cycle_offset - 1) offset);
    Scroll.Scrollbar.set_offset t.scroll_waves offset
  ;;

  let get_cursor_offset (t : t) = Cursors.get_offset t.hierarchy.cursors

  let set_cursor_offset (t : t) offset =
    Cursors.set_offset
      t.hierarchy.cursors
      (Some (max 0 (min (t.max_cycle_offset - 1) offset)))
  ;;

  let _get_signal_name_offset (t : t) = t.hierarchy.cfg.signal_scroll

  let set_signal_name_offset (t : t) offset =
    t.hierarchy.cfg.signal_scroll
    <- max
         0
         (min (Signals_window.max_signal_name_width t.signals_window.window - 1) offset);
    Scroll.Scrollbar.set_offset t.scroll_signals offset
  ;;

  let _get_value_offset (t : t) = t.hierarchy.cfg.value_scroll

  let set_value_offset (t : t) offset =
    t.hierarchy.cfg.value_scroll
    <- max 0 (min (Values_window.max_value_width t.values_window.window - 1) offset);
    Scroll.Scrollbar.set_offset t.scroll_values offset
  ;;

  let cycles_displayed t =
    match M.Render.wave_width_of_code t.hierarchy.cfg.wave_width with
    | `Cycles_per_char w ->
      (* This isn't the actual bounds - it needs adjustment for the border. *)
      t.waves_window.bounds.w * w
    | `Chars_per_cycle w -> Int.round_down ~to_multiple_of:w t.waves_window.bounds.w / w
  ;;

  let scroll_step t =
    match Render.wave_width_of_code t.hierarchy.cfg.wave_width with
    | `Chars_per_cycle _ -> 1
    | `Cycles_per_char w -> w
  ;;

  module Keys =
    Key_handlers.Waveform_window.Make (Data) (M) (Hierarchy) (Ui_state)
      (struct
        type nonrec t = t

        let cfg (t : t) = t.hierarchy.cfg
        let cursors (t : t) = t.hierarchy.cursors
        let hierarchy (t : t) = t.hierarchy
        let cycles_displayed = cycles_displayed
        let set_cycle_offset = set_cycle_offset
        let get_cycle_offset = get_cycle_offset
        let set_signal_offset = set_signal_offset
        let get_signal_offset = get_signal_offset
        let max_cycle_offset t = t.max_cycle_offset
        let max_signal_offset t = t.max_signal_offset

        let change_selected_signal_index t ~by =
          Hierarchy.change_selected_signal_index t.hierarchy ~delta:by
        ;;

        let max_view t =
          (* [pick] the bottom right character of the window bounds and see what
             cycle/signal we select *)
          let rendered_waves = Hierarchy.get_currently_rendered_waves t.hierarchy in
          let bounds = Draw_notty.Border.adjust t.waves_window.bounds in
          match
            Render.pick
              ~bounds:
                { waves = bounds
                ; values = Rect.empty
                ; signals = Rect.empty
                ; status = Rect.empty
                }
              ~r:(bounds.r + bounds.h - 1)
              ~c:(bounds.c + bounds.w - 1)
              rendered_waves
          with
          | Wave { cycle; signal_index } -> Some (cycle, signal_index)
          | _ -> None
        ;;

        let set_cursor_offset = set_cursor_offset
        let get_cursor_offset = get_cursor_offset
        let scroll_step = scroll_step
        let ui_state_file t = t.ui_state_file
      end)

  (* We optionally take in an existing [Waveform_window.t] to use in constructing the new
     [t]. This is useful for when we want to resize an existing window. *)
  let create ?(waveform : t option) ~rows ~cols ~ui_state_file (waves : M.Waves.t) keys =
    let hierarchy =
      (* hierarchy is subtle - it is a shared state that needs to survive when the
         waveform is recreated on things like resizing. It is also stored inside the
         signals, values and waves windows.

         There is an argument for hierarchy to be external to the viewer windows and
         passed in instead. *)
      match waveform with
      | None -> Hierarchy.of_waves waves
      | Some waveform -> waveform.hierarchy
    in
    let full_bounds = { Rect.r = 0; c = 0; w = cols; h = rows } in
    let signals_width = waves.cfg.signals_width in
    let values_width = waves.cfg.values_width in
    let signals =
      match waveform with
      | None -> Signals_window.create ~waves ~hierarchy
      | Some waveform -> waveform.signals_window.window
    in
    let scroll_vert, bounds_wo_vert_scroll =
      Scroll.VScrollbar.create_right_aligned
        ~container:full_bounds
        ~range:(Signals_window.num_signals signals)
        ()
    in
    let signals_window, scroll_signals, bounds_wo_vert_and_horz_scroll =
      let c, w = 0, signals_width in
      let scroll_signals, bounds_wo_vert_and_horz_scroll =
        Scroll.HScrollbar.create_bottom_aligned
          ~container:bounds_wo_vert_scroll
          ~c
          ~w
          ~range:(Signals_window.max_signal_name_width signals)
          ()
      in
      let signals_window : Signals_window.t With_bounds.t =
        { bounds = { bounds_wo_vert_and_horz_scroll with c; w }; window = signals }
      in
      signals_window, scroll_signals, bounds_wo_vert_and_horz_scroll
    in
    let values =
      match waveform with
      | None -> Values_window.create ~waves ~hierarchy
      | Some waveform -> waveform.values_window.window
    in
    let values_window, scroll_values =
      let c, w = signals_width, values_width in
      let scroll_values, _ =
        Scroll.HScrollbar.create_bottom_aligned
          ~container:bounds_wo_vert_scroll
          ~c
          ~w
          ~range:(Values_window.max_value_width values)
          ()
      in
      let values_window : Values_window.t With_bounds.t =
        { bounds = { bounds_wo_vert_and_horz_scroll with c; w }; window = values }
      in
      values_window, scroll_values
    in
    let max_cycle_offset = Render.total_cycles_in_waveform waves in
    let max_signal_offset = Render.total_signals_in_waveform waves in
    let waves =
      match waveform with
      | None -> Waves_window.create ~hierarchy ()
      | Some waveform -> waveform.waves_window.window
    in
    let waves_window, scroll_waves =
      let c = signals_width + values_width in
      let w = bounds_wo_vert_scroll.w - c in
      let scroll_waves, _ =
        Scroll.HScrollbar.create_bottom_aligned
          ~container:bounds_wo_vert_scroll
          ~c
          ~w
          ~range:max_cycle_offset
          ()
      in
      let waves_window : Waves_window.t With_bounds.t =
        { bounds = { bounds_wo_vert_and_horz_scroll with c; w }; window = waves }
      in
      waves_window, scroll_waves
    in
    let key_table =
      match waveform with
      | None -> Keys.key_table keys
      | Some { key_table; _ } -> key_table
    in
    let waveform =
      { hierarchy
      ; signals_window
      ; values_window
      ; waves_window
      ; scroll_signals
      ; scroll_values
      ; scroll_waves
      ; scroll_vert
      ; max_signal_offset
      ; max_cycle_offset
      ; ui_state_file
      ; key_table
      }
    in
    Scroll.Scrollbar.on_offset_change scroll_vert ~f:(set_signal_offset waveform);
    Scroll.Scrollbar.on_offset_change scroll_waves ~f:(set_cycle_offset waveform);
    Scroll.Scrollbar.on_offset_change scroll_signals ~f:(set_signal_name_offset waveform);
    Scroll.Scrollbar.on_offset_change scroll_values ~f:(set_value_offset waveform);
    Scroll.Scrollbar.set_offset_max scroll_values;
    waveform
  ;;

  let draw ~ctx (t : t) =
    let cfg = t.hierarchy.cfg in
    let cursors = t.hierarchy.cursors in
    let draw_with_border f ~ctx ~bounds name a =
      f ~ctx ~bounds:(Draw_notty.Border.adjust bounds) a;
      Draw_notty.Border.draw ~ctx ~bounds name
    in
    draw_with_border
      (Signals_window.draw ~selected_wave_index:(Some cfg.selected_signal))
      ~ctx
      ~bounds:t.signals_window.bounds
      "signals"
      t.signals_window.window;
    draw_with_border
      (Values_window.draw ~selected_wave_index:(Some cfg.selected_signal))
      ~ctx
      ~bounds:t.values_window.bounds
      "values"
      t.values_window.window;
    let cursor_sel = Cursors.get_selected_as_string cursors in
    let cursor_cycle = Cursors.get_offset_as_string cursors in
    let cursor_deltas =
      match Cursors.get_offset cursors with
      | None -> "-"
      | Some base_offset ->
        Cursors.with_selected cursors ~f:(fun selected ->
          let offsets =
            List.init Cursors.num_cursors ~f:(fun idx ->
              if idx = selected
              then None
              else (
                Cursors.set_selected cursors idx;
                Option.map (Cursors.get_offset cursors) ~f:(fun offset -> idx, offset)))
          in
          List.filter_opt offsets
          |> List.map ~f:(fun (idx, offset) ->
            [%string "%{idx+1#Int}:%{offset-base_offset#Int}"])
          |> String.concat ~sep:",")
    in
    draw_with_border
      (Waves_window.draw ~selected_wave_index:(Some cfg.selected_signal))
      ~ctx
      ~bounds:t.waves_window.bounds
      [%string
        "waves [%{cfg.start_cycle#Int} cursor %{cursor_sel}:%{cursor_cycle} deltas \
         %{cursor_deltas}]"]
      t.waves_window.window;
    Scroll.VScrollbar.draw ~ctx ~style:Style.default t.scroll_vert;
    Scroll.HScrollbar.draw ~ctx ~style:Style.default t.scroll_signals;
    Scroll.HScrollbar.draw ~ctx ~style:Style.default t.scroll_values;
    Scroll.HScrollbar.draw ~ctx ~style:Style.default t.scroll_waves
  ;;

  let key_handler (t : t) key =
    match Hashtbl.find t.key_table (Key_actions.Key.from_notty_encoding key) with
    | Some f -> f t
    | None -> No_action
  ;;

  let last_mouse_button : (Notty.Unescape.button * Notty.Unescape.mods) option ref =
    ref None
  ;;

  let mouse_handler (t : t) ((button, (col, row), mods) as mouse : Notty.Unescape.mouse) =
    let cursors = t.hierarchy.cursors in
    let in_bounds (bounds : Rect.t) = Rect.inside bounds ~row ~col in
    let rendered_waves = Hierarchy.get_currently_rendered_waves t.hierarchy in
    (* Used during dragging to set the cursor pos without changing the selected signal *)
    let update_cursor button =
      match button with
      | Some (`Left, []) ->
        (match
           Render.pick
             ~bounds:
               { waves = Draw_notty.Border.adjust t.waves_window.bounds
               ; values = Rect.empty
               ; signals = Rect.empty
               ; status = Rect.empty
               }
             ~r:row
             ~c:col
             rendered_waves
         with
         | Wave { cycle; signal_index = _ } ->
           Cursors.set_offset cursors (Some cycle);
           true
         | _ -> false)
      | _ -> false
    in
    let update_buttons button =
      match button with
      | Some (`Left, []) ->
        (match
           Render.pick
             ~bounds:
               { waves = Draw_notty.Border.adjust t.waves_window.bounds
               ; values = Draw_notty.Border.adjust t.values_window.bounds
               ; signals = Draw_notty.Border.adjust t.signals_window.bounds
               ; status = Rect.empty
               }
             ~r:row
             ~c:col
             rendered_waves
         with
         | Signal { signal_index } ->
           (match signal_index with
            | None -> false
            | Some signal_index ->
              let actual_wave = Hierarchy.find_actual_wave t.hierarchy signal_index in
              (match rendered_waves.waves.(signal_index) with
               | Empty _ ->
                 (* An Empty node represents a hierarchy module. Toggle it open or closed. *)
                 t.hierarchy.cfg.selected_signal <- signal_index;
                 let name = Wave.get_name actual_wave in
                 Hierarchy.toggle_module t.hierarchy name;
                 true
               | Clock _ -> false
               | _ ->
                 (* Select the highlighted signal *)
                 t.hierarchy.cfg.selected_signal <- signal_index;
                 true))
         | Value { signal_index } ->
           (* Select the highlighted signal *)
           Option.iter signal_index ~f:(fun signal_index ->
             t.hierarchy.cfg.selected_signal <- signal_index);
           true
         | Wave { cycle; signal_index } ->
           (* Select the highlighted signal and place the cursor *)
           Option.iter signal_index ~f:(fun signal_index ->
             t.hierarchy.cfg.selected_signal <- signal_index);
           Cursors.set_offset cursors (Some cycle);
           true
         | Status | No_pick -> false)
      | _ -> false
    in
    let update_mouse_button_scroll button =
      match button with
      | Some (`Scroll `Up, []) ->
        set_signal_offset t (get_signal_offset t - 1);
        true
      | Some (`Scroll `Down, []) ->
        set_signal_offset t (get_signal_offset t + 1);
        true
      | Some (`Scroll `Up, [ `Ctrl ]) ->
        set_cycle_offset t (get_cycle_offset t - scroll_step t);
        true
      | Some (`Scroll `Down, [ `Ctrl ]) ->
        set_cycle_offset t (get_cycle_offset t + scroll_step t);
        true
      | _ -> false
    in
    let update_scroll_bar (scroll : Scroll.Scrollbar.t) _ =
      in_bounds (Scroll.Scrollbar.bounds scroll)
      && Scroll.Scrollbar.mouse_event scroll mouse
    in
    let or_no_short_circuit a b = a || b in
    match button with
    | `Press b ->
      last_mouse_button := Some (b, mods);
      List.fold_left
        [ update_buttons
        ; update_mouse_button_scroll
        ; update_scroll_bar t.scroll_vert
        ; update_scroll_bar t.scroll_waves
        ; update_scroll_bar t.scroll_signals
        ; update_scroll_bar t.scroll_values
        ]
        ~init:false
        ~f:(fun acc f -> or_no_short_circuit acc (f !last_mouse_button))
    | `Release ->
      let button = !last_mouse_button in
      last_mouse_button := None;
      update_cursor button
    | `Drag ->
      List.fold_left
        [ update_cursor
        ; update_scroll_bar t.scroll_vert
        ; update_scroll_bar t.scroll_waves
        ; update_scroll_bar t.scroll_signals
        ; update_scroll_bar t.scroll_values
        ]
        ~init:false
        ~f:(fun acc f -> or_no_short_circuit acc (f !last_mouse_button))
  ;;

  (* return true to redraw *)
  let handler (t : t) event =
    match event with
    | `Mouse mouse ->
      if mouse_handler t mouse then Event_response.Redraw_window else No_action
    | `Key key -> key_handler t key
  ;;
end
