open Core
open Key_handlers_intf

module Make_common (X : sig
    type t

    val quit : t -> Event_response.t
    val scroll_vertical : t -> by:int -> Event_response.t
  end) =
struct
  open X

  let key_functions =
    { Key_actions.Common.quit
    ; scroll_up = scroll_vertical ~by:(-1)
    ; scroll_down = scroll_vertical ~by:1
    ; scroll_up_fast = scroll_vertical ~by:(-10)
    ; scroll_down_fast = scroll_vertical ~by:10
    }
  ;;
end

module Waveform_window = struct
  module Make
      (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
      (Modl : Hardcaml_waveterm_kernel.Expert.M(Data).S)
      (Hierarchy : Hierarchy.M(Data)(Modl).S)
      (Ui_state : Ui_state.M(Data)(Modl)(Hierarchy).S)
      (X : Waveform_window_api
           with type waves_config := Modl.Waves.Config.t
            and type hierarchy := Hierarchy.t) =
  struct
    include X
    open Event_response

    let show_help _ = Show_help

    let cursor_centered_zoom t ~new_wave_width =
      let cfg = cfg t in
      let new_wave_width =
        let new_wave_width = Int.min new_wave_width 1_000 in
        let new_wave_width = Int.max new_wave_width (-1_000_000) in
        new_wave_width
      in
      (* Set the new wave width *)
      cfg.wave_width <- new_wave_width;
      (* Center on cursor, if set *)
      Option.iter
        (Cursors.get_offset (cursors t))
        ~f:(fun cursor_offset ->
          set_cycle_offset t (cursor_offset - (cycles_displayed t / 2)))
    ;;

    let cursor_centered_zoom_out t =
      cursor_centered_zoom t ~new_wave_width:((cfg t).wave_width - 1);
      Redraw_window
    ;;

    let cursor_centered_zoom_in t =
      cursor_centered_zoom t ~new_wave_width:((cfg t).wave_width + 1);
      Redraw_window
    ;;

    let cursor_centered_zoom_out_fast t =
      let cfg = cfg t in
      let new_wave_width =
        if cfg.wave_width = 0
        then -1
        else if cfg.wave_width < 0
        then cfg.wave_width * 2
        else cfg.wave_width / 2
      in
      cursor_centered_zoom t ~new_wave_width;
      Redraw_window
    ;;

    let cursor_centered_zoom_in_fast t =
      let cfg = cfg t in
      let new_wave_width =
        if cfg.wave_width = 0
        then 1
        else if cfg.wave_width > 0
        then cfg.wave_width * 2
        else cfg.wave_width / 2
      in
      cursor_centered_zoom t ~new_wave_width;
      Redraw_window
    ;;

    let expand_signals_window t =
      let cfg = cfg t in
      cfg.signals_width <- cfg.signals_width + 1;
      Recreate_window
    ;;

    let shrink_signals_window t =
      let cfg = cfg t in
      cfg.signals_width <- max 2 (cfg.signals_width - 1);
      Recreate_window
    ;;

    let expand_values_window t =
      let cfg = cfg t in
      cfg.values_width <- cfg.values_width + 1;
      Recreate_window
    ;;

    let shrink_values_window t =
      let cfg = cfg t in
      cfg.values_width <- max 2 (cfg.values_width - 1);
      Recreate_window
    ;;

    let scroll_wave_to_beginning t =
      set_cycle_offset t 0;
      Redraw_window
    ;;

    let scroll_wave_to_end t =
      set_cycle_offset t (max_cycle_offset t);
      Redraw_window
    ;;

    let scroll_to_first_signal t =
      set_signal_offset t 0;
      Redraw_window
    ;;

    let scroll_to_last_signal t =
      set_signal_offset t (max_signal_offset t);
      Redraw_window
    ;;

    let scroll_horizontal t ~by =
      let by = by * scroll_step t in
      set_cycle_offset t (get_cycle_offset t + by);
      Redraw_window
    ;;

    let move_cursor_vertical t ~by =
      match max_view t with
      | None ->
        (* this shouldn't happen. *)
        No_action
      | Some (_max_cycle, max_signal) ->
        let selected_signal = change_selected_signal_index ~by t in
        let signal_offset = get_signal_offset t in
        if selected_signal < signal_offset
        then set_signal_offset t selected_signal
        else
          Option.iter max_signal ~f:(fun max_signal ->
            if selected_signal > max_signal - 1
            then (
              (* iterate down 1 signal at a time until the selected signal is in view.

               Why is this difficult? Because the height of each signal differs. Another
               approach would be to iterate backwards from the selected signal and count
               the height relative to the window bounds. *)
              let rec find_in_view () =
                let signal_offset = get_signal_offset t + 1 in
                set_signal_offset t signal_offset;
                match max_view t with
                | None -> ()
                | Some (_max_cycle, max_signal) ->
                  (match max_signal with
                   | None -> ()
                   | Some max_signal ->
                     if max_signal >= selected_signal + 1 then () else find_in_view ())
              in
              find_in_view ()));
        Redraw_window
    ;;

    let move_cursor_horizontal t ~by =
      let by = by * scroll_step t in
      match max_view t with
      | None -> No_action
      | Some (max_cycle, _max_signal) ->
        (match get_cursor_offset t with
         | None ->
           (* No cursor - scroll screen *)
           set_cycle_offset t (get_cycle_offset t + by)
         | Some cursor_offset ->
           (* move cursor and update scroll if needed *)
           set_cursor_offset t (cursor_offset + by);
           Option.iter (get_cursor_offset t) ~f:(fun cursor_offset ->
             if cursor_offset < get_cycle_offset t
             then set_cycle_offset t cursor_offset
             else if cursor_offset > max_cycle
             then (
               let diff = cursor_offset - max_cycle in
               set_cycle_offset t (get_cycle_offset t + diff))));
        Redraw_window
    ;;

    let center_on_cursor t =
      Option.iter (get_cursor_offset t) ~f:(fun cursor_offset ->
        set_cycle_offset t (cursor_offset - (cycles_displayed t / 2)));
      Redraw_window
    ;;

    let set_cursor_to_first_wave_on_screen t =
      set_cursor_offset t (get_cycle_offset t);
      Redraw_window
    ;;

    let set_selected_cursor t ~index =
      Cursors.set_selected (cursors t) index;
      Redraw_window
    ;;

    let set_selected_cursor_and_center t ~index =
      Cursors.set_selected (cursors t) index;
      center_on_cursor t
    ;;

    let remove_selected_cursor t =
      Cursors.set_offset (cursors t) None;
      Redraw_window
    ;;

    let maybe_toggle_module t =
      if Hierarchy.toggle_selected_module_if_present (hierarchy t)
      then Redraw_window
      else No_action
    ;;

    let save_ui_state t =
      Core.Out_channel.write_all
        (ui_state_file t)
        ~data:
          (Ui_state.of_hierarchy (hierarchy t)
           |> [%sexp_of: Ui_state.t]
           |> Sexp.to_string_hum);
      Redraw_window
    ;;

    let load_ui_state t =
      match
        Option.try_with (fun () ->
          Sexplib.Sexp.load_sexp (ui_state_file t) |> [%of_sexp: Ui_state.t])
      with
      | Some state ->
        Ui_state.apply_to state (hierarchy t);
        Recreate_window
      | None -> No_action
    ;;

    let search_for_transition t ~direction:search_forwards_or_backwards =
      let cursor = cursors t in
      let wave_cursor =
        match Cursors.get_offset cursor with
        | Some offset -> Int.max 0 offset
        | None -> 0
      in
      let new_cycle_offset =
        Hierarchy.move_to_delta_on_active_node
          ~start_cycle:wave_cursor
          ~search_forwards_or_backwards
          (hierarchy t)
      in
      match new_cycle_offset with
      | None -> No_action
      | Some new_cycle_offset ->
        let distance = new_cycle_offset - wave_cursor in
        Cursors.set_offset cursor (Some new_cycle_offset);
        (* If the wave we are trying to select is going to be out of bounds, we move the
           window just enough so we can see it. *)
        set_cycle_offset t (get_cycle_offset t + distance);
        Redraw_window
    ;;

    let cycle_wave_format t =
      Hierarchy.cycle_wave_format (hierarchy t);
      Redraw_window
    ;;

    let reset_wave_format t =
      Hierarchy.reset_wave_format (hierarchy t);
      Redraw_window
    ;;

    let cycle_colour t =
      Hierarchy.cycle_colour (hierarchy t);
      Redraw_window
    ;;

    let toggle_bold t =
      Hierarchy.toggle_bold (hierarchy t);
      Redraw_window
    ;;

    module Common = Make_common (struct
        type nonrec t = X.t

        let quit _ = Escape

        let scroll_vertical t ~by =
          set_signal_offset t (get_signal_offset t + by);
          Redraw_window
        ;;
      end)

    let key_functions =
      { Key_actions.common = Common.key_functions
      ; waveform_window =
          { show_help
          ; cursor_centered_zoom_in
          ; cursor_centered_zoom_out
          ; cursor_centered_zoom_in_fast
          ; cursor_centered_zoom_out_fast
          ; expand_signals_window
          ; shrink_signals_window
          ; expand_values_window
          ; shrink_values_window
          ; scroll_wave_to_beginning
          ; scroll_wave_to_end
          ; scroll_to_first_signal
          ; scroll_to_last_signal
          ; scroll_left = scroll_horizontal ~by:(-1)
          ; scroll_right = scroll_horizontal ~by:1
          ; scroll_left_fast = scroll_horizontal ~by:(-10)
          ; scroll_right_fast = scroll_horizontal ~by:10
          ; move_cursor_up = move_cursor_vertical ~by:(-1)
          ; move_cursor_down = move_cursor_vertical ~by:1
          ; move_cursor_left = move_cursor_horizontal ~by:(-1)
          ; move_cursor_right = move_cursor_horizontal ~by:1
          ; move_cursor_up_fast = move_cursor_vertical ~by:(-10)
          ; move_cursor_down_fast = move_cursor_vertical ~by:10
          ; move_cursor_left_fast = move_cursor_horizontal ~by:(-10)
          ; move_cursor_right_fast = move_cursor_horizontal ~by:10
          ; center_on_cursor
          ; set_cursor_to_first_wave_on_screen
          ; select_cursor_1 = set_selected_cursor ~index:0
          ; select_cursor_2 = set_selected_cursor ~index:1
          ; select_cursor_3 = set_selected_cursor ~index:2
          ; select_cursor_4 = set_selected_cursor ~index:3
          ; select_cursor_5 = set_selected_cursor ~index:4
          ; select_cursor_1_and_center = set_selected_cursor_and_center ~index:0
          ; select_cursor_2_and_center = set_selected_cursor_and_center ~index:1
          ; select_cursor_3_and_center = set_selected_cursor_and_center ~index:2
          ; select_cursor_4_and_center = set_selected_cursor_and_center ~index:3
          ; select_cursor_5_and_center = set_selected_cursor_and_center ~index:4
          ; remove_selected_cursor
          ; maybe_toggle_module
          ; save_ui_state
          ; load_ui_state
          ; search_for_transition_forward = search_for_transition ~direction:`Forwards
          ; search_for_transition_backward = search_for_transition ~direction:`Backwards
          ; cycle_wave_format
          ; reset_wave_format
          ; cycle_colour
          ; toggle_bold
          }
      }
    ;;

    let key_table keys =
      Hashtbl.of_alist_exn
        (module Key_actions.Key)
        (Key_actions.zip keys key_functions |> Key_actions.to_list)
    ;;
  end
end

module Help_window = struct
  module Make (X : Help_api) = struct
    include X
    open Event_response

    module Common = Make_common (struct
        type nonrec t = X.t

        let quit _ = Show_waveform

        let scroll_vertical t ~by =
          scroll_vertical t ~by;
          Redraw_window
        ;;
      end)

    let key_table keys =
      Hashtbl.of_alist_exn
        (module Key_actions.Key)
        (Key_actions.Common.zip keys Common.key_functions |> Key_actions.Common.to_list)
    ;;
  end
end
