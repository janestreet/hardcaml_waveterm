open Base

module Common = struct
  type 'a t =
    { quit : 'a
    ; scroll_up : 'a
    ; scroll_down : 'a
    ; scroll_up_fast : 'a
    ; scroll_down_fast : 'a
    }
  [@@deriving hardcaml, sexp_of]
end

module Waveform_window = struct
  type 'a t =
    { show_help : 'a
    ; cursor_centered_zoom_in : 'a
    ; cursor_centered_zoom_out : 'a
    ; cursor_centered_zoom_in_fast : 'a
    ; cursor_centered_zoom_out_fast : 'a
    ; expand_signals_window : 'a
    ; shrink_signals_window : 'a
    ; expand_values_window : 'a
    ; shrink_values_window : 'a
    ; scroll_wave_to_beginning : 'a
    ; scroll_wave_to_end : 'a
    ; scroll_to_first_signal : 'a
    ; scroll_to_last_signal : 'a
    ; scroll_left : 'a
    ; scroll_right : 'a
    ; scroll_left_fast : 'a
    ; scroll_right_fast : 'a
    ; move_cursor_up : 'a
    ; move_cursor_down : 'a
    ; move_cursor_left : 'a
    ; move_cursor_right : 'a
    ; move_cursor_up_fast : 'a
    ; move_cursor_down_fast : 'a
    ; move_cursor_left_fast : 'a
    ; move_cursor_right_fast : 'a
    ; center_on_cursor : 'a
    ; set_cursor_to_first_wave_on_screen : 'a
    ; select_cursor_1 : 'a
    ; select_cursor_2 : 'a
    ; select_cursor_3 : 'a
    ; select_cursor_4 : 'a
    ; select_cursor_5 : 'a
    ; select_cursor_1_and_center : 'a
    ; select_cursor_2_and_center : 'a
    ; select_cursor_3_and_center : 'a
    ; select_cursor_4_and_center : 'a
    ; select_cursor_5_and_center : 'a
    ; remove_selected_cursor : 'a
    ; maybe_toggle_module : 'a
    ; save_ui_state : 'a
    ; load_ui_state : 'a
    ; search_for_transition_forward : 'a
    ; search_for_transition_backward : 'a
    ; cycle_wave_format : 'a
    ; reset_wave_format : 'a
    ; cycle_colour : 'a
    ; toggle_bold : 'a
    }
  [@@deriving hardcaml, sexp_of]
end

type 'a t =
  { common : 'a Common.t
  ; waveform_window : 'a Waveform_window.t
  }
[@@deriving hardcaml, sexp_of]

module Key = struct
  type special =
    [ `Escape
    | `Enter
    | `Tab
    | `Backspace
    | `Insert
    | `Delete
    | `Home
    | `End
    | `Arrow of [ `Up | `Down | `Left | `Right ]
    | `Page of [ `Up | `Down ]
    | `Function of int
    ]
  [@@deriving compare, equal, hash, sexp]

  type key =
    [ special
    | `Uchar of Uchar.t
    | `ASCII of char
    ]
  [@@deriving compare, equal, hash, sexp]

  type mods =
    [ `Meta
    | `Ctrl
    | `Shift
    ]
  [@@deriving compare, equal, hash, sexp]

  type t = key * mods option [@@deriving compare, equal, hash, sexp]

  let to_string (key, mods) =
    let mods =
      match mods with
      | Some `Meta -> "Alt "
      | Some `Ctrl -> "Ctrl "
      | Some `Shift -> "Shift "
      | None -> ""
    in
    let key =
      match key with
      | `ASCII c -> Char.to_string c
      | `Arrow `Left -> "Left"
      | `Arrow `Right -> "Right"
      | `Arrow `Up -> "Up"
      | `Arrow `Down -> "Down"
      | `Enter -> "Enter"
      | `Tab -> "Tab"
      | `Backspace -> "Backspace"
      | `Function i -> [%string "F%{i#Int}"]
      | `Home -> "Home"
      | `End -> "End"
      | `Page `Up -> "Pageup"
      | `Page `Down -> "Pagedown"
      | `Insert -> "Insert"
      | `Delete -> "Delete"
      | `Escape -> "Escape"
      | `Uchar _ -> "?"
    in
    mods ^ key
  ;;

  let from_notty_encoding (key, mods) =
    let mods =
      (* We only allow 1 modifier key *)
      match mods with
      | [ mods ] -> Some mods
      | _ -> None
    in
    key, mods
  ;;
end

let default_keys =
  { common =
      { quit = `Escape, None
      ; scroll_up = `Arrow `Up, None
      ; scroll_down = `Arrow `Down, None
      ; scroll_up_fast = `Arrow `Up, Some `Ctrl
      ; scroll_down_fast = `Arrow `Down, Some `Ctrl
      }
  ; waveform_window =
      { show_help = `ASCII '?', None
      ; cursor_centered_zoom_in = `ASCII '=', None
      ; cursor_centered_zoom_out = `ASCII '-', None
      ; cursor_centered_zoom_in_fast = `ASCII '=', Some `Meta
      ; cursor_centered_zoom_out_fast = `ASCII '-', Some `Meta
      ; expand_signals_window = `ASCII '9', None
      ; shrink_signals_window = `ASCII '(', None
      ; expand_values_window = `ASCII '0', None
      ; shrink_values_window = `ASCII ')', None
      ; scroll_wave_to_beginning = `Home, None
      ; scroll_wave_to_end = `End, None
      ; scroll_to_first_signal = `Home, Some `Ctrl
      ; scroll_to_last_signal = `End, Some `Ctrl
      ; scroll_left = `Arrow `Left, None
      ; scroll_right = `Arrow `Right, None
      ; scroll_left_fast = `Arrow `Left, Some `Ctrl
      ; scroll_right_fast = `Arrow `Right, Some `Ctrl
      ; move_cursor_up = `ASCII 'k', None
      ; move_cursor_down = `ASCII 'j', None
      ; move_cursor_left = `ASCII 'h', None
      ; move_cursor_right = `ASCII 'l', None
      ; move_cursor_up_fast = `ASCII 'K', None
      ; move_cursor_down_fast = `ASCII 'J', None
      ; move_cursor_left_fast = `ASCII 'H', None
      ; move_cursor_right_fast = `ASCII 'L', None
      ; center_on_cursor = `ASCII 'g', None
      ; set_cursor_to_first_wave_on_screen = `ASCII 'G', None
      ; select_cursor_1 = `ASCII '1', None
      ; select_cursor_2 = `ASCII '2', None
      ; select_cursor_3 = `ASCII '3', None
      ; select_cursor_4 = `ASCII '4', None
      ; select_cursor_5 = `ASCII '5', None
      ; select_cursor_1_and_center = `ASCII '1', Some `Meta
      ; select_cursor_2_and_center = `ASCII '2', Some `Meta
      ; select_cursor_3_and_center = `ASCII '3', Some `Meta
      ; select_cursor_4_and_center = `ASCII '4', Some `Meta
      ; select_cursor_5_and_center = `ASCII '5', Some `Meta
      ; remove_selected_cursor = `ASCII 'r', None
      ; maybe_toggle_module = `Enter, None
      ; save_ui_state = `ASCII 's', None
      ; load_ui_state = `ASCII 'S', None
      ; search_for_transition_forward = `ASCII 'e', None
      ; search_for_transition_backward = `ASCII 'b', None
      ; cycle_wave_format = `ASCII 'f', None
      ; reset_wave_format = `ASCII 'F', None
      ; cycle_colour = `ASCII 'c', None
      ; toggle_bold = `ASCII 'C', None
      }
  }
;;

let key_help =
  map port_names ~f:(fun name ->
    match String.split name ~on:'$' with
    | [ group; descr ] ->
      let group = String.tr group ~target:'_' ~replacement:' ' |> String.capitalize in
      let descr = String.tr descr ~target:'_' ~replacement:' ' in
      group, descr
    | _ ->
      raise_s [%message "Unexpected number of levels in struct. Please update this logic"])
;;

module File_format = struct
  type nonrec t = Key.t t

  let sexp_of_t t =
    [%sexp_of: (string * Key.t) list] (Unsafe_assoc_by_port_name.to_alist t)
  ;;

  let t_of_sexp sexp =
    let overrides = [%of_sexp: (string * Key.t) list] sexp in
    map2 port_names default_keys ~f:(fun name default ->
      Option.value ~default (List.Assoc.find overrides ~equal:String.equal name))
  ;;
end
