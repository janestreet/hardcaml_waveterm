open Base

module Common : sig
  type 'a t =
    { quit : 'a
    ; scroll_up : 'a
    ; scroll_down : 'a
    ; scroll_up_fast : 'a
    ; scroll_down_fast : 'a
    }
  [@@deriving hardcaml, sexp_of]
end

module Waveform_window : sig
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

(* Effectively a copy of the notty encoding. *)
module Key : sig
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
  [@@deriving compare, equal, hash, sexp_of]

  type key =
    [ special
    | `Uchar of Uchar.t
    | `ASCII of char
    ]
  [@@deriving compare, equal, hash, sexp_of]

  type mods =
    [ `Meta
    | `Ctrl
    | `Shift
    ]
  [@@deriving compare, equal, hash, sexp_of]

  type t = key * mods option [@@deriving compare, equal, hash, sexp_of, to_string]

  val from_notty_encoding : key * mods list -> t
end

val default_keys : Key.t t

(** Heading key belongs to and key specific description *)
val key_help : (string * string) t

module File_format : sig
  type nonrec t = Key.t t [@@deriving sexp]
end
