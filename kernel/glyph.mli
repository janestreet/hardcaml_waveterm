(** The set of box-drawing / block / arrow characters used to render waveforms. Each
    constructor maps to a single unicode scalar via {!to_unicode}. *)

open! Base

type t =
  | Top_left
  | Bottom_right
  | Bottom_left
  | Top_right
  | Vertical
  | Horizontal
  | T_down
  | T_up
  | Full
  | Top_half
  | Bottom_half
  | Collapsed_triangle (** ▸ - a folded group header *)
  | Expanded_triangle (** ▾ - an expanded group header *)
  | Horizontal_dash
  | Ellipsis
  | Cross
  | Left_half
  | Right_half
  | Quarter_top_left
  | Quarter_bottom_right
  | Quarter_bottom_left
  | Quarter_top_right
  | Up_arrow
  | Down_arrow
[@@deriving sexp_of, equal ~localize]

(** Unicode scalar value for the glyph. *)
val to_unicode : t -> int
