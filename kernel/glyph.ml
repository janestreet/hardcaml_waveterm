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
  | Collapsed_triangle
  | Expanded_triangle
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
[@@deriving sexp_of, equal]

let to_unicode = function
  | Top_left -> 0x250c
  | Bottom_right -> 0x2518
  | Bottom_left -> 0x2514
  | Top_right -> 0x2510
  | Vertical -> 0x2502
  | Horizontal -> 0x2500
  | T_down -> 0x252c
  | T_up -> 0x2534
  (* Historical waveterm rendering uses double bars rather than solid blocks. *)
  | Full -> 0x2551
  | Top_half -> 0x2568
  | Bottom_half -> 0x2565
  | Collapsed_triangle -> 0x25b8
  | Expanded_triangle -> 0x25be
  | Horizontal_dash -> 0x254d
  | Ellipsis -> 0x2026
  | Cross -> 0x253c
  | Left_half -> 0x258c
  | Right_half -> 0x2590
  | Quarter_top_left -> 0x2598
  | Quarter_bottom_right -> 0x2597
  | Quarter_bottom_left -> 0x2596
  | Quarter_top_right -> 0x259d
  | Up_arrow -> 0x2191
  | Down_arrow -> 0x2193
;;
