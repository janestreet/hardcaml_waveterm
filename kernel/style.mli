(** Styling information *)

open Base

type colour =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
[@@deriving sexp_of, compare ~localize, equal ~localize]

(** foreground/backgound colours and weight (bold) *)
type t =
  { bold : bool
  ; fg : colour
  ; bg : colour
  }
[@@deriving sexp_of, compare ~localize, equal ~localize]

(** white on black, normal weight *)
val default : t
