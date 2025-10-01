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

type t =
  { bold : bool
  ; fg : colour
  ; bg : colour
  }
[@@deriving sexp_of, compare ~localize, equal ~localize]

let default = { bold = false; fg = White; bg = Black }
