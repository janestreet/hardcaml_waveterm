open! Base

type t =
  | Left
  | Right
[@@deriving sexp_of, equal]
