(** Graphics drawing api to a 2d array of integers.

    Most functions take a [bounds] parameter which is a [Rect.t] to which drawing is
    clipped and also serves as an origin for any coordinates. *)

open Base

type piece =
  (* corners, vert/horz bar, T shapes, cross *)
  | TL
  | BR
  | BL
  | TR
  | V
  | H
  | T
  | Tu
  | C
  (* full/half blocks *)
  | F
  | TH
  | BH
  | LH
  | RH
  (* quarter blocks *)
  | QTL
  | QBR
  | QBL
  | QTR
  (* Arrows *)
  | Up_arrow
  | Down_arrow
[@@deriving sexp_of]

(** unicode value of piece *)
val unicode_of_piece : piece -> int

(** drawing context *)
type ctx = (int * Style.t) array array

val init : rows:int -> cols:int -> ctx
val rows : ctx -> int
val cols : ctx -> int

(** get context size *)
val get_bounds : ctx -> Rect.t

(** clear display *)
val clear : ctx -> unit

(** fill [bounds] with [char] given [style] *)
val fill : ctx:ctx -> style:Style.t -> bounds:Rect.t -> char -> unit

(** draw int (representing unicode value) *)
val draw_int : ctx:ctx -> style:Style.t -> bounds:Rect.t -> r:int -> c:int -> int -> unit

(** draw piece *)
val draw_piece
  :  ctx:ctx
  -> style:Style.t
  -> bounds:Rect.t
  -> r:int
  -> c:int
  -> piece
  -> unit

(** draw char *)
val draw_char
  :  ctx:ctx
  -> style:Style.t
  -> bounds:Rect.t
  -> r:int
  -> c:int
  -> char
  -> unit

(** draw string (nothing fancy - horizontal, no breaks) *)
val draw_string
  :  ctx:ctx
  -> style:Style.t
  -> bounds:Rect.t
  -> r:int
  -> c:int
  -> string
  -> unit

(** draw box outline with label *)
val draw_box : ctx:ctx -> style:Style.t -> bounds:Rect.t -> string -> unit

(** get value and style at point *)
val get : ctx:ctx -> bounds:Rect.t -> r:int -> c:int -> int * Style.t

(** invert fg and bg at point. Or force to given style if provided. *)
val inv : ?style:Style.t -> ctx:ctx -> bounds:Rect.t -> r:int -> c:int -> unit -> unit

(** set bold on point *)
val bold : ctx:ctx -> bounds:Rect.t -> r:int -> c:int -> unit
