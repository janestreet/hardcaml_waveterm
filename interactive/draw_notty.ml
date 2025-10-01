open Base
open Hardcaml_waveterm_kernel
include Draw

let to_colour = function
  | Style.Black -> Notty.A.black
  | Red -> Notty.A.red
  | Green -> Notty.A.green
  | Yellow -> Notty.A.yellow
  | Blue -> Notty.A.blue
  | Magenta -> Notty.A.magenta
  | Cyan -> Notty.A.cyan
  | White -> Notty.A.white
;;

let styling (st : Style.t) =
  let bg' = to_colour st.bg in
  let fg' = to_colour st.fg in
  if st.bold then Notty.A.(bg bg' ++ fg fg' ++ st bold) else Notty.A.(bg bg' ++ fg fg')
;;

let to_image ctx =
  let cols, rows = cols ctx, rows ctx in
  let bounds = { Rect.r = 0; c = 0; w = cols; h = rows } in
  Notty.I.tabulate cols rows (fun c r ->
    let p, st = get ~ctx ~bounds ~r ~c in
    Notty.I.uchar (styling st) (Uchar.of_scalar_exn p) 1 1)
;;

module Border = struct
  let adjust (x : Rect.t) = { Rect.r = x.r + 1; c = x.c + 1; w = x.w - 2; h = x.h - 2 }
  let draw ~ctx ~bounds label = draw_box ~ctx ~bounds ~style:Style.default label
end
