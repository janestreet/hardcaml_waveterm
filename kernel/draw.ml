open Base

type ctx = (int * Style.t) array array

let rows ctx = Array.length ctx

let cols ctx =
  try Array.length ctx.(0) with
  | _ -> 0
;;

let get_bounds ctx = { Rect.r = 0; c = 0; h = rows ctx; w = cols ctx }
let get_style s = s

let draw_int ~ctx ~style ~(bounds : Rect.t) ~r ~c i =
  if r >= 0 && r < bounds.h && c >= 0 && c < bounds.w
  then ctx.(bounds.r + r).(bounds.c + c) <- i, style
;;

let get ~ctx ~(bounds : Rect.t) ~r ~c =
  if r >= 0 && r < bounds.h && c >= 0 && c < bounds.w
  then ctx.(bounds.r + r).(bounds.c + c)
  else raise (Invalid_argument "Gfx.get: out of bounds")
;;

let init ~rows ~cols =
  let ch = Char.to_int ' ' in
  Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> ch, Style.default))
;;

let draw_char ~ctx ~style ~bounds ~r ~c ch =
  draw_int ~ctx ~style ~bounds ~r ~c (Char.to_int ch)
;;

let draw_glyph ~ctx ~style ~bounds ~r ~c glyph =
  draw_int ~ctx ~style ~bounds ~r ~c (Glyph.to_unicode glyph)
;;

let fill ~ctx ~style ~(bounds : Rect.t) ch =
  for r = 0 to bounds.h - 1 do
    for c = 0 to bounds.w - 1 do
      draw_char ~ctx ~style ~bounds ~r ~c ch
    done
  done
;;

let clear ctx =
  let bounds = get_bounds ctx in
  let style = get_style Style.default in
  for r = 0 to bounds.h - 1 do
    for c = 0 to bounds.w - 1 do
      draw_char ~ctx ~style ~bounds ~r ~c ' '
    done
  done
;;

let draw_string ~ctx ~style ~bounds ~r ~c str =
  for i = 0 to String.length str - 1 do
    draw_char ~ctx ~style ~bounds ~r ~c:(c + i) str.[i]
  done
;;

let draw_box ~ctx ~style ~(bounds : Rect.t) label =
  let w, h = bounds.w, bounds.h in
  assert (w >= 2 && h >= 2);
  (* min box size including borders *)
  draw_glyph ~ctx ~style ~bounds ~r:0 ~c:0 Top_left;
  draw_glyph ~ctx ~style ~bounds ~r:(h - 1) ~c:0 Bottom_left;
  draw_glyph ~ctx ~style ~bounds ~r:0 ~c:(w - 1) Top_right;
  draw_glyph ~ctx ~style ~bounds ~r:(h - 1) ~c:(w - 1) Bottom_right;
  for c = 1 to w - 2 do
    draw_glyph ~ctx ~style ~bounds ~r:0 ~c Horizontal
  done;
  for c = 1 to w - 2 do
    draw_glyph ~ctx ~style ~bounds ~r:(h - 1) ~c Horizontal
  done;
  for r = 1 to h - 2 do
    draw_glyph ~ctx ~style ~bounds ~r ~c:0 Vertical
  done;
  for r = 1 to h - 2 do
    draw_glyph ~ctx ~style ~bounds ~r ~c:(w - 1) Vertical
  done;
  draw_string ~ctx ~style ~bounds:{ bounds with w = w - 1 } ~r:0 ~c:1 label
;;

let inv ?style ~ctx ~bounds ~r ~c () =
  try
    let x, s = get ~ctx ~bounds ~r ~c in
    let style =
      get_style
        (match style with
         | None -> Style.{ s with fg = s.bg; bg = s.fg }
         | Some style -> style)
    in
    draw_int ~ctx ~style ~bounds ~r ~c x
  with
  | _ -> ()
;;

let bold ~ctx ~bounds ~r ~c =
  try
    let x, s = get ~ctx ~bounds ~r ~c in
    let style = get_style Style.{ s with bold = true } in
    draw_int ~ctx ~style ~bounds ~r ~c x
  with
  | _ -> ()
;;
