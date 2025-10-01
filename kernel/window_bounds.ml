open Base

type t =
  { signals : Rect.t
  ; values : Rect.t
  ; waves : Rect.t
  ; status : Rect.t
  }

let expand_for_border (x : Rect.t) =
  if x.w <> 0 && x.h <> 0
  then { Rect.r = x.r - 1; c = x.c - 1; w = x.w + 2; h = x.h + 2 }
  else x
;;

let shrink_for_border (x : Rect.t) =
  if x.w <> 0 && x.h <> 0
  then { Rect.r = x.r + 1; c = x.c + 1; w = max 0 (x.w - 2); h = max 0 (x.h - 2) }
  else x
;;

let fit_to_window
  ?(signals = true)
  ?(values = true)
  ?(waves = true)
  ?(status = false)
  ?(border = true)
  ?signals_width
  (bounds : Rect.t)
  =
  let rows, cols = bounds.h, bounds.w in
  let minb = if border then 3 else 1 in
  let iw6 = max minb (min 20 (cols / 6)) in
  (* approx 1/6 of width, >minb and < 20 *)
  let iw4 = max minb (min 20 (cols / 4)) in
  (* approx 1/4 of width, >minb and < 20 *)
  let z = { Rect.r = 0; c = 0; w = 0; h = (rows - if status then 3 else 0) } in
  let signals_width default =
    match signals_width with
    | None -> default
    | Some w -> w
  in
  let get_bounds w0 w1 w2 =
    if w2 <= 0 && waves
    then failwith "windows wont fit (sorry, should be more graceful!)"
    else (
      let border (x : Rect.t) =
        if border && x.w <> 0 && x.h <> 0 then shrink_for_border x else x
      in
      { signals = border { z with w = w0 }
      ; values = border { z with c = w0; w = w1 }
      ; waves = border { z with c = w0 + w1; w = w2 }
      ; status =
          (if status then border { r = bounds.h - 3; c = 0; h = 3; w = bounds.w } else z)
      })
  in
  match signals, values, waves with
  (* all *)
  | true, true, true ->
    let signals_width = signals_width iw6 in
    get_bounds signals_width iw6 (cols - signals_width - iw6)
  (* 2 *)
  | true, true, false ->
    let signals_width = signals_width (cols / 2) in
    get_bounds signals_width (cols - signals_width) 0
  | true, false, true ->
    let signals_width = signals_width iw4 in
    get_bounds signals_width 0 (cols - signals_width)
  | false, true, true -> get_bounds 0 iw4 (cols - iw4)
  (* 1 *)
  | true, false, false -> get_bounds (signals_width cols) 0 0
  | false, true, false -> get_bounds 0 cols 0
  | false, false, true -> get_bounds 0 0 cols
  (* 0 *)
  | false, false, false -> get_bounds 0 0 0
;;
