open Base

type t =
  { signals : Rect.t
  ; values : Rect.t
  ; waves : Rect.t
  ; status : Rect.t
  }

val expand_for_border : Rect.t -> Rect.t
val shrink_for_border : Rect.t -> Rect.t

val fit_to_window
  :  ?signals:bool
  -> ?values:bool
  -> ?waves:bool
  -> ?status:bool
  -> ?border:bool
  -> ?signals_width:int
  -> Rect.t
  -> t
