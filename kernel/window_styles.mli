open Base

type t =
  { border : Style.t option
  ; signals : Style.t
  ; values : Style.t
  ; waves : Style.t
  ; status : Style.t
  }

val default : Style.t -> t
val black_on_white : t
val white_on_black : t
val colour : t -> t
val colour_on_white : t
val colour_on_black : t
