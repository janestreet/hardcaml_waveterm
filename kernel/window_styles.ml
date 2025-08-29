open Base

type t =
  { border : Style.t option
  ; signals : Style.t
  ; values : Style.t
  ; waves : Style.t
  ; status : Style.t
  }

let default d = { border = Some d; signals = d; values = d; waves = d; status = d }
let white_on_black = default Style.{ default with fg = White; bg = Black }
let black_on_white = default Style.{ default with fg = Black; bg = White }

let colour s =
  { s with
    signals = Style.{ s.signals with fg = Blue }
  ; values = Style.{ s.values with fg = Red }
  ; waves = Style.{ s.waves with fg = Green }
  ; status = Style.{ s.waves with fg = Magenta }
  }
;;

let colour_on_black = colour white_on_black
let colour_on_white = colour black_on_white
