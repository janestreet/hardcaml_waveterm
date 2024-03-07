module Draw_notty = Draw_notty
module Scroll = Scroll
module Widget = Widget
include Widget.Make (Hardcaml_waveterm_cyclesim.Data) (Hardcaml_waveterm_cyclesim)
