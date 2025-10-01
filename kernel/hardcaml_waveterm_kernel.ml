(** {!Hardcaml_waveterm_kernel} is a library for displaying terminal-based waveforms from
    hardcaml simulations.

    The kernel library avoids any unix dependency so can be compiled for javascript. *)

module Display_rule = Display_rule
module Display_rules = Display_rules
module Draw = Draw
module Key_help = Key_help
module Text_alignment = Text_alignment
module Wave_format = Hardcaml.Wave_format
module Port = Port
module Port_name = Port_name
module Rect = Rect
module Style = Style
module Window_bounds = Window_bounds
module Window_styles = Window_styles

module Expert = struct
  module M (Data : Data.S) = struct
    module type S = sig
      module Wave : Wave.M(Data).S
      module Waves : Waves.M(Data)(Wave).S
      module Render : Render.M(Data)(Wave)(Waves).S
      module Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S
    end
  end

  module Make (Data : Data.S) : M(Data).S = struct
    module Wave = Wave.Make (Data)
    module Waves = Waves.Make (Data) (Wave)
    module Render = Render.Make (Data) (Wave) (Waves)
    module Waveform = Waveform.Make (Data) (Wave) (Waves) (Render)
  end

  module Data = Data
  module Write = Write
  module Wave = Wave
  module Waves = Waves
  module Render = Render
  module Waveform = Waveform
end
