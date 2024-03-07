(** {!Hardcaml_waveterm_kernel} is a library for displaying terminal-based waveforms from
    hardcaml simulations.

    The kernel library avoids any unix dependency so can be compiled for javascript.
*)

module Display_rule = Display_rule
module Display_rules = Display_rules
module Text_alignment = Text_alignment
module Wave_format = Hardcaml.Wave_format
module Port = Port
module Port_name = Port_name

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
  module Draw = Draw
  module Write = Write
  module Wave = Wave
  module Waves = Waves
  module Render = Render
  module Waveform = Waveform
end
