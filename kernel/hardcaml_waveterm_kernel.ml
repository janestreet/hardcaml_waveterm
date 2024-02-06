(** {!Hardcaml_waveterm_kernel} is a library for displaying terminal-based waveforms from
    hardcaml simulations.

    The kernel library avoids any unix dependency so can be compiled for javascript.
*)

module Make (Data : Data.Readable) = struct
  module Wave = Wave.Make (Data)
  module Waves = Waves.Make (Data) (Wave)
  module Render = Render.Make (Data) (Wave) (Waves)
  module Waveform = Waveform.Make (Data) (Wave) (Waves) (Render)
end

module Display_rule = Display_rule
module Display_rules = Display_rules
module Text_alignment = Text_alignment
module Wave_format = Hardcaml.Wave_format

module Waveform = struct
  include Sim.Waveform

  let create = Sim.create
end

module Expert = struct
  module Data = Data
  module Draw = Draw
  module Port = Port
  module Port_name = Port_name
  module Render = Sim.Render
  module Sim = Sim
  module Wave = Sim.Wave
  module Waves = Sim.Waves
  module Write = Write
end
