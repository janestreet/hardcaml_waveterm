(** {!Hardcaml_waveterm} is a library for displaying terminal-based waveforms from
    hardcaml simulations.
*)

include struct
  open Hardcaml_waveterm_kernel
  module Display_rule = Display_rule
  module Display_rules = Display_rules
  module Text_alignment = Text_alignment
  module Wave_format = Wave_format

  module Waveform = struct
    include Waveform

    module Serialize = struct
      let marshall = Serialize_waveform.marshall
      let unmarshall = Serialize_waveform.unmarshall
    end

    let expect = Serialize_waveform.expect
  end

  module Expert = struct
    open Expert
    module Data = Data
    module Draw = Draw
    module Port = Port
    module Port_name = Port_name
    module Render = Render
    module Sim = Sim
    module Wave = Wave
    module Waves = Waves
    module Write = Write
  end
end
