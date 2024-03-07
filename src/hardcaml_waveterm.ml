(** {!Hardcaml_waveterm} is a library for displaying terminal-based waveforms from
    hardcaml simulations.
*)

include struct
  open Hardcaml_waveterm_kernel
  module Display_rule = Display_rule
  module Display_rules = Display_rules
  module Text_alignment = Text_alignment
  module Wave_format = Wave_format
  module Port = Port
  module Port_name = Port_name
end

module Expert = struct
  module M (Data : Hardcaml_waveterm_kernel.Expert.Data.S) = struct
    module type S = sig
      module Wave : Hardcaml_waveterm_kernel.Expert.Wave.M(Data).S
      module Waves : Hardcaml_waveterm_kernel.Expert.Waves.M(Data)(Wave).S
      module Render : Hardcaml_waveterm_kernel.Expert.Render.M(Data)(Wave)(Waves).S

      module Waveform : sig
        include Hardcaml_waveterm_kernel.Expert.Waveform.M(Data)(Wave)(Waves)(Render).S

        val expect : (?show_digest:bool -> ?serialize_to:string -> t -> unit) with_options
      end

      module Serialize : Serialize_waveform.M(Data)(Wave)(Waves)(Render)(Waveform).S
      module Expect : Expect.M(Data)(Wave)(Waves)(Render)(Waveform).S
    end
  end

  module Make (Data : Hardcaml_waveterm_kernel.Expert.Data.S) : M(Data).S = struct
    module Modules = Hardcaml_waveterm_kernel.Expert.Make (Data)
    module Wave = Modules.Wave
    module Waves = Modules.Waves
    module Render = Modules.Render

    module Serialize =
      Serialize_waveform.Make (Data) (Wave) (Waves) (Render) (Modules.Waveform)

    module Expect = Expect.Make (Data) (Wave) (Waves) (Render) (Modules.Waveform)

    module Waveform = struct
      include Modules.Waveform

      let expect = Expect.expect
    end
  end
end

module For_cyclesim = struct
  include Hardcaml_waveterm_cyclesim
  module Data = Data
  module Serialize = Serialize_waveform.Make (Data) (Wave) (Waves) (Render) (Waveform)
  module Expect = Expect.Make (Data) (Wave) (Waves) (Render) (Waveform)

  module Waveform = struct
    include Waveform
    module Serialize = Serialize

    let expect = Expect.expect
  end
end

include struct
  open For_cyclesim
  module Data = Data
  module Serialize = Serialize
  module Waveform = Waveform
end
