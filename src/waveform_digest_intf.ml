open Base
open Hardcaml_waveterm_kernel.Expert

module M
  (Data : Data.S)
  (Wave : Wave.M(Data).S)
  (Waves : Waves.M(Data)(Wave).S)
  (Render : Render.M(Data)(Wave)(Waves).S)
  (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) =
struct
  module type S = sig
    type t

    val create : Waveform.t -> t
    val to_hex_string : t -> string
  end
end

module type Waveform_digest = sig
  module M = M

  module Make
    (Data : Data.S)
    (Wave : Wave.M(Data).S)
    (Waves : Waves.M(Data)(Wave).S)
    (Render : Render.M(Data)(Wave)(Waves).S)
    (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) :
    M(Data)(Wave)(Waves)(Render)(Waveform).S
end
