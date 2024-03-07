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
    val marshall : Waveform.t -> string -> unit
    val unmarshall : string -> Waveform.t

    (** For testing marshalling functions. *)
    val equal : Waveform.t -> Waveform.t -> bool
  end
end

module type Serialize_waveform = sig
  module M = M

  module Make
    (Data : Data.S)
    (Wave : Wave.M(Data).S)
    (Waves : Waves.M(Data)(Wave).S)
    (Render : Render.M(Data)(Wave)(Waves).S)
    (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) :
    M(Data)(Wave)(Waves)(Render)(Waveform).S
end
