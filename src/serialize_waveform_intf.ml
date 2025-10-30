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
    val marshall_here : ?here:Stdlib.Lexing.position -> Waveform.t -> unit
    val unmarshall : string -> Waveform.t

    (** Write to VCD using the "legacy" VCD conversion method of running the waveform
        through a fake cyclesim instance *)
    val marshall_vcd : Waveform.t -> string -> unit

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
