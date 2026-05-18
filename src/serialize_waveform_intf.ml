open Base

module type S = sig
  val marshall : Hardcaml.Wave_data.t -> string -> unit
  val marshall_here : here:[%call_pos] -> Hardcaml.Wave_data.t -> unit
  val unmarshall : string -> Hardcaml.Wave_data.t

  (** Write to VCD using the "legacy" VCD conversion method of running the waveform
      through a fake cyclesim instance *)
  val marshall_vcd : Hardcaml.Wave_data.t -> string -> unit
end

module type Serialize_waveform = sig
  module type S = S

  include S
end
