(** Module for connecting the waveform tracer to a hardcaml simulator. *)

module Data = Hardcaml.Wave_data
include Hardcaml_waveterm_kernel.Expert.M(Data).S

module Waveform : sig
  include Hardcaml_waveterm_kernel.Expert.Waveform.M(Data)(Wave)(Waves)(Render).S

  val create : ('i, 'o) Hardcaml.Cyclesim.t -> t * ('i, 'o) Hardcaml.Cyclesim.t
end

module Expert : sig
  (** Wraps a hardcaml simulator with a waveform tracer. This returns a simulator
      object with waveform traced. Both simulators are interchangable (ie: calling
      [Sim.cycle] on one cycles the other as well, but only the simulator
      returned from the function traces waveforms.
  *)
  val wrap : ('i, 'o) Hardcaml.Cyclesim.t -> ('i, 'o) Hardcaml.Cyclesim.t * Wave.t array
end
