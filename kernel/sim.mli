(** Module for connecting the waveform tracer to a hardcaml simulator. *)

open Hardcaml
module Wave : Wave.M(Data).S
module Waves : Waves.M(Data)(Wave).S
module Render : Render.M(Data)(Wave)(Waves).S
module Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S

(** Wraps a hardcaml simulator with a waveform tracer. This returns a simulator
    object with waveform traced. Both simulators are interchangable (ie: calling
    [Sim.cycle] on one cycles the other as well, but only the simulator
    returned from the function traces waveforms.
*)
val wrap : ('i, 'o) Hardcaml.Cyclesim.t -> ('i, 'o) Hardcaml.Cyclesim.t * Wave.t array

val create : ('i, 'o) Cyclesim.t -> Waveform.t * ('i, 'o) Cyclesim.t
