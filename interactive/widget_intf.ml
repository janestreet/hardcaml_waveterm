open Base
open Hardcaml_waveterm

module type S = sig
  type wave

  val run_async
    :  ?ui_state_file:string
    -> ?signals_width:int
    -> ?values_width:int
    -> ?start_cycle:int
    -> ?wave_width:int
    -> ?display_rules:Display_rules.t
    -> wave
    -> unit Async.Deferred.t

  (** Run interactive waveterm viewer. [ESC] to quit. *)
  val run
    :  ?ui_state_file:string
    -> ?signals_width:int
    -> ?values_width:int
    -> ?start_cycle:int
    -> ?wave_width:int
    -> ?display_rules:Display_rules.t
    -> wave
    -> unit
end

module M (Data : Hardcaml.Wave_data.S) = struct
  module type S = S with type wave := Data.t Hardcaml.Wave_data.Wave.t array
end

module type Widget = sig
  module type S = S

  module M = M
  module Make (Data : Hardcaml.Wave_data.S) : M(Data).S
  module By_cycle : M(Hardcaml.Wave_data_in_cycles).S
  module By_event : M(Hardcaml.Wave_data_in_events.Bits).S
  include S with type wave := Hardcaml.Wave_data.t
end
