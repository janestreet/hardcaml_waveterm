open Base
open Hardcaml_waveterm_kernel

module Make
  (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
  (M : Hardcaml_waveterm_kernel.Expert.M(Data).S) : sig
  val run_waves : ?ui_state_file:string -> M.Waves.t -> unit Async.Deferred.t

  (** Run the waveform viewer and return.  Calls into Async/LWT so MUST NOT BE CALLED from
      within an Async or LWT deferred. *)
  val run_and_return : ?ui_state_file:string -> M.Waves.t -> unit

  (** Run the waveform viewer and close the program on exit. *)
  val run_and_close : ?ui_state_file:string -> M.Waves.t -> unit

  (** Run interactive waveterm viewer. [ESC] to quit. *)
  val run
    :  ?ui_state_file:string
    -> ?signals_width:int
    -> ?values_width:int
    -> ?start_cycle:int
    -> ?wave_width:int
    -> ?display_rules:Display_rules.t
    -> M.Waveform.t
    -> unit
end
