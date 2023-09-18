open Base
open Hardcaml_waveterm
open Hardcaml_waveterm.Expert

val run_waves : ?ui_state_file:string -> Waves.t -> unit Async.Deferred.t

(** Run the waveform viewer and return.  Calls into Async/LWT so MUST NOT BE CALLED from
    within an Async or LWT deferred. *)
val run : ?ui_state_file:string -> Waves.t -> unit

(** Run the waveform viewer and close the program on exit. *)
val run_and_close : ?ui_state_file:string -> Waves.t -> unit

(** Run interactive waveterm viewer. [ESC] to quit. *)
val run_interactive_viewer
  :  ?ui_state_file:string
  -> ?signals_width:int
  -> ?values_width:int
  -> ?start_cycle:int
  -> ?wave_width:int
  -> ?display_rules:Display_rules.t
  -> Waveform.t
  -> unit
