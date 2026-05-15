(** A waveform is a mutable value that represents the outcome of a simulation; it holds a
    time series of values for a number of ports. The waveform is updated by running the
    simulation. *)

open Base
open! Hardcaml

(** Waveform configuration options.

    [display_rules] see [Display_rules]. A waveform may be constructed once and displayed
    multiple times with differing options.

    [wave_width] sets the number of chars used to render each clock cycle.

    - [n>=0] ((n+1)*2) characters per cycle. This ensures that the clock is properly
      rendered when [n=0]. The default of [3] allows up to 7 chars per cycle to be
      rendered to represent data values in the waveform.

    - [n<0] (-n) cycles per character. Characters in the waveform within which multiple
      transitions occur are displayed with a double veritical bar.

    [display_width] and [display_height] set the overall size of the displayed waveform.
    An auto scaling routine assigns approximately [1/3] of the display for the signals and
    values windows. The default size is 70 characters wide with the height automatically
    inferred (upto a max of 256). The minimum size is 7 characters wide and 3 high and an
    exception is raised otherwise.

    [signals_alignment] changes the text alignment of the displayed signals names. By
    default this is Wave_format.Left. *)
type 'a with_options =
  ?display_rules:Display_rules.t
  -> ?display_width:int
  -> ?display_height:int
  -> ?display_values:bool
  -> ?wave_width:int
  -> ?signals_width:int
  -> ?start_cycle:int
  -> ?signals_alignment:Text_alignment.t
  -> 'a

module type S = sig
  type t [@@deriving sexp_of, equal ~localize]

  (** Write waveform into a [Buffer.t]. *)
  val to_buffer : (t -> Buffer.t) with_options

  (** Convert waveform to a string. *)
  val to_string : (t -> string) with_options

  (** Print waveform to [channel] *)
  val print
    : (?channel:Stdio.Out_channel.t (** default is [stdout] *) -> t -> unit) with_options

  (** General function to look up the nth cycle where the set of conditions given are met.
      Searches cycles [pos] (default 0) through [pos + span - 1] (or to the end of the
      waveform if [span] is not specified). [negate] inverts the combined condition. *)
  val look_for_nth_instance_of_condition_in_waveform
    :  ?pos:int
    -> ?span:int
    -> ?negate:bool
    -> n:int
    -> conditions:Wave_condition.t list
    -> t
    -> int option

  (** Convenience wrapper for [look_for_nth_instance_of_condition_in_waveform] when you
      are looking for one condition and matching by wave name *)
  val look_for_first_instance_of_condition_in_wave
    :  f:(Bits.t -> bool)
    -> wave_name:string
    -> t
    -> int option

  (** Specialization of [look_for_nth_instance_of_condition_in_waveform] to find the first
      cycle where a wave is [vdd]. *)
  val look_for_wave_first_vdd_cycle
    :  how_to_find:Wave_condition.How_to_find.t
    -> t
    -> int option
end

module type Waveform = sig
  module type S = S

  type nonrec 'a with_options = 'a with_options

  val sort_ports_and_formats
    :  'wave_data Wave_data.Wave.t array
    -> Display_rules.t option
    -> 'wave_data Wave.t array

  (** Create a waveform printer for the given [Data] *)
  module Make (Data : Hardcaml.Wave_data.S) :
    S with type t = Data.t Hardcaml.Wave_data.Wave.t array

  (** Printer for Cycle based waveforms *)
  module By_cycle :
    S with type t = Hardcaml.Wave_data_in_cycles.t Hardcaml.Wave_data.Wave.t array

  (** Printer for Event based waveforms *)
  module By_event :
    S with type t = Hardcaml.Wave_data_in_events.Bits.t Hardcaml.Wave_data.Wave.t array

  (** Generic waveform printer *)
  include S with type t = Hardcaml.Wave_data.t
end
