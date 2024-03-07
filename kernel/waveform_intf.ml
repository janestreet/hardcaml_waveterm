(** A waveform is a mutable value that represents the outcome of a simulation; it holds a
    time series of values for a number of ports.  The waveform is updated by running the
    simulation. *)

open Base
open! Hardcaml

module type S = sig
  module Data : Data.S
  module Wave : Wave.M(Data).S
  module Waves : Waves.M(Data)(Wave).S
  module Render : Render.M(Data)(Wave)(Waves).S

  type t [@@deriving sexp_of, equal]

  val create_from_data : waves:Wave.t list -> ports:Port.t list -> t
  val waves : t -> Wave.t array
  val update_waves : t -> Wave.t array -> t

  (** Combine two waveforms into one waveform *)
  val combine : t -> t -> t

  (** Waveform configuration options.

      [display_rules] see [Display_rules].  A waveform may be constructed once and
      displayed multiple times with differing options.

      [wave_height] sets the number of rows each wave is displayed with.

      - [<0] an exception is raised
      - [0] 2 lines per wave.  This show transitions but not the data for multi-bit signals.
      - [1] 2 lines for binary data, 3 lines for multi-bit data.  This is the default and
        most compact view.
      - [n] (n+1) lines per wave.

      [wave_width] sets the number of chars used to render each clock cycle.

      - [n>=0] ((n+1)*2) characters per cycle.  This ensures that the clock is properly
        rendered when [n=0].  The default of [3] allows up to 7 chars per cycle to be
        rendered to represent data values in the waveform.

      - [n<0] (-n) cycles per character.  Characters in the waveform within which multiple
        transitions occur are displayed with a double veritical bar.

      [display_width] and [display_height] simply set the overall size of the displayed
      waveform.  An auto scaling routine assigns approximately [1/3] of the display for the
      signals and values windows.  The default size is 70 character wide and 20 high.  The
      minimum size is 7 characters wide and 3 high and an exception is raise otherwise.

      [signals_alignment] changes the text alignment of the displayed signals names. By
      default this is Wave_format.Left. *)
  type 'a with_options =
    ?display_rules:Display_rules.t
    -> ?display_width:int
    -> ?display_height:int
    -> ?display_values:bool
    -> ?wave_width:int
    -> ?wave_height:int
    -> ?signals_width:int
    -> ?start_cycle:int
    -> ?signals_alignment:Text_alignment.t
    -> 'a

  val sort_ports_and_formats : t -> Display_rules.t option -> Wave.t array

  (** Write waveform into a [Buffer.t]. *)
  val to_buffer : (t -> Buffer.t) with_options

  (** Convert waveform to a string. *)
  val to_string : (t -> string) with_options

  (** Print waveform to [channel] *)
  val print
    : (?channel:Stdio.Out_channel.t (** default is [stdout] *) -> t -> unit) with_options
end

module M
  (Data : Data.S)
  (Wave : Wave.M(Data).S)
  (Waves : Waves.M(Data)(Wave).S)
  (Render : Render.M(Data)(Wave)(Waves).S) =
struct
  module type S =
    S
      with module Data := Data
       and module Wave := Wave
       and module Waves := Waves
       and module Render := Render
end

module type Waveform = sig
  module type S = S

  module M = M

  module Make
    (Data : Data.S)
    (Wave : Wave.M(Data).S)
    (Waves : Waves.M(Data)(Wave).S)
    (Render : Render.M(Data)(Wave)(Waves).S) : M(Data)(Wave)(Waves)(Render).S
end
