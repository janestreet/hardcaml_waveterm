open Base
open Hardcaml

module type S = sig
  module Data : Data.S
  module Wave : Wave.M(Data).S
  module Waves : Waves.M(Data)(Wave).S

  (** Functions for drawing waves, signal names and values *)

  (** max width of name window *)
  val get_max_signal_width_in_chars : Waves.t -> int

  (** gets an estimate fo the max with of values. Inaccruate for the constructors [U], [S]
      and [F]. *)
  val get_estimated_max_value_width : Waves.t -> int

  (** max no of wave cycles *)
  val total_cycles_in_waveform : Waves.t -> int

  (** max no of wave signals *)
  val total_signals_in_waveform : Waves.t -> int

  (** Turn the UI integer representation of wave_width into a the rendering form. *)
  val wave_width_of_code : int -> [ `Cycles_per_char of int | `Chars_per_cycle of int ]

  (** draws [cnt] clock cycles *)
  val draw_clock_cycles
    :  ctx:Draw.ctx
    -> style:Style.t
    -> bounds:Rect.t
    -> wave_width:[ `Cycles_per_char of int | `Chars_per_cycle of int ]
    -> cnt:int
    -> unit

  (** draw binary waveform data *)
  val draw_binary_data
    :  ctx:Draw.ctx
    -> style:Style.t
    -> bounds:Rect.t
    -> wave_width:[ `Cycles_per_char of int | `Chars_per_cycle of int ]
    -> data:Data.t
    -> off:int
    -> unit

  (** draw arbitrary waveform data *)
  val draw_data
    :  ctx:Draw.ctx
    -> style:Style.t
    -> bounds:Rect.t
    -> to_str:(Bits.t -> string)
    -> alignment:Text_alignment.t
    -> wave_width:[ `Cycles_per_char of int | `Chars_per_cycle of int ]
    -> data:Data.t
    -> off:int
    -> unit

  type 'a draw_item = ?style:Style.t -> ctx:Draw.ctx -> bounds:Rect.t -> Waves.t -> 'a

  val with_border : draw:'a draw_item -> label:string -> ?border:Style.t -> 'a draw_item

  (** draw cursor *)
  val draw_cursor
    :  ctx:Draw.ctx
    -> bounds:Rect.t
    -> wave_cursor:int
    -> primary:bool
    -> state:Waves.t
    -> unit

  (** draw waveforms *)
  val draw_wave
    :  ?style:Style.t
    -> ?wave_cursors:[ `Primary of int | `Secondary of int ] list
    -> selected_wave_index:int option
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Waves.t
    -> unit

  (** draw signal names *)
  val draw_signals
    :  ?alignment:Text_alignment.t
    -> ?style:Style.t
    -> selected_wave_index:int option
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Waves.t
    -> unit

  (** draw signal values *)
  val draw_values
    :  ?style:Style.t
    -> ?wave_cursor:int
    -> selected_wave_index:int option
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Waves.t
    -> int

  val draw_status
    :  ?style:Style.t
    -> ?wave_cursor:int
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Waves.t
    -> unit

  (** draw standard user inferface (names, values, waveforms left to right *)
  val draw_ui
    :  ?signals_alignment:Text_alignment.t
    -> ?style:Window_styles.t
    -> ?bounds:Window_bounds.t
    -> ctx:Draw.ctx
    -> Waves.t
    -> unit

  val draw_help
    :  ?style:Style.t
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Key_help.t list
    -> offset:int
    -> unit

  type pick =
    | Wave of
        { cycle : int
        ; signal_index : int option
        }
    | Value of { signal_index : int option }
    | Signal of { signal_index : int option }
    | Status
    | No_pick

  val pick : bounds:Window_bounds.t -> r:int -> c:int -> Waves.t -> pick

  module Static : sig
    val draw
      :  ?signals_alignment:Text_alignment.t
      -> ?signals:bool
      -> ?values:bool
      -> ?waves:bool
      -> ?style:Window_styles.t
      -> ?rows:int
      -> ?cols:int
      -> ?signals_width:int
      -> Waves.t
      -> Draw.ctx

    val draw_full
      :  ?signals_alignment:Text_alignment.t
      -> ?style:Window_styles.t
      -> Waves.t
      -> Draw.ctx * Draw.ctx * Draw.ctx
  end
end

module M (Data : Data.S) (Wave : Wave.M(Data).S) (Waves : Waves.M(Data)(Wave).S) = struct
  module type S =
    S with module Data := Data and module Wave := Wave and module Waves := Waves
end

module type Render = sig
  module type S = S

  module M = M

  module Make (Data : Data.S) (Wave : Wave.M(Data).S) (Waves : Waves.M(Data)(Wave).S) :
    M(Data)(Wave)(Waves).S
end
