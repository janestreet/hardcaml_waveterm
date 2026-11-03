open Base
open Hardcaml

module type S = sig
  module Data : Hardcaml.Wave_data.S

  (** Functions for drawing waves, signal names and values *)

  (** Maximum width of the signal-name window, in characters. *)
  val get_max_signal_width_in_chars : Data.t Waves.t -> int

  (** Estimate of the maximum width of the values window, in characters. Inaccurate for
      the constructors [U], [S], and [F]. *)
  val get_estimated_max_value_width : Data.t Waves.t -> int

  (** Maximum number of waveform time steps. *)
  val total_time_steps_in_waveform : Data.t Waves.t -> int

  (** Number of signals in the waveform. *)
  val total_signals_in_waveform : Data.t Waves.t -> int

  (** Turn the UI integer representation of [wave_scale] into a rendering scale. *)
  val wave_scale_of_code
    :  int
    -> [ `Time_steps_per_char of int | `Chars_per_time_step of int ]

  (** draw binary waveform data *)
  val draw_binary_data
    :  ctx:Draw.ctx
    -> style:Style.t
    -> bounds:Rect.t
    -> wave_scale:[ `Time_steps_per_char of int | `Chars_per_time_step of int ]
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
    -> wave_scale:[ `Time_steps_per_char of int | `Chars_per_time_step of int ]
    -> data:Data.t
    -> off:int
    -> unit

  (** Like [draw_data], for data whose values are 4-state (see [Four_state_value]). *)
  val draw_four_state_data
    :  ctx:Draw.ctx
    -> style:Style.t
    -> bounds:Rect.t
    -> to_str:(Four_state_logic.t -> string)
    -> alignment:Text_alignment.t
    -> wave_scale:[ `Time_steps_per_char of int | `Chars_per_time_step of int ]
    -> data:Data.t
    -> off:int
    -> unit

  type 'a draw_item =
    ?style:Style.t -> ctx:Draw.ctx -> bounds:Rect.t -> Data.t Waves.t -> 'a

  val with_border : draw:'a draw_item -> label:string -> ?border:Style.t -> 'a draw_item

  (** draw cursor *)
  val draw_cursor
    :  ctx:Draw.ctx
    -> bounds:Rect.t
    -> wave_cursor:int
    -> primary:bool
    -> state:Data.t Waves.t
    -> unit

  (** draw waveforms *)
  val draw_wave
    :  ?style:Style.t
    -> ?wave_cursors:[ `Primary of int | `Secondary of int ] list
    -> selected_wave_index:int option
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Data.t Waves.t
    -> unit

  (** draw signal names *)
  val draw_signals
    :  ?alignment:Text_alignment.t
    -> ?style:Style.t
    -> selected_wave_index:int option
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Data.t Waves.t
    -> unit

  (** draw signal values *)
  val draw_values
    :  ?style:Style.t
    -> ?wave_cursor:int
    -> selected_wave_index:int option
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Data.t Waves.t
    -> int

  val draw_status
    :  ?style:Style.t
    -> ?wave_cursor:int
    -> ctx:Draw.ctx
    -> bounds:Rect.t
    -> Data.t Waves.t
    -> unit

  (** draw standard user inferface (names, values, waveforms left to right *)
  val draw_ui
    :  ?signals_alignment:Text_alignment.t
    -> ?style:Window_styles.t
    -> ?bounds:Window_bounds.t
    -> ctx:Draw.ctx
    -> Data.t Waves.t
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
        { time : int
        ; signal_index : int option
        }
    | Value of { signal_index : int option }
    | Signal of { signal_index : int option }
    | Status
    | No_pick

  val pick : bounds:Window_bounds.t -> r:int -> c:int -> Data.t Waves.t -> pick

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
      -> Data.t Waves.t
      -> Draw.ctx

    val draw_full
      :  ?signals_alignment:Text_alignment.t
      -> ?style:Window_styles.t
      -> Data.t Waves.t
      -> Draw.ctx * Draw.ctx * Draw.ctx
  end

  module To_render : sig
    (** Values are held as raw [Four_state_value] planes; on 2-state data the masks are
        all-zeros. *)
    type 'a t =
      { at_most_one_transition : bool
      ; first : 'a
      ; first_mask : 'a
      ; last : 'a
      ; last_mask : 'a
      }
  end

  module For_testing : sig
    val get_transition_data
      :  prev_data:(Bits.t * Bits.t) option
      -> data:Data.t
      -> time_steps_per_char:int
      -> off:int
      -> Bits.t To_render.t
  end
end

module M (Data : Hardcaml.Wave_data.S) = struct
  module type S = S with module Data := Data
end

module type Render = sig
  module type S = S

  module M = M
  module Make (Data : Hardcaml.Wave_data.S) : M(Data).S
end
